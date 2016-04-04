(in-package :cepl.pipelines)
(in-readtable fn:fn-reader)

;;;--------------------------------------------------------------
;;; ARG ASSIGNERS ;;;
;;;---------------;;;

(defun stages->uniform-assigners (stage-names)
  (mapcar λ(make-arg-assigners _) (aggregate-uniforms stage-names nil t)))

(defun make-arg-assigners (uniform-arg)
  (varjo:with-v-arg (arg-name varjo-type~1 qualifiers glsl-name) uniform-arg
    (let* ((local-arg-name 'val)
           (glsl-name (or glsl-name (varjo:safe-glsl-name-string arg-name)))
           (assigner (dispatch-make-assigner local-arg-name varjo-type~1
                                             glsl-name qualifiers)))
      `(,(let-forms assigner)
         (when ,arg-name
           (let ((,local-arg-name ,(if (pointer-arg assigner)
                                       `(pointer ,arg-name)
                                       arg-name)))
             ,@(uploaders assigner)))))))

(defun dispatch-make-assigner (arg-name type glsl-name qualifiers)
  (assert (not (null glsl-name)))
  (let* ((varjo-type (varjo:type-spec->type type))
         (struct-arg (varjo:v-typep varjo-type 'varjo:v-user-struct))
         (array-length (when (v-typep varjo-type 'v-array)
                         (apply #'* (v-dimensions varjo-type))))
         (sampler (cepl.samplers::sampler-typep varjo-type))
         (ubo (member :ubo qualifiers)))
    (cond
      (ubo (make-ubo-assigner arg-name varjo-type glsl-name))
      ;;
      (array-length (make-array-assigners arg-name varjo-type glsl-name))
      ;;
      (struct-arg (make-struct-assigners arg-name varjo-type glsl-name))
      ;;
      (sampler (make-sampler-assigner arg-name varjo-type glsl-name))
      ;;
      (t (make-simple-assigner arg-name varjo-type glsl-name nil)))))

;; {TODO} Why does this not have a byte-offset? Very tired cant work it out :)
(defun make-sampler-assigner (arg-name type glsl-name-path)
  (let ((id-name (gensym))
        (i-unit (gensym "IMAGE-UNIT")))
    (make-assigner
     :let-forms `((,id-name (gl:get-uniform-location prog-id ,glsl-name-path))
                  (,i-unit (incf image-unit)))
     :uploaders `((when (>= ,id-name 0)
		    (unless (eq (%sampler-type ,arg-name)
				,(cepl.types::type->spec type))
		      (error "incorrect type of sampler passed to shader"))
		    (cepl.textures::active-texture-num ,i-unit)
		    (cepl.textures::bind-texture (%sampler-texture ,arg-name))
		    (if cepl.samplers::*samplers-available*
			(gl:bind-sampler ,i-unit (%sampler-id ,arg-name))
			(cepl.textures::fallback-sampler-set ,arg-name))
		    (uniform-sampler ,id-name ,i-unit))))))

(defun make-ubo-assigner (arg-name varjo-type glsl-name)
  (let ((id-name (gensym))
        (type-spec (varjo:type->type-spec varjo-type)))
    (make-assigner
     :let-forms
     `((,id-name (get-uniform-block-index
                  prog-id ,(format nil "_UBO_~a" glsl-name))))
     :uploaders
     `((when (>= ,id-name 0)
	 (if (and (typep ,arg-name 'ubo)
		  (v-type-eq (varjo:type-spec->type ',type-spec)
			     (ubo-data-type ,arg-name)))
	     (%gl:uniform-block-binding prog-id ,id-name (ubo-id ,arg-name))
	     (error "Invalid type for ubo argument:~%Required:~a~%Recieved:~a~%"
		    ',type-spec (ubo-data-type ,arg-name))))))))

(defun get-uniform-block-index (program name)
  (with-foreign-string (s name)
    (%gl:get-uniform-block-index program s)))

;; NOTE: byte-offset can be nil, this is a legit value
(defun make-simple-assigner (arg-name type glsl-name-path
                             &optional (byte-offset 0))
  (let ((id-name (gensym)))
    (make-assigner
     :let-forms `((,id-name (gl:get-uniform-location prog-id ,glsl-name-path)))
     :uploaders
     `((when (>= ,id-name 0)
	 ;; If we have a byte offset then we need index into that block of
	 ;; foreign data to upload (think structs) so we need to use
	 ;; #'get-foreign-uniform-function-name.
         ,(if byte-offset
              `(,(get-foreign-uniform-function-name (cepl.types::type->spec type))
                 ,id-name 1 (cffi:inc-pointer ,arg-name ,byte-offset))
              `(,(get-uniform-function-name (cepl.types::type->spec type)) ,id-name ,arg-name)))))))

(defun make-array-assigners (arg-name type glsl-name-path &optional (byte-offset 0))
  (let ((element-type (varjo:v-element-type type))
        (array-length (apply #'* (v-dimensions type))))
    (merge-into-assigner
     t
     (loop :for i :below array-length
        :if (varjo:v-typep element-type 'varjo:v-user-struct) :append
        (make-struct-assigners arg-name element-type
                               (format nil "~a[~a]" glsl-name-path i)
                               byte-offset)
        :else :collect
        (make-simple-assigner arg-name element-type
                              (format nil "~a[~a]" glsl-name-path i)
                              byte-offset)
        :do (incf byte-offset (gl-type-size (cepl.types::type->spec element-type)))))))


(defun make-struct-assigners (arg-name type glsl-name-path
                              &optional (byte-offset 0))
  (merge-into-assigner
   t
   (loop
      :for (l-slot-name v-slot-type) :in (varjo:v-slots type)
      :for (pslot-type array-length . rest) := (listify v-slot-type)
      :do (just-ignore rest)
      :append
      (let* ((pslot-type (type-spec->type pslot-type))
             (glsl-name (varjo:safe-glsl-name-string l-slot-name))
             (glsl-name-path (format nil "~a.~a" glsl-name-path glsl-name)))
        (cond
          ;;
          (array-length (make-array-assigners arg-name v-slot-type
                                              glsl-name-path byte-offset))
          ;;
          ((varjo:v-typep pslot-type 'v-user-struct)
           (make-struct-assigners arg-name pslot-type glsl-name-path
                                  byte-offset))
          ;;
          (t (list (make-simple-assigner arg-name pslot-type glsl-name-path
                                         byte-offset)))))
      :do (when byte-offset
	    (incf byte-offset (* (gl-type-size pslot-type)
				 (or array-length 1)))))))


(defclass assigner ()
  ((let-forms :initform nil :initarg :let-forms :accessor let-forms)
   (uploaders :initform nil :initarg :uploaders :accessor uploaders)
   (pointer-arg :initform nil :initarg :pointer-arg :accessor pointer-arg)))

(defun make-assigner (&key let-forms uploaders pointer-arg)
  (make-instance 'assigner :let-forms let-forms :uploaders uploaders
                 :pointer-arg pointer-arg))

(defun merge-into-assigner (pointer-arg assingers)
  (make-assigner :let-forms (mapcat #'let-forms assingers)
                 :uploaders (mapcat #'uploaders assingers)
                 :pointer-arg pointer-arg))

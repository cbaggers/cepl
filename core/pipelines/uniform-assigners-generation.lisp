(in-package :cepl.pipelines)
(in-readtable fn:fn-reader)

;;;--------------------------------------------------------------

(defclass assigner ()
  ((let-forms :initform nil :initarg :let-forms :accessor let-forms)
   (uploaders :initform nil :initarg :uploaders :accessor uploaders)
   (pointer-arg :initform nil :initarg :pointer-arg :accessor pointer-arg)
   (arg-name :initform nil :initarg :arg-name :accessor arg-name)
   (local-arg-name  :initform nil :initarg :local-arg-name
                    :accessor local-arg-name)
   (cleanup :initform nil :initarg :cleanup :accessor cleanup)))

(defclass assigner-let-form ()
  ((index :initarg :index :accessor assigner-index)
   (type :initarg :type :accessor assigner-type)
   (name :initarg :name :accessor assigner-name)
   (body :initarg :body :accessor assigner-body)))

;;;--------------------------------------------------------------
;;; ARG ASSIGNERS ;;;
;;;---------------;;;

(declaim (type (signed-byte 32) +unknown-uniform-int-id+)
         (type (unsigned-byte 32) +unknown-uniform-uint-id+))

(defconstant +unknown-uniform-int-id+
  (- (/ (expt 2 32) 2) 1))

(defconstant +unknown-uniform-uint-id+
  (- (expt 2 32) 1))

(defstruct uidx
  (int -1)
  (uint -1))

(defun+ make-arg-assigners (uniform-args)
  (let ((indexes (make-uidx)))
    (labels ((inner (uniform-arg)
               (varjo.internals:with-v-arg (arg-name varjo-type~1 qualifiers glsl-name)
                   uniform-arg
                 (let* ((local-arg-name 'val)
                        (glsl-name (or glsl-name (varjo.internals:safe-glsl-name-string
                                                  arg-name))))
                   (dispatch-make-assigner indexes
                                           local-arg-name arg-name varjo-type~1
                                           glsl-name qualifiers)))))
      (mapcar #'inner uniform-args))))

(defmethod gen-uploaders-block ((assigner assigner))
  (with-slots (arg-name local-arg-name) assigner
    `(when ,arg-name
       (let ((,local-arg-name ,(if (pointer-arg assigner)
                                   `(pointer ,arg-name)
                                   arg-name)))
         ,@(uploaders assigner)))))

(defmethod unpack-arrayd-assigner ((assigner assigner))
  (with-slots (let-forms arg-name local-arg-name) assigner
    (loop :for let-form :in let-forms :collect
       (with-slots (name index type) let-form
         (let ((arr (if (equal type '(signed-byte 32))
                        `(pipeline-state-uniform-int-ids state)
                        `(pipeline-state-uniform-uint-ids state))))
           `(,name (aref ,arr ,index)))))))

(defmethod gen-cleanup-block ((assigner assigner))
  (with-slots (arg-name local-arg-name) assigner
    (when (cleanup assigner)
      `(when ,arg-name
         (let ((,local-arg-name ,(if (pointer-arg assigner)
                                     `(pointer ,arg-name)
                                     arg-name)))
           (declare (ignorable ,local-arg-name))
           ,@(cleanup assigner))))))

(defun+ dispatch-make-assigner (indexes
                                local-arg-name arg-name type glsl-name
                                qualifiers)
  (assert (not (null glsl-name)))
  (let* ((varjo-type (varjo:type-spec->type type))
         (struct-arg (varjo:v-typep varjo-type 'varjo:v-user-struct))
         (array-length (when (v-typep varjo-type 'v-array)
                         (apply #'* (v-dimensions varjo-type))))
         (sampler (cepl.samplers::sampler-typep varjo-type))
         (ubo (member :ubo qualifiers))
         (assigner
          (cond
            (ubo (make-ubo-assigner indexes local-arg-name varjo-type glsl-name))
            ;;
            (array-length (make-array-assigners indexes local-arg-name varjo-type glsl-name))
            ;;
            (struct-arg (make-struct-assigners indexes local-arg-name varjo-type glsl-name))
            ;;
            (sampler (make-sampler-assigner indexes local-arg-name varjo-type glsl-name))
            ;;
            (t (make-simple-assigner indexes local-arg-name varjo-type glsl-name nil)))))
    (setf (arg-name assigner) arg-name
          (local-arg-name assigner) local-arg-name)
    assigner))

;; {TODO} Why does this not have a byte-offset? Very tired cant work it out :)
(defun+ make-sampler-assigner (indexes arg-name type glsl-name-path)
  (let ((id-name (gensym))
        (i-unit (gensym "IMAGE-UNIT")))
    (make-assigner
     :let-forms
     (list (make-assigner-let
            :name i-unit
            :index (incf (uidx-int indexes))
            :type '(signed-byte 32)
            :body `(incf image-unit))
           (make-assigner-let
            :name id-name
            :index (incf (uidx-int indexes))
            :type '(signed-byte 32)
            :body `(let ((name (the (signed-byte 32)
                                    (gl:get-uniform-location
                                     prog-id ,glsl-name-path))))
                     (uniform-sampler name image-unit)
                     name)))
     :uploaders
     `((when (and (< ,id-name +unknown-uniform-int-id+)
                  (>= ,id-name 0)
                  (< ,i-unit +unknown-uniform-int-id+)
                  (>= ,i-unit 0))
         (unless (eq (%sampler-type ,arg-name)
                     ,(cepl.types::type->spec type))
           (error "incorrect type of sampler passed to shader"))
         (cepl.context::set-sampler-bound
          ,*pipeline-body-context-var* ,arg-name ,i-unit))))))

(defun+ make-ubo-assigner (indexes arg-name varjo-type glsl-name)
  (let ((id-name (gensym))
        (type-spec (varjo:type->type-spec varjo-type)))
    (make-assigner
     :let-forms
     (list (make-assigner-let
            :name id-name
            :type '(unsigned-byte 32)
            :index (incf (uidx-uint indexes))
            :body `(get-uniform-block-index
                    prog-id ,(format nil "_UBO_~a" glsl-name))))
     :uploaders
     `((when (and (< ,id-name +unknown-uniform-uint-id+)
                  (>= ,id-name 0))
         (if (and (typep ,arg-name 'ubo)
                  (v-type-eq (varjo:type-spec->type ',type-spec)
                             (ubo-data-type ,arg-name)))
             (%gl:uniform-block-binding prog-id ,id-name (ubo-id ,arg-name))
             (error "Invalid type for ubo argument:~%Required:~a~%Recieved:~a~%"
                    ',type-spec (ubo-data-type ,arg-name))))))))

(defun+ get-uniform-block-index (program name)
  (with-foreign-string (s name)
    (%gl:get-uniform-block-index program s)))

;; NOTE: byte-offset can be nil, this is a legit value
(defun+ make-simple-assigner (indexes arg-name type glsl-name-path
                             &optional (byte-offset 0))
  (let ((id-name (gensym)))
    (make-assigner
     :let-forms
     (list (make-assigner-let
            :name id-name
            :type '(signed-byte 32)
            :index (incf (uidx-int indexes))
            :body `(the (signed-byte 32)
                        (gl:get-uniform-location prog-id ,glsl-name-path))))
     :uploaders
     `((when (and (< ,id-name +unknown-uniform-int-id+)
                  (>= ,id-name 0))
         ;; If we have a byte offset then we need index into that block of
         ;; foreign data to upload (think structs) so we need to use
         ;; #'get-foreign-uniform-function-name.
         ,(if byte-offset
              `(,(get-foreign-uniform-function-name (cepl.types::type->spec type))
                 ,id-name 1 (cffi:inc-pointer ,arg-name ,byte-offset))
              `(,(get-uniform-function-name (cepl.types::type->spec type)) ,id-name ,arg-name)))))))

(defun+ make-array-assigners (indexes arg-name type glsl-name-path &optional (byte-offset 0))
  (let ((element-type (varjo:v-element-type type))
        (array-length (apply #'* (v-dimensions type))))
    (merge-into-assigner
     t
     (loop :for i :below array-length
        :if (varjo:v-typep element-type 'varjo:v-user-struct) :append
        (make-struct-assigners indexes arg-name element-type
                               (format nil "~a[~a]" glsl-name-path i)
                               byte-offset)
        :else :collect
        (make-simple-assigner indexes arg-name element-type
                              (format nil "~a[~a]" glsl-name-path i)
                              byte-offset)
        :do (incf byte-offset (gl-type-size (cepl.types::type->spec element-type)))))))


(defun+ make-struct-assigners (indexes arg-name type glsl-name-path
                              &optional (byte-offset 0))
  (merge-into-assigner
   t
   (loop
      :for (l-slot-name v-slot-type) :in (varjo.internals:v-slots type)
      :for (pslot-type array-length . rest) := (listify v-slot-type)
      :do (just-ignore rest)
      :append
      (let* ((pslot-type (type-spec->type pslot-type))
             (glsl-name (varjo.internals:safe-glsl-name-string l-slot-name))
             (glsl-name-path (format nil "~a.~a" glsl-name-path glsl-name)))
        (cond
          ;;
          (array-length (make-array-assigners indexes arg-name v-slot-type
                                              glsl-name-path byte-offset))
          ;;
          ((varjo:v-typep pslot-type 'v-user-struct)
           (make-struct-assigners indexes arg-name pslot-type glsl-name-path
                                  byte-offset))
          ;;
          (t (list (make-simple-assigner indexes arg-name pslot-type glsl-name-path
                                         byte-offset)))))
      :do (when byte-offset
            (incf byte-offset (* (gl-type-size pslot-type)
                                 (or array-length 1)))))))


(defun+ make-assigner (&key let-forms uploaders pointer-arg
                        arg-name local-arg-name cleanup)
  (make-instance 'assigner :let-forms let-forms :uploaders uploaders
                 :pointer-arg pointer-arg :arg-name arg-name
                 :local-arg-name local-arg-name
                 :cleanup cleanup))

(defun+ merge-into-assigner (pointer-arg assingers)
  (make-assigner :let-forms (mapcat #'let-forms assingers)
                 :uploaders (mapcat #'uploaders assingers)
                 :pointer-arg pointer-arg
                 :cleanup (mapcat #'cleanup assingers)))

(defun+ make-assigner-let (&key name index type body)
  (make-instance 'assigner-let-form
                 :name name
                 :index index
                 :type type
                 :body body))

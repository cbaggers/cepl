(in-package :cepl.vaos)

;; [TODO] The terminology in here seems inconsistant, need to
;; nail this down

;;--------------------------------------------------------------
;; VAOS ;;
;;------;;

(defun2 free-vao (vao)
  (with-foreign-object (id :uint)
    (setf (mem-ref id :uint) vao)
    (%gl:delete-vertex-arrays 1 id)))

;; [TODO] would a unboxed lisp array be faster?
(defun2 free-vaos (vaos)
  (with-foreign-object (ids :uint (length vaos))
    (loop :for vao :in vaos :for i :from 0 :do
       (setf (mem-aref ids :uint i) vao))
    (%gl:delete-vertex-arrays (length vaos) ids)))

(defmacro with-vao-bound (vao &body body)
  (alexandria:with-gensyms (ctx vao-id old-vao)
    `(let* ((,ctx (cepl-context))
            (,old-vao (vao-bound ,ctx))
            (,vao-id ,vao))
       (unwind-protect
            (progn (setf (vao-bound ,ctx) ,vao-id)
                   ,@body)
         (setf (vao-bound (cepl-context)) ,old-vao)))))

(defun2 suitable-array-for-index-p (array)
  (and (eql (length (gpu-buffer-arrays (gpu-array-buffer array))) 1)
       (1d-p array)
       (find (element-type array) '(:uint8 :ushort :uint :unsigned-short
                                    :unsigned-int))))

(defn make-vao ((gpu-arrays list) &optional (index-array gpu-array-bb))
    gl-id
  (let ((gpu-arrays (listify gpu-arrays)))
    (the gl-id
         (make-vao-from-id
          (progn (assert (and (every #'1d-p gpu-arrays)
                              (or (null index-array)
                                  (suitable-array-for-index-p
                                   index-array))))
                 (gl:gen-vertex-array))
          gpu-arrays index-array))))

(defgeneric make-vao-from-id (gl-object gpu-arrays &optional index-array))

(defmethod make-vao-from-id (gl-object (gpu-arrays list) &optional index-array)
  "makes a vao using a list of gpu-arrays as the source data
   (remember that you can also use gpu-sub-array here if you
   need a subsection of a gpu-array).
   You can also specify an index-array which will be used as
   the indicies when rendering"
  (unless (and (every #'1d-p gpu-arrays)
               (or (null index-array) (suitable-array-for-index-p
                                       index-array)))
    (error "You can only make VAOs from 1D arrays"))
  (with-buffer (xx nil :array-buffer)
    (with-buffer (yy nil :element-array-buffer)
      (let ((element-buffer (when index-array (gpu-array-buffer index-array)))
            (vao gl-object)
            (attr 0)
            (ctx (cepl-context)))
        (setf (vao-bound ctx) vao)
        (loop :for gpu-array :in gpu-arrays :do
           (let* ((buffer (gpu-array-buffer gpu-array))
                  (elem-type (gpu-array-bb-element-type gpu-array))
                  (offset (gpu-array-bb-offset-in-bytes-into-buffer gpu-array)))
             (with-buffer (foo buffer :array-buffer)
               (incf attr (gl-assign-attrib-pointers
                           (if (listp elem-type) (second elem-type) elem-type)
                           attr offset)))))
        (when element-buffer
          (setf (gpu-buffer-bound (cepl-context) :element-array-buffer)
                element-buffer))
        (setf (vao-bound ctx) 0)
        vao))))

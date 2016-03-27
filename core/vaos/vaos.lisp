(in-package :cepl.vaos)

;; [TODO] The terminology in here seems inconsistant, need to
;; nail this down

;;--------------------------------------------------------------
;; VAOS ;;
;;------;;

(defun free-vao (vao)
  (with-foreign-object (id :uint)
    (setf (mem-ref id :uint) vao)
    (%gl:delete-vertex-arrays 1 id)))

;; [TODO] would a unboxed lisp array be faster?
(defun free-vaos (vaos)
  (with-foreign-object (id :uint (length vaos))
    (loop :for vao :in vaos :for i :from 0 :do
       (setf (mem-aref id :uint i) vao))
    (%gl:delete-vertex-arrays 1 id)))

;; [TODO] Vao changes the inhabitants of :vertex-array etc
;;        this should be undone
(defun bind-vao (vao)
  (gl:bind-vertex-array vao))

(defun unbind-vao ()
  (gl:bind-vertex-array 0))

(defmacro with-vao-bound (vao &body body)
  `(unwind-protect
	(progn (bind-vao ,vao)
	       ,@body)
     (unbind-vao)))


;; [TODO] Types need full support. Read glVertexAttribPointer and work out
;;        what to do with gl_half_float, gl_double, gl_fixed, gl_int_2_10_10_10_rev
;;        & gl_unsigned_int_2_10_10_10_rev
;; [TODO] Read about glVertexAttribLPointer

;; [TODO] Use suitable-array-for-index-p
;; [TODO] Sanity check dimensions of buffer contents?
(defun make-vao-from-formats (formats &key element-buffer)
  "Makes a vao from a list of buffer formats.
   The formats list should be laid out as follows:
   `((buffer1 (attr-format1) (attr-format2))
     (buffer2 (attr-format3) (attr-format4)))
   with each attr-format laid out as follows:
   `(component-type normalized-flag stride pointer)
   if you have the type and offset of the data this can be generated
   by using the function gl-type-format.
   You can also specify an element buffer to be used in the vao"
  (let ((vao (gl:gen-vertex-array))
        (attr-num 0))
    (bind-vao vao)
    (loop for format in formats
       :do (let ((buffer (first format)))
             (cepl.gpu-buffers::force-bind-buffer buffer :array-buffer)
             (loop :for (type normalized stride pointer)
                :in (rest format)
		:do (just-ignore normalized)
                :do (setf attr-num
                          (+ attr-num
                             (gl-assign-attrib-pointers
                              type pointer stride))))))
    (when element-buffer
      (cepl.gpu-buffers::force-bind-buffer element-buffer :element-array-buffer))
    (bind-vao 0)
    vao))

(defun suitable-array-for-index-p (array)
  (and (eql (length (gpu-buffer-format (gpu-array-buffer array))) 1)
       (1d-p array)
       (find (element-type array) '(:uint8 :ushort :uint :unsigned-short
                                    :unsigned-int))))

(defun make-vao (gpu-arrays &optional index-array)
  (let ((gpu-arrays (listify gpu-arrays)))
    (make-vao-from-id
     (progn (assert (and (every #'1d-p gpu-arrays)
                         (or (null index-array)
                             (suitable-array-for-index-p
                              index-array))))
            (gl:gen-vertex-array))
     gpu-arrays index-array)))

(defmethod make-vao-from-id (gl-object (gpu-arrays list) &optional index-array)
  "makes a vao using a list of gpu-arrays as the source data
   (remember that you can also use gpu-sub-array here if you
   need a subsection of a gpu-array).
   You can also specify an index-array which will be used as
   the indicies when rendering"
  (unless (and (every #'1d-p gpu-arrays)
               (or (null index-array) (suitable-array-for-index-p
                                       index-array))))
  (let ((element-buffer (when index-array (gpu-array-buffer index-array)))
        (vao gl-object)
        (attr 0))
    (bind-vao vao)
    (loop :for gpu-array :in gpu-arrays :do
       (let* ((buffer (gpu-array-buffer gpu-array))
              (format (gpu-array-format gpu-array)))
         (cepl.gpu-buffers::force-bind-buffer buffer :array-buffer)
         (setf attr (+ attr (gl-assign-attrib-pointers
                             (let ((type (first format)))
                               (if (listp type) (second type) type))
                             attr
                             (+ (third format)
                                (cepl.c-arrays::gl-calc-byte-size
				 (first format)
				 (list (gpu-array-bb-start gpu-array)))))))))
    (when element-buffer
      (cepl.gpu-buffers::force-bind-buffer element-buffer :element-array-buffer))
    (bind-vao 0)
    vao))

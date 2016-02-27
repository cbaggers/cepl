(in-package :jungl)

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
(let ((vao-cache nil))
  (defun bind-vao (vao)
    (unless (eq vao vao-cache)
      (gl:bind-vertex-array vao)
      (setf vao-cache vao)))
  (defun force-bind-vao (vao)
    (gl:bind-vertex-array vao)
    (setf vao-cache vao)))
(setf (documentation 'bind-vao 'function)
      "Binds the vao specfied")
(setf (symbol-function 'bind-vertex-array) #'bind-vao)

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
    (force-bind-vao vao)
    (loop for format in formats
       :do (let ((buffer (first format)))
             (force-bind-buffer buffer :array-buffer)
             (loop :for (type normalized stride pointer)
                :in (rest format)
                :do (setf attr-num
                          (+ attr-num
                             (gl-assign-attrib-pointers
                              type pointer stride))))))
    (when element-buffer
      (force-bind-buffer element-buffer :element-array-buffer))
    (bind-vao 0)
    vao))

(defun suitable-array-for-index-p (array)
  (and (eql (length (glbuffer-format (gpuarray-buffer array))) 1)
       (1d-p array)
       (find (element-type array) '(:ubyte :ushort :uint :unsigned-short
                                    :unsigned-byte :unsigned-int))))

(defun make-vao (gpu-arrays &optional index-array)
  "makes a vao using a list of gpu-arrays as the source data
   (remember that you can also use gpu-sub-array here if you
   need a subsection of a gpu-array).
   You can also specify an index-array which will be used as
   the indicies when rendering"
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
  (let ((element-buffer (when index-array (gpuarray-buffer index-array)))
        (vao gl-object)
        (attr 0))
    (force-bind-vao vao)
    (loop :for gpu-array :in gpu-arrays :do
       (let* ((buffer (gpuarray-buffer gpu-array))
              (format (gpuarray-format gpu-array)))
         (force-bind-buffer buffer :array-buffer)
         (setf attr (+ attr (gl-assign-attrib-pointers
                             (let ((type (first format)))
                               (if (listp type) (second type) type))
                             attr
                             (+ (third format)
                                (gl-calc-byte-size (first format)
                                                   (list (gpuarray-start
                                                          gpu-array)))))))))
    (when element-buffer
      (force-bind-buffer element-buffer :element-array-buffer))
    (bind-vao 0)
    vao))

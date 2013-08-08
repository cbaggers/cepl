(in-package :cgl)

;;;--------------------------------------------------------------
;;; VAOS ;;;
;;;------;;;

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

;; glVertexAttribPointer
;; ---------------------
;; GL_BYTE
;; GL_UNSIGNED_BYTE
;; GL_SHORT
;; GL_UNSIGNED_SHORT
;; GL_INT
;; GL_UNSIGNED_INT 
;; GL_HALF_FLOAT
;; GL_FLOAT

;; GL_DOUBLE
;; GL_FIXED
;; GL_INT_2_10_10_10_REV
;; GL_UNSIGNED_INT_2_10_10_10_REV

;; glVertexAttribLPointer 
;; ----------------------
;; GL_DOUBLE 

;; buffer format is a list whose sublists are of the format
;; type, length, byte-offset-from-start-of-buffer

;; For element-array-buffer the indices can be unsigned bytes, 
;; unsigned shorts, or unsigned ints. 

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


;; buffer format is a list whose sublists are of the format
;; type, length, byte-offset-from-start-of-buffer

(defun make-vao-from-gpu-arrays
    (gpu-arrays &optional indicies-array)
  "makes a vao using a list of gpu-arrays as the source data
   (remember that you can also use gpu-sub-array here if you
   need a subsection of a gpu-array).
   You can also specify an indicies-array which will be used as
   the indicies when rendering"
  (let ((element-buffer (when indicies-array
                          (gpuarray-buffer indicies-array)))
        (vao (gl:gen-vertex-array))
        (attr 0))
    (force-bind-vao vao)
    (loop for gpu-array in gpu-arrays
       :do (let* ((buffer (gpuarray-buffer gpu-array))
                  (format (nth (gpuarray-format-index gpu-array)
                               (glbuffer-format buffer))))
             (force-bind-buffer buffer :array-buffer)
             (setf attr (+ attr (gl-assign-attrib-pointers
                                 (let ((type (first format)))
                                   (if (listp type) (second type) type))
                                 attr
                                 (+ (third format)
                                    (cgl:foreign-type-index 
                                     (first format) 
                                     (gpuarray-start gpu-array)))))))) 
    ;; the line above needs start to be taken into account ^^^
    (when element-buffer
      (force-bind-buffer element-buffer :element-array-buffer))
    (bind-vao 0)
    vao))

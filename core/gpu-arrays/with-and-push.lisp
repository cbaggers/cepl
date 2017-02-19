(in-package :cepl.gpu-arrays)

(defmacro with-gpu-array-as-pointer
    ((temp-name gpu-array &key (access-type :read-write) (target :array-buffer))
     &body body)
  "This macro is really handy if you need to have random access
   to the data on the gpu. It takes a gpu-array and maps it
   giving you the pointer"
  (assert (keywordp target))
  (unless (find access-type '(:read-write :read-only :write-only))
    (error "The access argument must be set to :read-write :read-only or :write-only"))
  (let ((glarray-pointer (gensym "POINTER"))
        (array-sym (gensym "BUFFER"))
        (buffer-sym (gensym "BUFFER"))
        (gtarget (gensym "target")))
    `(progn
       (let ((,array-sym ,gpu-array))
         (unless (typep ,array-sym 'gpu-array)
           (if (typep ,array-sym 'gpu-array-t)
               (error "Unfortunately cepl doesnt not support texture backed gpu-array right now, it should, and it will...But not today. Prod me with a github issue if you need this urgently")
               (error "with-gpu-array-* does not support the type ~s"
                      (type-of ,array-sym))))
         (let ((,buffer-sym (gpu-array-buffer ,array-sym))
               (,gtarget ,target))
           (cepl.gpu-buffers::with-buffer
               (foo ,buffer-sym ,gtarget)
             (gl:with-mapped-buffer (,glarray-pointer
                                     ,gtarget
                                     ,access-type)
               (if (pointer-eq ,glarray-pointer (null-pointer))
                   (error "with-gpu-array-as-*: buffer mapped to null pointer~%Have you defintely got an opengl context?~%~s"
                          ,glarray-pointer)
                   (let ((,temp-name ,glarray-pointer))
                     ,@body)))))))))

;; [TODO] Dont require a temporary name, just use the one it has
;;        this makes it feel more magical to me and also it is
;;        in-line with things like with-slots
;; [TODO] Need to unmap if something goes wrong
(defmacro with-gpu-array-as-c-array
    ((temp-name gpu-array &key (access-type :read-write)) &body body)
  (let ((ggpu-array (gensym "gpu-array")))
    `(let ((,ggpu-array ,gpu-array))
       (with-gpu-array-as-pointer (,temp-name ,ggpu-array :access-type ,access-type)
         (let ((,temp-name
                (make-c-array-from-pointer
                 (gpu-array-dimensions ,ggpu-array)
                 (element-type ,ggpu-array)
                 (cffi:inc-pointer
                  ,temp-name
                  (gpu-array-bb-offset-in-bytes-into-buffer ,ggpu-array)))))
           ,@body)))))


(defmethod print-mem ((thing gpu-array-bb) &optional (size-in-bytes 64) (offset 0))
  (with-gpu-array-as-pointer (a thing :access-type :read-only)
    (print-mem (cffi:inc-pointer a offset) size-in-bytes)))

(defun gpu-array-pull-1 (gpu-array)
  "This function returns the contents of the gpu array as a c-array
   Note that you often dont need to use this as the generic
   function pull-g will call this function if given a gpu-array"
  (with-gpu-array-as-c-array (x gpu-array :access-type :read-only)
    (clone-c-array x)))

;; allignmetn
(defmethod push-g ((object list) (destination gpu-array-bb))
  (with-c-array (tmp (make-c-array object
                                   :dimensions (dimensions destination)
                                   :element-type (element-type destination)))
    (push-g tmp destination)))

(defmethod push-g ((object c-array) (destination gpu-array-bb))
  (let* ((type (gpu-array-bb-element-type destination))
         (ob-dimen (dimensions object))
         (des-dimen (dimensions object)))
    (if (and (eq (element-type object) type)
             (if (= 1 (length des-dimen) (length ob-dimen))
                 (<= (first ob-dimen) (first des-dimen))
                 (equal ob-dimen des-dimen)))
        (cepl.gpu-buffers::gpu-array-sub-data
         destination object :types-must-match t)
        (error "If the arrays are 1D then the length of the source array must
be <= length of the destination array. If the arrays have more than 1
dimension then their sizes must match exactly"))
    destination))

(defmethod pull1-g ((object gpu-array-bb))
  (gpu-array-pull-1 object))

(defmethod pull-g ((object gpu-array-bb))
  (with-gpu-array-as-c-array (x object :access-type :read-only)
    (pull1-g x)))

(defun gpu-array-element-type (gpu-array)
  (etypecase gpu-array
    (gpu-array-t
     (%cepl.types::gpu-array-t-image-format gpu-array))
    (gpu-array-bb
     (cepl.gpu-arrays.buffer-backed::gpu-array-bb-element-type gpu-array))))

(defun backed-by (gpu-array)
  (etypecase gpu-array
    (gpu-array-t :texture)
    (gpu-array-bb :buffer)))

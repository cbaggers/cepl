(in-package :cepl.ubos)

;;---------------------------------------------------

(defvar *ubo-id-lock* (bt:make-lock))
(defvar *lowest-unused-ubo-id* 0)
(defvar *freed-ubo-id* nil)

(defun+ get-free-ubo-id ()
  (bt:with-lock-held (*ubo-id-lock*)
    (if *freed-ubo-id*
        (pop *freed-ubo-id*)
        (incf *lowest-unused-ubo-id*))))

;;---------------------------------------------------

(defmethod print-object ((ubo ubo) stream)
  (format stream "#<UBO (~s) ~s>" (ubo-id ubo)
          (element-type (ubo-data ubo))))

;;---------------------------------------------------

(defun+ make-ubo (data &optional element-type)
  (let ((ubo (%make-ubo :id (get-free-ubo-id)
                        :data (if (typep data 'gpu-array)
                                  data
                                  (make-gpu-array
                                   (when data (vector data))
                                   :dimensions 1
                                   :element-type element-type))
                        :index 0)))
    (%bind-ubo ubo)))


(defun+ make-ubo-from-array (data &optional (index 0) element-type)
  (assert (>= index 0))
  (assert (or (null element-type) (symbolp element-type)))
  (let* ((id (get-free-ubo-id))
         (ubo
          (etypecase data
            (gpu-array
             (when element-type
               (assert (equal element-type (element-type data)) ()
                       'make-ubo-from-array-bad-type
                       :data data :element-type element-type))
             (%make-ubo :id id
                        :data data
                        :index index
                        :owns-gpu-array nil))
            (c-array
             (assert (equal element-type (element-type data)) ()
                     'make-ubo-from-array-bad-type
                     :data data :element-type element-type)
             (%make-ubo :id id
                        :data (make-gpu-array
                               (vector (aref-c data index))
                               :dimensions 1
                               :element-type (element-type data))
                        :index 0))
            (uploadable-lisp-seq
             (make-ubo (elt data index) element-type)))))
    (%bind-ubo ubo)))

(defun+ make-ubo-from-buffer (&rest not-yet-implemented)
  (declare (ignore not-yet-implemented))
  (error "make-ubo-from-buffer is not yet implemented"))

(deferror make-ubo-from-array-bad-type () (data element-type)
    "CEPL: Invalid element-type for make-ubo-from-array

Attemped to make a ubo from ~s which but also specified that the element-type
should be ~s" data element-type)

;;---------------------------------------------------

(defun+ ubo-data-type (ubo)
  (gpu-array-bb-element-type (ubo-data ubo)))

;;---------------------------------------------------

;; {TODO} using the id as the binding point is crazy town as it doesnt
;;        take :max-uniform-buffer-bindings into account.
;;        (For example it's only 84 on my desktop)
;;
;;        Ok, not quite as drastic as assumed, ubo-id is artificial and
;;        we always make it as low as possible. However we should get
;;        the max on cepl init, and check on ubo construction.
(defun+ %bind-ubo (ubo)
  (let* ((data (ubo-data ubo))
         (type (ubo-data-type ubo))
         (offset (+ (gpu-array-bb-offset-in-bytes-into-buffer data)
                    (cepl.c-arrays::gl-calc-byte-size
                     type (list (ubo-index ubo)) 1)))
         (size (gl-type-size type))
         (gpu-buffer (gpu-array-buffer data)))
    (cepl.context::ubo-bind-buffer-id-range
     (cepl-context)
     (gpu-buffer-id gpu-buffer)
     (ubo-id ubo)
     offset
     size))
  ubo)

;;---------------------------------------------------

(defn copy-c-array-to-ubo ((src c-array) (dst ubo))
    ubo
  (cepl.gpu-arrays::copy-c-array-to-buffer-backed-gpu-array
   src (subseq-g (ubo-data dst) 0 1))
  dst)

(defn copy-lisp-list-to-ubo ((src list)
                             (dst ubo))
    ubo
  (let ((element-type (element-type (ubo-data dst))))
    (with-c-array-freed (arr (make-c-array
                              (list src)
                              :dimensions 1
                              :element-type element-type))
      (copy-c-array-to-ubo arr dst))))

(defn copy-lisp-array-to-ubo ((src array)
                              (dst ubo))
    ubo
  (let ((element-type (element-type (ubo-data dst))))
    (with-c-array-freed (arr (make-c-array
                              (list (row-major-aref src 0))
                              :dimensions 1
                              :element-type element-type))
      (copy-c-array-to-ubo arr dst))))

(defn copy-ubo-to-new-lisp-data ((src ubo)) t
  (elt (cepl.gpu-arrays::copy-buffer-backed-gpu-array-to-new-lisp-data
        (subseq-g (ubo-data src) 0 1))
       0))

(defn copy-ubo-to-new-c-array ((src ubo)) c-array
  (let* ((data (ubo-data src)))
    (cepl.gpu-arrays::copy-buffer-backed-gpu-array-to-new-c-array
             (subseq-g data 0 1))))


(defmethod push-g ((object c-array) (destination ubo))
  (copy-c-array-to-ubo object destination))
(defmethod push-g ((object list) (destination ubo))
  (copy-lisp-list-to-ubo object destination))
(defmethod push-g ((object array) (destination ubo))
  (copy-lisp-array-to-ubo object destination))


(defmethod pull-g ((object ubo))
  (copy-ubo-to-new-lisp-data object))


(defmethod copy-g ((source c-array) (destination ubo))
  (copy-c-array-to-ubo source destination))
(defmethod copy-g ((source list) (destination ubo))
  (copy-lisp-list-to-ubo source destination))
(defmethod copy-g ((source array) (destination ubo))
  (copy-lisp-array-to-ubo source destination))
(defmethod copy-g ((object ubo) (destination (eql :lisp)))
  (copy-ubo-to-new-lisp-data object))

;;---------------------------------------------------

(defmethod free ((object ubo))
  (let ((data (ubo-data object)))
    (when (and data (ubo-owns-gpu-array object))
      (cepl.gpu-arrays.buffer-backed::free-gpu-array-bb data))
    (setf (ubo-data object) +null-buffer-backed-gpu-array+))
  +null-buffer-backed-gpu-array+
  t)

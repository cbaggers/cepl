(in-package :cepl.ssbos)

;;---------------------------------------------------

(defvar *ssbo-id-lock* (bt:make-lock))
(defvar *lowest-unused-ssbo-id* 0)
(defvar *freed-ssbo-id* nil)

(defun+ get-free-ssbo-id ()
  (bt:with-lock-held (*ssbo-id-lock*)
    (if *freed-ssbo-id*
        (pop *freed-ssbo-id*)
        (incf *lowest-unused-ssbo-id*))))

;;---------------------------------------------------

(defmethod print-object ((ssbo ssbo) stream)
  (format stream "#<SSBO (~s) ~s>" (ssbo-id ssbo)
          (element-type (ssbo-data ssbo))))

;;---------------------------------------------------

(defun+ make-ssbo (data &optional element-type)
  (let* ((data (cond
                 ((gpu-array-bb-p data)
                  data)
                 ((c-array-p data)
                  (make-gpu-array
                   data
                   :dimensions 1
                   :element-type element-type))
                 (t (make-gpu-array
                     (when data (vector data))
                     :dimensions 1
                     :element-type element-type))))
         (ssbo (%make-ssbo :id (get-free-ssbo-id)
                           :data data
                           :index 0)))
    (%bind-ssbo ssbo)))


(defun+ make-ssbo-from-array (data &optional (index 0) element-type)
  (assert (>= index 0))
  (assert (or (null element-type) (symbolp element-type)))
  (let* ((id (get-free-ssbo-id))
         (ssbo
          (etypecase data
            (gpu-array
             (when element-type
               (assert (equal element-type (element-type data)) ()
                       'make-ssbo-from-array-bad-type
                       :data data :element-type element-type))
             (%make-ssbo :id id
                         :data data
                         :index index
                         :owns-gpu-array nil))
            (c-array
             (assert (equal element-type (element-type data)) ()
                     'make-ssbo-from-array-bad-type
                     :data data :element-type element-type)
             (%make-ssbo :id id
                         :data (make-gpu-array
                                (vector (aref-c data index))
                                :dimensions 1
                                :element-type (element-type data))
                         :index 0))
            (uploadable-lisp-seq
             (make-ssbo (elt data index) element-type)))))
    (%bind-ssbo ssbo)))

(defun+ make-ssbo-from-buffer (&rest not-yet-implemented)
  (declare (ignore not-yet-implemented))
  (error "make-ssbo-from-buffer is not yet implemented"))

(deferror make-ssbo-from-array-bad-type () (data element-type)
    "CEPL: Invalid element-type for make-ssbo-from-array

Attemped to make a ssbo from ~s which but also specified that the element-type
should be ~s" data element-type)

;;---------------------------------------------------

(defun+ ssbo-data-type (ssbo)
  (gpu-array-bb-element-type (ssbo-data ssbo)))

;;---------------------------------------------------

;; {TODO} using the id as the binding point is crazy town as it doesnt
;;        take :max-uniform-buffer-bindings into account.
;;        (For example it's only 84 on my desktop)
(defun+ %bind-ssbo (ssbo)
  (let* ((data (ssbo-data ssbo))
         (type (ssbo-data-type ssbo))
         (offset (+ (gpu-array-bb-offset-in-bytes-into-buffer data)
                    (cepl.c-arrays::gl-calc-byte-size
                     type (list (ssbo-index ssbo)) 1)))
         (size (gl-type-size type))
         (gpu-buffer (gpu-array-buffer data)))
    (cepl.context::ssbo-bind-buffer-id-range
     (cepl-context)
     (gpu-buffer-id gpu-buffer)
     (ssbo-id ssbo)
     offset
     size))
  ssbo)

;;---------------------------------------------------

(defn copy-c-array-to-ssbo ((src c-array) (dst ssbo))
    ssbo
  (cepl.gpu-arrays::copy-c-array-to-buffer-backed-gpu-array
   src (subseq-g (ssbo-data dst) 0 1))
  dst)

(defn copy-lisp-list-to-ssbo ((src list)
                             (dst ssbo))
    ssbo
  (let ((element-type (element-type (ssbo-data dst))))
    (with-c-array-freed (arr (make-c-array
                              (list src)
                              :dimensions 1
                              :element-type element-type))
      (copy-c-array-to-ssbo arr dst))))

(defn copy-lisp-array-to-ssbo ((src array)
                              (dst ssbo))
    ssbo
  (let ((element-type (element-type (ssbo-data dst))))
    (with-c-array-freed (arr (make-c-array
                              (list (row-major-aref src 0))
                              :dimensions 1
                              :element-type element-type))
      (copy-c-array-to-ssbo arr dst))))

(defn copy-ssbo-to-new-lisp-data ((src ssbo)) t
  (elt (cepl.gpu-arrays::copy-buffer-backed-gpu-array-to-new-lisp-data
        (subseq-g (ssbo-data src) 0 1))
       0))

(defn copy-ssbo-to-new-c-array ((src ssbo)) c-array
  (let* ((data (ssbo-data src)))
    (cepl.gpu-arrays::copy-buffer-backed-gpu-array-to-new-c-array
             (subseq-g data 0 1))))



(defmethod push-g ((object c-array) (destination ssbo))
  (copy-c-array-to-ssbo object destination))
(defmethod push-g ((object list) (destination ssbo))
  (copy-lisp-list-to-ssbo object destination))
(defmethod push-g ((object array) (destination ssbo))
  (copy-lisp-array-to-ssbo object destination))


(defmethod pull-g ((object ssbo))
  (copy-ssbo-to-new-lisp-data object))


(defmethod copy-g ((source c-array) (destination ssbo))
  (copy-c-array-to-ssbo source destination))
(defmethod copy-g ((source list) (destination ssbo))
  (copy-lisp-list-to-ssbo source destination))
(defmethod copy-g ((source array) (destination ssbo))
  (copy-lisp-array-to-ssbo source destination))
(defmethod copy-g ((object ssbo) (destination (eql :lisp)))
  (copy-ssbo-to-new-lisp-data object))

;;---------------------------------------------------

(defmethod free ((object ssbo))
  (let ((data (ssbo-data object)))
    (when (and data (ssbo-owns-gpu-array object))
      (cepl.gpu-arrays.buffer-backed::free-gpu-array-bb data))
    (setf (ssbo-data object) +null-buffer-backed-gpu-array+))
  +null-buffer-backed-gpu-array+
  t)

;;---------------------------------------------------

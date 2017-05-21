(in-package :cepl.gpu-arrays.buffer-backed)

;; [TODO] Justify your use of the gl- prefix everywhere.
;; [TODO] How do we free these? Tag buffer format type as :free and handle?

;;;--------------------------------------------------------------
;;; GPUARRAYS ;;;
;;;-----------;;;

(defmethod print-object ((object gpu-array) stream)
  (if (initialized-p object)
      (format stream "#<GPU-ARRAY :element-type ~s :dimensions ~a :backed-by :BUFFER>"
              (element-type object)
              (gpu-array-dimensions object))
      (format stream "#<GPU-ARRAY :UNINITIALIZED :backed-by :BUFFER>")))

;; defmethod print-mem can be found further down the page

(defmethod free ((object gpu-array))
  (free-gpu-array-bb object))

(defgeneric cepl.gpu-arrays:free-gpu-array (gpu-array))
(defmethod cepl.gpu-arrays:free-gpu-array ((gpu-array gpu-array))
  (free-gpu-array-bb gpu-array))

(defun gpu-array-buffer (gpu-array)
  (%cepl.types::gpu-array-bb-buffer gpu-array))

(defun gpu-array-access-style (gpu-array)
  (%cepl.types::gpu-array-bb-access-style gpu-array))

(defun blank-gpu-array-b-object (gpu-array)
  (setf (gpu-array-dimensions gpu-array) nil
        (gpu-array-bb-buffer gpu-array) +null-gpu-buffer+
        (gpu-array-bb-access-style gpu-array) :uninitialized
        (gpu-array-bb-element-type gpu-array) nil
        (gpu-array-bb-byte-size gpu-array) 0
        (gpu-array-bb-offset-in-bytes-into-buffer gpu-array) 0))

;; we only set the buffer slot type as undefined as the size and
;; offset dont change
(defun free-gpu-array-bb (gpu-array)
  (let* ((buffer (gpu-array-bb-buffer gpu-array)))
    (blank-gpu-array-b-object gpu-array)
    (free-buffer buffer))
  nil)

;;---------------------------------------------------------------

(defmethod dimensions ((object gpu-array))
  (gpu-array-dimensions object))

(defmethod element-type ((object gpu-array-bb))
  (gpu-array-bb-element-type object))

;;---------------------------------------------------------------

;; old-gpu (initial-contents &key element-type length access-style)
;; ??????? (initial-contents &key element-type dimensions access-style)
;; [TODO] Check to see we have all the data we need
;; [TODO] all make-gpu-array need the start argument specified
;; [TODO] all dimensions need checking for sanity..some clearly dont have any :D

(defgeneric make-gpu-array (initial-contents &key))

;;---------------------------------------------------------------

(defun make-gpu-array-share-data (gpu-array-to-modify gpu-array-with-data
                                  byte-offset-into-source-data element-type
                                  dimensions &optional byte-size)
  (assert dimensions)
  (let* ((parent gpu-array-with-data)
         (child gpu-array-to-modify)
         (offset byte-offset-into-source-data)
         (dimensions (listify dimensions))
         (byte-size (or byte-size
                        (cepl.c-arrays::gl-calc-byte-size
                         element-type dimensions))))
    (setf (gpu-array-dimensions child) dimensions
          (gpu-array-bb-buffer child) (gpu-array-bb-buffer parent)
          (gpu-array-bb-access-style child) (gpu-array-bb-access-style parent)
          (gpu-array-bb-element-type child) element-type
          (gpu-array-bb-byte-size child) byte-size)
    (setf (gpu-array-bb-offset-in-bytes-into-buffer child)
          (+ (gpu-array-bb-offset-in-bytes-into-buffer parent) offset))
    child))

;;---------------------------------------------------------------
;; no initial-contents

(defun init-gpu-array-no-data (array dimensions element-type access-style)
  (let* ((buffer (buffer-reserve-block
                  (cepl.gpu-buffers::make-managed-gpu-buffer)
                  element-type dimensions :array-buffer
                  access-style))
         (base-arr (aref (gpu-buffer-arrays buffer) 0)))
    (make-gpu-array-share-data
     array base-arr 0 element-type dimensions)))

(defmethod make-gpu-array ((initial-contents null)
                           &key element-type dimensions
                             (access-style :static-draw))
  (declare (ignore initial-contents))
  (cepl.context::if-gl-context
   (init-gpu-array-no-data %pre% dimensions element-type access-style)
   (make-uninitialized-gpu-array-bb)))

;;---------------------------------------------------------------
;; from c-array

(defun init-gpu-array-from-c-array (arr c-array access-style
                                    dimensions)
  (let ((dimensions (listify dimensions))
        (c-dimensions (dimensions c-array)))
    (when dimensions
      (assert (and (every #'= c-dimensions dimensions)
                   (= (length c-dimensions) (length dimensions)))
              ()
              'make-gpu-array-from-c-array-mismatched-dimensions
              :c-arr-dimensions c-dimensions
              :provided-dimensions dimensions))
    (let* ((source (buffer-data (gpu-array-bb-buffer arr)
                                c-array :usage access-style))
           (base-arr (aref (gpu-buffer-arrays source) 0)))
      (make-gpu-array-share-data arr base-arr 0 (c-array-element-type c-array)
                                 c-dimensions))))

(defmethod make-gpu-array ((initial-contents c-array)
                           &key (access-style :static-draw) dimensions)
  (let ((buffer (cepl.gpu-buffers::make-gpu-buffer)))
    (cepl.context::if-gl-context
     (init-gpu-array-from-c-array %pre% initial-contents access-style dimensions)
     (make-uninitialized-gpu-array-bb buffer)
     (list buffer))))

;;---------------------------------------------------------------
;; from lisp-data

(defmethod make-gpu-array ((initial-contents t)
                           &key dimensions element-type (access-style :static-draw))
  (let ((buffer (cepl.gpu-buffers::make-gpu-buffer)))
    (cepl.context::if-gl-context
     (with-c-array (c-array (make-c-array initial-contents :dimensions dimensions
                                          :element-type element-type))
       (init-gpu-array-from-c-array %pre% c-array access-style
                                    (c-array-dimensions c-array)))
     (make-uninitialized-gpu-array-bb buffer)
     (list buffer))))

;;---------------------------------------------------------------
;; from multiple c-arrays

(defun init-gpu-arrays-from-c-arrays (g-arrays c-arrays access-style)
  (let ((buffer (gpu-array-bb-buffer (first g-arrays))))
    (multi-buffer-data buffer c-arrays :array-buffer access-style)
    (loop :for dest :in g-arrays
       :for src :across (gpu-buffer-arrays buffer)
       :for c-array :in c-arrays :do
       (make-gpu-array-share-data dest src 0 (c-array-element-type c-array)
                                  (c-array-dimensions c-array)))))

(defun make-gpu-arrays (c-arrays &key (access-style :static-draw))
  (let* ((buffer (cepl.gpu-buffers::make-gpu-buffer))
         (g-arrays (mapcar (lambda (c-array)
                             (%make-gpu-array-bb
                              :buffer buffer
                              :dimensions (dimensions c-array)
                              :access-style access-style))
                           c-arrays)))
    (cepl.context::if-gl-context
     (init-gpu-arrays-from-c-arrays %pre% c-arrays access-style)
     g-arrays
     (list buffer))))

;;---------------------------------------------------------------

(defun subseq-g (array start &optional end)
  (subseq-g-raw array start end :new-element-type nil))

(defun subseq-g-raw (array start end &key new-element-type)
  (let ((dimensions (dimensions array)))
    (if (> (length dimensions) 1)
        (error "Cannot take subseq of multidimensional array")
        (let* ((source-len (first dimensions))
               (type (or new-element-type
                         (gpu-array-bb-element-type array)))
               (end (or end source-len)))
          (if (and (< start end)
                   (< start source-len)
                   (<= end source-len))
              (make-gpu-array-share-data
               (make-uninitialized-gpu-array-bb) array
               (cepl.c-arrays::gl-calc-byte-size type start) type
               (list (- end start)))
              (error "Invalid subseq start or end for c-array"))))))

;; {TODO} copy buffer to buffer: glCopyBufferSubData
;; http://www.opengl.org/wiki/GLAPI/glCopyBufferSubData

;;---------------------------------------------------------------

(defun adjust-gpu-array (buffer-backed-gpu-array new-dimensions
                         &key initial-contents (access-style :static-draw))
  (let* ((new-dimensions (listify new-dimensions))
         (arr buffer-backed-gpu-array)
         (old-dim (gpu-array-dimensions arr))
         (buffer-arrays (gpu-buffer-arrays (gpu-array-bb-buffer arr)))
         (element-type (gpu-array-bb-element-type arr)))
    (assert (= (length (gpu-array-dimensions arr)) (length new-dimensions))
            (new-dimensions) 'adjust-gpu-array-mismatched-dimensions
            :current-dim old-dim :new-dim new-dimensions)
    (assert (= (length buffer-arrays) 1) () 'adjust-gpu-array-shared-buffer
            :array arr :shared-count (length buffer-arrays))
    ;;
    (if initial-contents
        (with-c-array (c-array (if (typep initial-contents 'c-array)
                                   initial-contents
                                   (make-c-array initial-contents
                                                 :dimensions new-dimensions
                                                 :element-type element-type)))
          (init-gpu-array-from-c-array arr c-array access-style
                                       (c-array-dimensions c-array)))
        ;;
        (let* ((buffer (buffer-reserve-block
                        (gpu-array-bb-buffer arr)
                        element-type new-dimensions :array-buffer
                        access-style))
               (base-arr (aref (gpu-buffer-arrays buffer) 0)))
          (make-gpu-array-share-data
           arr base-arr 0 element-type new-dimensions)))))

;;---------------------------------------------------------------

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

(defn-inline gpu-array-buffer ((gpu-array gpu-array-bb)) gpu-buffer
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%cepl.types::gpu-array-bb-buffer gpu-array))

(defn-inline gpu-array-access-style ((gpu-array gpu-array-bb)) symbol
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (%cepl.types::gpu-array-bb-access-style gpu-array))

(defun+ blank-gpu-array-b-object (gpu-array)
  (setf (gpu-array-dimensions gpu-array) nil
        (gpu-array-bb-buffer gpu-array) +null-gpu-buffer+
        (gpu-array-bb-access-style gpu-array) :uninitialized
        (gpu-array-bb-element-type gpu-array) nil
        (gpu-array-bb-byte-size gpu-array) 0
        (gpu-array-bb-offset-in-bytes-into-buffer gpu-array) 0))

;; we only set the buffer slot type as undefined as the size and
;; offset dont change
(defun+ free-gpu-array-bb (gpu-array)
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

(defn make-gpu-array-share-data ((gpu-array-to-modify gpu-array-bb)
                                 (gpu-array-with-data gpu-array-bb)
                                 (byte-offset-into-source-data integer)
                                 element-type
                                 dimensions
                                 &optional
                                 byte-size
                                 (row-alignment (integer 1 4) 1))
    gpu-array-bb
  (declare (profile t))
  (assert dimensions)
  (let* ((parent gpu-array-with-data)
         (child gpu-array-to-modify)
         (offset byte-offset-into-source-data)
         (dimensions (listify dimensions))
         (byte-size (or byte-size
                        (cepl.c-arrays::gl-calc-byte-size
                         element-type dimensions row-alignment))))
    (setf (gpu-array-dimensions child) dimensions
          (gpu-array-bb-buffer child) (gpu-array-bb-buffer parent)
          (gpu-array-bb-access-style child) (gpu-array-bb-access-style parent)
          (gpu-array-bb-element-type child) element-type
          (gpu-array-bb-element-byte-size child) (gl-type-size element-type)
          (gpu-array-bb-byte-size child) byte-size)
    (setf (gpu-array-bb-offset-in-bytes-into-buffer child)
          (+ (gpu-array-bb-offset-in-bytes-into-buffer parent) offset))
    child))

;;---------------------------------------------------------------
;; no initial-contents

(defun+ init-gpu-array-no-data (array dimensions element-type access-style)
  (let* ((buffer (buffer-reserve-block
                  (cepl.gpu-buffers::make-gpu-buffer)
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

(defun+ init-gpu-array-from-c-array (arr c-array access-style
                                    dimensions)
  (let ((dimensions (listify dimensions))
        (c-dimensions (c-array-dimensions c-array)))
    (when dimensions
      (assert (equal c-dimensions dimensions)
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
     (with-c-array-freed (c-array (make-c-array initial-contents :dimensions dimensions
                                          :element-type element-type))
       (init-gpu-array-from-c-array %pre% c-array access-style
                                    (c-array-dimensions c-array)))
     (make-uninitialized-gpu-array-bb buffer)
     (list buffer))))

;;---------------------------------------------------------------
;; from multiple c-arrays

(defun+ init-gpu-arrays-from-c-arrays (g-arrays c-arrays access-style)
  (let ((buffer (gpu-array-bb-buffer (first g-arrays))))
    (multi-buffer-data buffer c-arrays :array-buffer access-style)
    (loop :for dest :in g-arrays
       :for src :across (gpu-buffer-arrays buffer)
       :for c-array :in c-arrays :do
       (make-gpu-array-share-data dest src 0 (c-array-element-type c-array)
                                  (c-array-dimensions c-array)))))


(defun+ make-gpu-arrays (c-arrays &key (access-style :static-draw))
  (let* ((buffer (cepl.gpu-buffers::make-gpu-buffer))
         (g-arrays (mapcar (lambda (c-array)
                             (%make-gpu-array-bb
                              :buffer buffer
                              :dimensions (c-array-dimensions c-array)
                              :access-style access-style
                              :row-alignment (c-array-row-alignment c-array)))
                           c-arrays)))
    (cepl.context::if-gl-context
     (init-gpu-arrays-from-c-arrays %pre% c-arrays access-style)
     g-arrays
     (list buffer))))

;;---------------------------------------------------------------

(defun process-layout (layout)
  (assert (listp layout) () 'invalid-gpu-arrays-layout :layout layout)
  (when (eq (first layout) 'quote)
    (error 'quote-in-buffer-layout :layout layout))
  (assert (and (listp layout)
               (find :dimensions layout)
               (find :element-type layout))
          () 'invalid-gpu-arrays-layout :layout layout)
  (destructuring-bind (&key dimensions element-type) layout
    (let* ((dimensions (listify dimensions))
           (elem-count (reduce #'* dimensions))
           (element-type (if (cepl.pixel-formats:pixel-format-p element-type)
                             (pixel-format->lisp-type element-type)
                             element-type))
           (byte-size (* elem-count (gl-type-size element-type))))
      (list element-type dimensions byte-size))))

(defun+ make-gpu-arrays-from-buffer (buffer
                                     layouts
                                     &key (access-style :static-draw)
                                     (keep-data nil))
  (check-type buffer gpu-buffer)
  (let* ((processed (mapcar #'process-layout (listify layouts)))
         (current-sizes (map 'list #'gpu-array-bb-byte-size
                             (gpu-buffer-arrays buffer)))
         (byte-sizes (mapcar #'third processed)))
    (if keep-data
        (progn
          (assert (= (length layouts) (length (gpu-buffer-arrays buffer))) ()
                  'make-arrays-layout-count-mismatch
                  :layouts layouts
                  :current-count (length (gpu-buffer-arrays buffer)))
          (assert (every #'<= byte-sizes current-sizes)
                  () 'make-arrays-layout-mismatch
                  :current-sizes current-sizes
                  :requested-sizes byte-sizes))
        (cepl.gpu-buffers::buffer-reserve-blocks-from-sizes
         buffer byte-sizes :array-buffer access-style))
    (loop
       :for (element-type dimensions byte-size) :in processed
       :for src :across (gpu-buffer-arrays buffer)
       :for g-array := (%make-gpu-array-bb
                        :buffer buffer
                        :dimensions dimensions
                        :access-style access-style
                        :row-alignment 1)
       :collect (make-gpu-array-share-data g-array
                                           src
                                           0
                                           element-type
                                           dimensions))))

(defun+ make-gpu-array-from-buffer (buffer
                                    &key
                                    element-type
                                    dimensions
                                    (access-style :static-draw)
                                    (keep-data nil))
  (assert (and element-type dimensions) ()
          'gpu-array-from-buffer-missing-args
          :element-type element-type
          :dimensions dimensions)
  (first (make-gpu-arrays-from-buffer
          buffer
          `((:element-type ,element-type :dimensions ,dimensions))
          :access-style access-style
          :keep-data keep-data)))

(defun+ make-gpu-arrays-from-buffer-id (gl-buffer-id
                                        layouts
                                        &key (access-style :static-draw))
  (let* ((processed-layouts (mapcar #'process-layout (listify layouts)))
         (buffer (make-gpu-buffer-from-id gl-buffer-id
                                          :layouts layouts
                                          :usage access-style)))
    (loop
       :for (element-type dimensions byte-size) :in processed-layouts
       :for src :across (gpu-buffer-arrays buffer)
       :for g-array := (%make-gpu-array-bb
                        :buffer buffer
                        :dimensions dimensions
                        :access-style access-style
                        :row-alignment 1)
       :collect (make-gpu-array-share-data g-array
                                           src
                                           0
                                           element-type
                                           dimensions))))

(defun+ make-gpu-array-from-buffer-id (gl-buffer-id
                                       &key
                                       element-type
                                       dimensions
                                       (access-style :static-draw))
  (assert (and element-type dimensions) ()
          'gpu-array-from-id-missing-args
          :element-type element-type
          :dimensions dimensions)
  (first (make-gpu-arrays-from-buffer-id
          gl-buffer-id
          `((:element-type ,element-type :dimensions ,dimensions))
          :access-style access-style)))

;;---------------------------------------------------------------

(defn subseq-g ((array gpu-array-bb) (start c-array-index)
                &optional (end c-array-index))
    gpu-array-bb
  (declare (profile t))
  (subseq-g-raw array start end :new-element-type nil))

(defn subseq-g-raw ((array gpu-array-bb)
                    (start c-array-index)
                    (end (or null c-array-index))
                    &key (new-element-type t))
    gpu-array-bb
  (declare (profile t))
  (let ((dimensions (dimensions array)))
    (assert (= (length dimensions) 1) ()
            "Cannot take subseq of multidimensional array")
    (let* ((source-len (first dimensions))
           (type (or new-element-type
                     (gpu-array-bb-element-type array)))
           (row-alignment
            (gpu-array-bb-row-alignment array))
           (end (or end source-len)))
      (assert (and (< start end)
                   (< start source-len)
                   (<= end source-len))
              () "Invalid subseq start or end for c-array")
      (make-gpu-array-share-data
       (make-uninitialized-gpu-array-bb) array
       (cepl.c-arrays::gl-calc-byte-size type start row-alignment)
       type
       (list (- end start))))))

;; {TODO} copy buffer to buffer: glCopyBufferSubData
;; http://www.opengl.org/wiki/GLAPI/glCopyBufferSubData

;;---------------------------------------------------------------

(defun+ adjust-gpu-array (buffer-backed-gpu-array new-dimensions
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
        (if (typep initial-contents 'c-array)
            (init-gpu-array-from-c-array arr initial-contents access-style
                                         (c-array-dimensions initial-contents))
            (with-c-array-freed
                (c-array (make-c-array initial-contents
                                       :dimensions new-dimensions
                                       :element-type element-type))
              (init-gpu-array-from-c-array arr c-array access-style
                                           (c-array-dimensions c-array))))
        ;;
        (let* ((buffer (buffer-reserve-block
                        (gpu-array-bb-buffer arr)
                        element-type new-dimensions :array-buffer
                        access-style))
               (base-arr (aref (gpu-buffer-arrays buffer) 0)))
          (make-gpu-array-share-data
           arr base-arr 0 element-type new-dimensions)))))

;;---------------------------------------------------------------

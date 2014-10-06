(in-package :cgl)

;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

(defstruct (vertex-stream (:constructor make-raw-vertex-stream 
                                        (&key vao start length
                                              index-type managed
                                              gpu-arrays))) 
  "vertex-streams are the structure we use in cepl to pass 
   information to our programs on what to draw and how to draw 
   it.

   It basically adds the only things that arent captured in the
   vao but are needed to draw, namely the range of data to draw
   and the style of drawing.

   If you are using c-arrays then be sure to use the 
   make-vertex-stream function as it does all the
   work for you."
  vao
  (start 0 :type unsigned-byte)
  (length 1 :type unsigned-byte)
  (index-type nil)
  (gpu-arrays nil)
  (managed nil))

(defmethod gl-free ((object vertex-stream))
  (free-vertex-stream object))

(defun blank-vertex-stream (vertex-stream)
  (setf (vertex-stream-vao vertex-stream) nil)
  (setf (vertex-stream-start vertex-stream) 0)
  (setf (vertex-stream-length vertex-stream) 0)
  (setf (vertex-stream-index-type vertex-stream) nil)
  (setf (vertex-stream-managed vertex-stream) nil))

(defun free-vertex-stream (vertex-stream)
  (when (vertex-stream-managed vertex-stream)
    (free-vao (vertex-stream-vao vertex-stream)))
  ;; (when (vertex-stream-gpu-arrays vertex-stream)
  ;;   (mapcar #'free-gpu-array-b (vertex-stream-gpu-arrays vertex-stream)))
  (blank-vertex-stream vertex-stream))


(defun make-vertex-stream (gpu-arrays &key index-array (start 0) length 
                                        retain-arrays)
  "This function simplifies making the vertex-stream if you are 
   storing the data in gpu-arrays.

   Remember that you can also use gpu-sub-arrays in here if you
   want to limit the data you are using, for example the 
   following is perfectly legal code:
   (make-vertex-stream 
     :gpu-arrays `(,(gpu-sub-array monster-pos-data 1000 2000)
                  ,(gpu-sub-array monster-col-data 1000 2000))
     :index-array monster-index-array
     :length 1000)"
  (let* ((gpu-arrays (if (gpuarray-p gpu-arrays) (list gpu-arrays) gpu-arrays))
         ;; THIS SEEMS WEIRD BUT IF HAVE INDICES ARRAY THEN
         ;; LENGTH MUST BE LENGTH OF INDICES ARRAY NOT NUMBER
         ;; OF TRIANGLES
         (length (or length 
                     (when index-array (first (dimensions index-array)))
                     (apply #'min (mapcar #'(lambda (x) (first (dimensions x)))
                                          gpu-arrays)))))
    (if (and (every #'1d-p gpu-arrays) (if index-array (1d-p index-array) t))
        (make-raw-vertex-stream :vao (make-vao gpu-arrays index-array)
                                :start start
                                :length length
                                :index-type (when index-array 
                                              (element-type index-array))
                                :managed t
                                :gpu-arrays (when retain-arrays gpu-arrays))
        (error "You can only make vertex-streams from 1D arrays"))))

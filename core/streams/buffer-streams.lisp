(in-package :cepl.streams)

;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

(defstruct (buffer-stream (:constructor make-raw-buffer-stream
                                        (&key vao start length
                                              index-type managed
                                              gpu-arrays)))
  "buffer-streams are the structure we use in cepl to pass
   information to our programs on what to draw and how to draw
   it.

   It basically adds the only things that arent captured in the
   vao but are needed to draw, namely the range of data to draw
   and the style of drawing.

   If you are using c-arrays then be sure to use the
   make-buffer-stream function as it does all the
   work for you."
  vao
  (start 0 :type unsigned-byte)
  (length 1 :type unsigned-byte)
  (index-type nil)
  (gpu-arrays nil)
  (managed nil))

(defmethod print-object ((object buffer-stream) stream)
  (format stream "#<JUNGL:BUFFER-STREAM (~s) :LENGTH ~s~@[ :ARRAYS ~s~]~@[ :INDEXED ~s~]>"
	  (buffer-stream-vao object)
	  (buffer-stream-length object)
	  (when (buffer-stream-gpu-arrays object)
	    (length (first (buffer-stream-gpu-arrays object))))
	  (not (null (buffer-stream-index-type object)))))

(defmethod free ((object buffer-stream))
  (free-buffer-stream object))

(defun blank-buffer-stream (buffer-stream)
  (setf (buffer-stream-vao buffer-stream) nil)
  (setf (buffer-stream-start buffer-stream) 0)
  (setf (buffer-stream-length buffer-stream) 0)
  (setf (buffer-stream-index-type buffer-stream) nil)
  (setf (buffer-stream-managed buffer-stream) nil))

(defun free-buffer-stream (buffer-stream)
  (when (buffer-stream-managed buffer-stream)
    (free-vao (buffer-stream-vao buffer-stream)))
  ;; (when (buffer-stream-gpu-arrays buffer-stream)
  ;;   (mapcar #'free-gpu-array-b (buffer-stream-gpu-arrays buffer-stream)))
  (blank-buffer-stream buffer-stream))


(defun make-buffer-stream (gpu-arrays &key index-array (start 0) length
                                        retain-arrays)
  "This function simplifies making the buffer-stream if you are
   storing the data in gpu-arrays.

   Remember that you can also use gpu-sub-arrays in here if you
   want to limit the data you are using, for example the
   following is perfectly legal code:
   (make-buffer-stream
     :gpu-arrays `(,(gpu-sub-array monster-pos-data 1000 2000)
                  ,(gpu-sub-array monster-col-data 1000 2000))
     :index-array monster-index-array
     :length 1000)"
  (let* ((gpu-arrays (listify gpu-arrays)))
    (make-buffer-stream-from-id (make-vao gpu-arrays index-array)
                                gpu-arrays
                                :index-array index-array
                                :start start
                                :length length
                                :retain-arrays retain-arrays)))

(defun make-buffer-stream-from-id (vao-gl-object gpu-arrays
                                   &key index-array (start 0) length
                                     retain-arrays)
  "This function simplifies making the buffer-stream if you are
   storing the data in gpu-arrays.

   Remember that you can also use gpu-sub-arrays in here if you
   want to limit the data you are using, for example the
   following is perfectly legal code:
   (make-buffer-stream
     :gpu-arrays `(,(gpu-sub-array monster-pos-data 1000 2000)
                  ,(gpu-sub-array monster-col-data 1000 2000))
     :index-array monster-index-array
     :length 1000)"
  (unless gpu-arrays
    (error 'make-buffer-stream-with-no-gpu-arrays))
  (let* ((gpu-arrays (listify gpu-arrays))
         ;; THIS SEEMS WEIRD BUT IF HAVE INDICES ARRAY THEN
         ;; LENGTH MUST BE LENGTH OF INDICES ARRAY NOT NUMBER
         ;; OF TRIANGLES
         (length (or length
                     (when index-array (first (dimensions index-array)))
                     (apply #'min (mapcar #'(lambda (x) (first (dimensions x)))
                                          gpu-arrays)))))
    (if (and (every #'1d-p gpu-arrays) (if index-array (1d-p index-array) t))
        (make-raw-buffer-stream
         :vao (make-vao-from-id vao-gl-object gpu-arrays index-array)
         :start start
         :length length
         :index-type (when index-array (element-type index-array))
         :managed t
         :gpu-arrays (when retain-arrays
                       (list gpu-arrays index-array)))
        (error "You can only make buffer-streams from 1D arrays"))))

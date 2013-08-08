(in-package :cgl)

;;;--------------------------------------------------------------
;;; GPUSTREAMS ;;;
;;;------------;;;

(defstruct gpu-stream 
  "gpu-streams are the structure we use in cepl to pass 
   information to our programs on what to draw and how to draw 
   it.

   It basically adds the only things that arent captured in the
   vao but are needed to draw, namely the range of data to draw
   and the style of drawing.

   If you are using c-arrays then be sure to use the 
   make-gpu-stream-from-gpu-arrays function as it does all the
   work for you."
  vao
  (start 0 :type unsigned-byte)
  (length 1 :type unsigned-byte)
  (draw-type :triangles :type symbol)
  (index-type nil))

(let ((vao-pool (make-hash-table)))
  (defun add-vao-to-pool (vao key)
    (setf (gethash key vao-pool) vao)
    vao)

  (defun free-all-vaos-in-pool ()
    (mapcar #'(lambda (x) (declare (ignore x)) 
                      (print "freeing a vao")) 
            vao-pool)))

(defun make-gpu-stream-from-gpu-arrays (gpu-arrays &key indicies-array (start 0)
                                                     length
                                                     (draw-type :triangles))
  "This function simplifies making the gpu-stream if you are 
   storing the data in gpu-arrays.

   Remember that you can also use gpu-sub-arrays in here if you
   want to limit the data you are using, for example the 
   following is perfectly legal code:
   (make-gpu-stream-from-gpu-arrays 
     :gpu-arrays `(,(gpu-sub-array monster-pos-data 1000 2000)
                  ,(gpu-sub-array monster-col-data 1000 2000))
     :indicies-array monster-indicies-array
     :length 1000)"
  (let* ((gpu-arrays (if (gpuarray-p gpu-arrays)
                         (list gpu-arrays)
                         gpu-arrays))
         ;; THIS SEEMS WEIRD BUT IF HAVE INDICES ARRAY THEN
         ;; LENGTH MUST BE LENGTH OF INDICES ARRAY NOT NUMBER
         ;; OF TRIANGLES
         (length (or length 
                     (when indicies-array (gpuarray-length
                                           indicies-array))
                     (apply #'min (mapcar #'gpuarray-length 
                                          gpu-arrays)))))
    
    (make-gpu-stream 
     :vao (make-vao-from-gpu-arrays gpu-arrays indicies-array)
     :start start
     :length length
     :draw-type draw-type
     :index-type (when indicies-array 
                   (gpuarray-type indicies-array)))))

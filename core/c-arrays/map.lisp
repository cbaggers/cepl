(in-package :cepl.c-arrays)

(defn map-c ((function function) (c-array c-array)
             &optional destination-element-type)
    (or null c-array)
  (declare (ignore destination-element-type))
  (loop
     :for i :below (c-array-total-size c-array)
     :do (funcall function (row-major-aref-c c-array i))))

(defn map-c-into ((destination-c-array (or null c-array))
                  (function function)
                  (source-c-array c-array))
    (or null c-array)
  (if destination-c-array
      (loop
         :for i :below (c-array-total-size source-c-array)
         :do (setf (row-major-aref-c destination-c-array i)
                   (funcall function (row-major-aref-c source-c-array i))))
      (map-c function source-c-array)))

;;----------------------------------------------------------------------

(defn %across-c-1d ((func function) (arr c-array))
    c-array
  (declare (optimize (speed 3) (safety 0) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for i :below (the c-array-index (first dim)) :do
       (funcall func arr i)))
  arr)

(defn %across-c-2d ((func function) (arr c-array))
    c-array
  (declare (optimize (speed 3) (safety 0) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for y :below (the c-array-index (second dim)) :do
       (loop :for x :below (the c-array-index (first dim)) :do
          (funcall func arr x y))))
  arr)


(defn %across-c-3d ((func function) (arr c-array))
    c-array
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for z :below (the c-array-index (third dim)) :do
       (loop :for y :below (the c-array-index (second dim)) :do
          (loop :for x :below (the c-array-index (first dim)) :do
             (funcall func arr x y z)))))
  arr)

(defn %across-c-4d ((func function) (arr c-array))
    c-array
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for w :below (the c-array-index (fourth dim)) :do
       (loop :for z :below (the c-array-index (third dim)) :do
          (loop :for y :below (the c-array-index (second dim)) :do
             (loop :for x :below (the c-array-index (first dim)) :do
                (funcall func arr x y z w))))))
  arr)

(defn across-c ((function function) (c-array c-array))
    c-array
  (ecase= (c-array-rank c-array)
    (1 (%across-c-1d function c-array))
    (2 (%across-c-2d function c-array))
    (3 (%across-c-3d function c-array))
    (4 (%across-c-4d function c-array))))

;;----------------------------------------------------------------------

(defn %across-c-ptr-1d ((func function) (arr c-array))
    c-array
  (declare (type function func) (type c-array arr)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for x :below (the c-array-index (first dim)) :do
       (let ((ptr (ptr-index arr x)))
         (funcall func ptr x))))
  arr)

(defn %across-c-ptr-2d ((func function) (arr c-array))
    c-array
  (declare (type function func) (type c-array arr)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for y :below (the c-array-index (second dim)) :do
       (loop :for x :below (the c-array-index (first dim)) :do
          (let ((ptr (ptr-index arr x y)))
            (funcall func ptr x y)))))
  arr)

(defn %across-c-ptr-3d ((func function) (arr c-array))
    c-array
  (declare (type function func) (type c-array arr)
           (optimize (speed 3) (safety 1) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for z :below (the c-array-index (third dim)) :do
       (loop :for y :below (the c-array-index (second dim)) :do
          (loop :for x :below (the c-array-index (first dim)) :do
             (let ((ptr (ptr-index arr x y z)))
               (funcall func ptr x y z))))))
  arr)

(defn %across-c-ptr-4d ((func function) (arr c-array))
    c-array
  (declare (type function func) (type c-array arr)
           (optimize (speed 3) (safety 1) (debug 1)))
  (let ((dim (c-array-dimensions arr)))
    (loop :for w :below (the c-array-index (fourth dim)) :do
       (loop :for z :below (the c-array-index (third dim)) :do
          (loop :for y :below (the c-array-index (second dim)) :do
             (loop :for x :below (the c-array-index (first dim)) :do
                (let ((ptr (ptr-index arr x y z w)))
                  (funcall func ptr x y z w)))))))
  arr)

(defn across-c-ptr ((function function) (c-array c-array))
    c-array
  (ecase= (c-array-rank c-array)
    (1 (%across-c-ptr-1d function c-array))
    (2 (%across-c-ptr-2d function c-array))
    (3 (%across-c-ptr-3d function c-array))
    (4 (%across-c-ptr-4d function c-array))))

(in-package :cepl.c-arrays)

(defn map-c ((function function) (c-array c-array)
             &optional destination-element-type)
    (or null c-array)
  (let ((r (make-c-array nil :dimensions (c-array-dimensions c-array)
                         :element-type (or destination-element-type
                                           (c-array-element-type c-array)))))
    (map-c-into r function c-array)))

(defn map-c-into ((destination-c-array c-array) (function function)
                  (source-c-array c-array))
    (or null c-array)
  (let ((func function)
        (src source-c-array)
        (dst destination-c-array))
    (ecase= (c-array-rank src)
      (1 (%map-c-into-1d dst func src))
      (2 (%map-c-into-2d dst func src))
      (3 (%map-c-into-3d dst func src))
      (4 (%map-c-into-4d dst func src)))))

(defn %map-c-into-1d ((dst (or null c-array)) (func function) (src c-array))
    (or null c-array)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (let ((dim (c-array-dimensions src)))
    (if dst
        (loop :for i :below (first dim) :do
           (setf (aref-c dst i) (funcall func (aref-c src i))))
        (loop :for i :below (first dim) :do
           (funcall func (aref-c src i)))))
  dst)

(defn %map-c-into-2d ((dst (or null c-array)) (func function) (src c-array))
    (or null c-array)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (let ((dim (c-array-dimensions src)))
    (if dst
        (loop :for y :below (second dim) :do
           (loop :for x :below (first dim) :do
              (setf (aref-c dst x y) (funcall func (aref-c src x y)))))
        (loop :for y :below (second dim) :do
           (loop :for x :below (first dim) :do
              (funcall func (aref-c src x y))))))
  dst)

(defn %map-c-into-3d ((dst (or null c-array)) (func function) (src c-array))
    (or null c-array)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (let ((dim (c-array-dimensions src)))
    (if dst
        (loop :for z :below (third dim) :do
           (loop :for y :below (second dim) :do
              (loop :for x :below (first dim) :do
                 (setf (aref-c dst x y z) (funcall func (aref-c src x y z))))))
        (loop :for z :below (third dim) :do
           (loop :for y :below (second dim) :do
              (loop :for x :below (first dim) :do
                 (funcall func (aref-c src x y z)))))))
  dst)

(defn %map-c-into-4d ((dst (or null c-array)) (func function) (src c-array))
    (or null c-array)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (profile t))
  (let ((dim (c-array-dimensions src)))
    (if dst
        (loop :for w :below (fourth dim) :do
           (loop :for z :below (third dim) :do
              (loop :for y :below (second dim) :do
                 (loop :for x :below (first dim) :do
                    (setf (aref-c dst x y z w)
                          (funcall func (aref-c src x y z w)))))))
        (loop :for w :below (fourth dim) :do
           (loop :for z :below (third dim) :do
              (loop :for y :below (second dim) :do
                 (loop :for x :below (first dim) :do
                    (funcall func (aref-c src x y z w))))))))
  dst)

;;----------------------------------------------------------------------

(defn across-c ((function function) (c-array c-array))
    c-array
  (ecase= (c-array-rank c-array)
    (1 (%across-c-1d function c-array))
    (2 (%across-c-2d function c-array))
    (3 (%across-c-3d function c-array))
    (4 (%across-c-4d function c-array))))

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

;;----------------------------------------------------------------------

(defn across-c-ptr ((function function) (c-array c-array))
    c-array
  (ecase= (c-array-rank c-array)
    (1 (%across-c-ptr-1d function c-array))
    (2 (%across-c-ptr-2d function c-array))
    (3 (%across-c-ptr-3d function c-array))
    (4 (%across-c-ptr-4d function c-array))))

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

(in-package :cepl)

(defun sample-2d (function
                  &key (x-range (v! 0 1)) (y-range (v! 0 1))
                    (incr 0.1) (passes 1) (pass-multiple 0.5)
                    (element-type t))
  (let* ((x-dim (floor (/ (- (v:y x-range) (v:x x-range)) incr)))
         (y-dim (floor (/ (- (v:y y-range) (v:x y-range)) incr)))
         (result-array (make-array (list x-dim y-dim)
                                   :element-type element-type)))
    (loop :for pass :below passes :do
       (loop :for x :from (v:x x-range) :to (v:y x-range) :by incr
          :for x-i :from 0 :do
          (loop :for y :from (v:x y-range) :to (v:y y-range) :by incr
             :for y-i :from 0 :do
             (setf (aref result-array x-i y-i) (funcall function x y))))
       (setf x-range (v:* x-range pass-multiple)
             y-range (v:* y-range pass-multiple)
             incr (* incr pass-multiple)))
    result-array))


(defun sample-3d (function
                  &key (x-range (v! 0 1)) (y-range (v! 0 1)) (z-range (v! 0 1))
                    (incr 0.1) (passes 1) (pass-multiple 0.5)
                    (element-type t))
  (let* ((x-dim (floor (/ (- (v:y x-range) (v:x x-range)) incr)))
         (y-dim (floor (/ (- (v:y y-range) (v:x y-range)) incr)))
         (z-dim (floor (/ (- (v:y z-range) (v:x z-range)) incr)))
         (result-array (make-array (list x-dim y-dim z-dim)
                                   :element-type element-type)))
    (loop :for pass :below passes :do
       (loop :for x :from (v:x x-range) :to (v:y x-range) :by incr
          :for x-i :from 0 :do
          (loop :for y :from (v:x y-range) :to (v:y y-range) :by incr
             :for y-i :from 0 :do
             (loop :for z :from (v:x z-range) :to (v:y z-range) :by incr
                :for z-i :from 0 :do
                (setf (aref result-array x-i y-i x-i)
                      (funcall function x y z)))))
       (setf x-range (v:* x-range pass-multiple)
             y-range (v:* y-range pass-multiple)
             z-range (v:* z-range pass-multiple)
             incr (* incr pass-multiple)))
    result-array))

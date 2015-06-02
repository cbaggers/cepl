;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is the 3x3 matrix functionality. 
;; There will be a generic function-set to make this as easy
;; as possible for people writing the games but this will 
;; be in a seperate package (prehaps the base-maths one)

(in-package :matrixn)

(defun convolve (array)
  (let ((data (loop for y across array append
                   (loop for x across array collect
                        (* x y)))))
    (make-array (length data) :initial-contents data)))

(defun copy-array (a)
  (make-array (length a) :initial-contents (loop :for i :across a :collect i)))

(defun normalize-array (array &optional destructive)
  (let ((array (if destructive array (copy-array array))))
    (let ((sum (loop :for i :below (length array) :sum i)))
      (loop :for i :below (length array) :do
         (setf (aref array i) (/ (float (aref array i)) sum))))
    array))


(defun pascal (n)
  (let ((line (make-array (1+ n) :initial-element 1)))
    (loop :for k :below n :for i :from 1 :do
       (setf (aref line i) (* (aref line k) (/ (- n k) (1+ k)))))
    line))

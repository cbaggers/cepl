;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is stub
;; I'm popping these function here as we will eventually need
;; some kind of abstraction aroudn the idea of 'cameras'

(in-package :cepl-camera)

;;;--------------------------------------------------------------

(defun calculate-frustrum-scale (field-of-view-degrees)
  (/ 1.0 (tan (/ (* field-of-view-degrees base-maths:+one-degree-in-radians+) 2.0))))


(defun make-cam-clip-matrix (frustrum-scale)
  (let* ((f-near 1.0)
	 (f-far 45.0)
	 (f-scale frustrum-scale))
    (matrix4:make-matrix4 f-scale 0.0 0.0 0.0
			  0.0 f-scale 0.0 0.0
			  0.0 0.0 (/ (+ f-far f-near)
				     (- f-near f-far)) -1.0
			  0.0 0.0 (/ (* 2 f-far f-near)
				     (- f-near f-far)) 0.0)))

;;;--------------------------------------------------------------

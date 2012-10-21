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


(defun make-cam-clip-matrix (frustrum-scale &optional (near 1.0) (far 45.0))
  (matrix4:make-matrix4 frustrum-scale 0.0 0.0 0.0
			0.0 frustrum-scale 0.0 0.0
			0.0 0.0 (/ (+ far near) (- near far)) -1.0
			0.0 0.0 (/ (* 2.0 far near) (- near far)) 0.0))

;;;--------------------------------------------------------------

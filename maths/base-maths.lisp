;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This contaisn the maths functions that will be used
;; all over cepl as opposed to 'belonging' to a certain
;; segment of game functionality.
;; the base-* packages are meant to be 'used' so that 
;; there is no need to write the package name.

(in-package :base-maths)

(defconstant +float-threshold+ 1.0e-6)
;; [TODO] Need to declare type of these as float
(defconstant +pi+ (the single-float 3.1415926535897932384626433832795))
(defconstant +one-degree-in-radians+ (the single-float (/ (* +pi+ 2.0) 360.0)))
(defconstant +one-radian-in-degrees+ (the single-float (/ 180.0 +pi+)))

;;----------------------------------------------------------------

(defun clamp (min max val)
  (min (max val min) max))

(declaim (inline clampf)
	 (ftype (function ((single-float) (single-float) (single-float)) 
			  (single-float)) 
		clampf))
(defun clampf (min max float)
  (declare (single-float min max float))
  (the single-float (min (max float min) max)))

(declaim (inline float-zero)
	 (ftype (function ((single-float)) 
			  (boolean)) 
		float-zero))
(defun float-zero (x)
  "Returns t if float is essentially zero
   This is to handle the fact that floats get less accurate the
   closer they get to zero so we have to test that a float is 
   within a range rather than being exacting equal to 0.0"
  (declare (single-float x))
  (< (abs x) +float-threshold+))

(declaim (inline float<=0)
	 (ftype (function ((single-float)) 
			  (boolean)) 
		float<=0))
(defun float<=0 (x)
  "Returns t if float is equal or less than zero
   This is to handle the fact that floats get less accurate the
   closer they get to zero so we have to test that a float is 
   within a range rather than being exacting equal to 0.0"
  (declare (single-float x))
  (<= x +float-threshold+))

(declaim (inline float>=0)
	 (ftype (function ((single-float)) 
			  (boolean)) 
		float>=0))
(defun float>=0 (x)
  "Returns t if float is equal or less than zero
   This is to handle the fact that floats get less accurate the
   closer they get to zero so we have to test that a float is 
   within a range rather than being exacting equal to 0.0"
  (declare (single-float x))
  (>= x +float-threshold+))

(declaim (inline float<0)
	 (ftype (function ((single-float)) 
			  (boolean)) 
		float<0))
(defun float<0 (x)
  "Returns t if float is equal or less than zero
   This is to handle the fact that floats get less accurate the
   closer they get to zero so we have to test that a float is 
   within a range rather than being exacting equal to 0.0"
  (declare (single-float x))
  (< x +float-threshold+))

(declaim (inline float>0)
	 (ftype (function ((single-float)) 
			  (boolean)) 
		float>0))
(defun float>0 (x)
  "Returns t if float is equal or less than zero
   This is to handle the fact that floats get less accurate the
   closer they get to zero so we have to test that a float is 
   within a range rather than being exacting equal to 0.0"
  (declare (single-float x))
  (> x +float-threshold+))

;;----------------------------------------------------------------

;;Come back and implement the fast versions of these two
(declaim (inline c-sqrt)
	 (ftype (function ((single-float)) 
			  (single-float)) 
		c-sqrt))
(defun c-sqrt (x)
  "Calculates the square root of a number"
  (declare (single-float x))
  (sqrt x))

(declaim (inline c-inv-sqrt)
	 (ftype (function ((single-float)) 
			  (single-float)) 
		c-inv-sqrt))
(defun c-inv-sqrt (x)
  "Calculates the inverse square root of a number"
  (declare (single-float x))
  (/ 1.0 (sqrt x)))

;;----------------------------------------------------------------

(declaim (inline radians)
	 (ftype (function ((single-float)) (single-float)) radians))
(defun radians (degrees)
  (declare (single-float degrees))
  (the single-float (* degrees +one-degree-in-radians+)))

(declaim (inline degrees)
	 (ftype (function ((single-float)) (single-float)) degrees))
(defun degrees (radians)
  (declare (single-float radians))
  (the single-float (* radians +one-radian-in-degrees+)))

;;----------------------------------------------------------------

(defun lerp (start end progress) (+ start (* progress (- end start))))

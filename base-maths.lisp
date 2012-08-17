;; This contaisn the maths functions that will be used
;; all over cepl as opposed to 'belonging' to a certain
;; segment of game functionality.
;; the base-* packages are meant to be 'used' so that 
;; there is no need to write the package name.

(in-package :base-maths)

(defconstant +float-threshold+ 1.0e-6)
;; [TODO] Need to declare type of these as float
(defconstant +pi+ 3.1415926535897932384626433832795)
(defconstant +one-degree-in-radians+ (/ (* +pi+ 2.0) 360.0))


;----------------------------------------------------------------
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

;----------------------------------------------------------------

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

;----------------------------------------------------------------

;; from quake3
;; float Q_rsqrt( float number )
;; {
;;    long i;
;;    float x2, y;
;;    const float threehalfs = 1.5F;
 
;;    x2 = number * 0.5F;
;;    y  = number;
;;    i  = * ( long * ) &y;// evil floatpoint bit level hacking
;;    i  = 0x5f3759df - ( i >> 1 );            // what the fuck?
;;    y  = * ( float * ) &i;
;;    y  = y * ( threehalfs - ( x2 * y * y ) ); // 1st iteration
;;    return y;
;; }


;;from Ogre
;; typedef union {float f; int i;} IntOrFloat;

;; inline float IvSqrt( float val )       
;; { 
;; #if defined(IV_APPROXIMATION)
;;       assert(val >= 0);
;;       IntOrFloat workval;
;;       workval.f = val;
;;       workval.i -= 0x3f800000; // subtract 127 from biased exponent
;;       workval.i >>= 1;         // requires signed shift to preserve sign
;;       workval.i += 0x3f800000; // rebias the new exponent
;;       workval.i &= 0x7FFFFFFF; // remove sign bit
;;       return workval.f;
;; #else
;;       return sqrtf( val ); 
;; #endif
;; }

;; inline float IvInvSqrt( float val ) 
;; { 
;; #if defined(IV_APPROXIMATION)
;;     float valhalf = 0.5f*val;
;;     IntOrFloat workval;
;;     workval.f = val;
;;     workval.i = 0x5f375a86 - (workval.i>>1); // initial guess y0 with magic number
;;     workval.f = workval.f*(1.5f-valhalf*workval.f*workval.f);  // Newton step, increases accuracy
;;     return workval.f;
;; #else
;;     return 1.0f/sqrtf( val );
;; #endif
;; } // InvSqrt

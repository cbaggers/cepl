(in-package :base)

(defconstant +float-threshold+ 1.0e-6)
(defconstant +float-threshold-sq+ (expt 1.0e-6 2))
(defconstant +pi+ 3.1415926535897932384626433832795)

;----------------------------------------------------------------

;; Returns t if float is essentially zero
;; This is handle the fact that floats get
;; less accurate the closer they get to zero
(declaim (inline float-zero)
	 (ftype (function ((single-float)) 
			  (boolean)) 
		float-zero))
(defun float-zero (x)
  (declare (single-float x))
  (< x +float-threshold+))

;----------------------------------------------------------------

;; Returns t if float is essentially zero-squared
;; This sounds mental but its used when we need 
;; to check if a distance squared is essentialy 
;; zero
;; [TODO] Maybe this is mental...go check
(declaim (inline float-zero-sq)
	 (ftype (function ((single-float)) 
			  (boolean)) 
		float-zero-sq))
(defun float-zero-sq (x)
  (declare (single-float x))
  (< x +float-threshold-sq+))


(declaim (inline inv-sqrt)
	 (ftype (function ((single-float)) 
			  (single-float)) 
		inv-sqrt))

;----------------------------------------------------------------

;;Come back and implement the fast versions of these two
(defun c-sqrt (x)
  (declare (single-float x))
  (sqrt x))

(defun c-inv-sqrt (x)
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

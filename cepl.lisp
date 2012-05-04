;;;; cepl.lisp

(in-package #:cepl)

;;; "cepl" goes here. Hacks and glory await!

;;; Maths bits, not in the right place but I just need
;;; to start writing.

;;; need a zero, unit-x, unit-y, unit-z, negative of all units
;;; & unit scale (all 1's)

;;; need to inline
;;; we will make destructive and no destructive versions,
;;; looking at some existing code the desctructive versions
;;; end up being comparitively fast to C. I will use nd as local
;;; parlance for non-destructive.
;;; see http://stackoverflow.com/questions/8356494/efficient-vector-operations-of-linear-algebra-in-common-lisp-especially-sbcl for more details

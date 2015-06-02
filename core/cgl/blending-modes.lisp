;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;
(in-package :cgl)

;; Most of the code that uses blend modes will be in other files
;; as it is most needed in map-g and fbos

;; (%gl:blend-func-separate-i draw-buffer-id src-rgb dst-rgb src-alpha dst-alpha)

;; draw-buffer-id
;;   For glBlendFuncSeparatei, specifies the index of the draw buffer for which
;;   to set the blend functions.
;; srcRGB
;;   Specifies how the red, green, and blue blending factors are computed.
;;   The initial value is GL_ONE.
;; dstRGB
;;   Specifies how the red, green, and blue destination blending factors are
;;   computed. The initial value is GL_ZERO.
;; srcAlpha
;;   Specified how the alpha source blending factor is computed. The initial
;;   value is GL_ONE.
;; dstAlpha
;;   Specified how the alpha destination blending factor is computed. The
;;   initial value is GL_ZERO.

;; Despite the apparent precision of the above equations, blending
;; arithmetic is not exactly specified, because blending operates with
;; imprecise integer color values. However, a blend factor that should be
;; equal to 1 is guaranteed not to modify its multiplicand, and a blend
;; factor equal to 0 reduces its multiplicand to 0. For example, when
;; srcRGB​ is GL_SRC_ALPHA​, dstRGB​ is GL_ONE_MINUS_SRC_ALPHA​, and As0 is
;; equal to 1, the equations reduce to simple replacement:

(defparameter *blend-color* (v! 0 0 0 0))

(defun blend-func-namep (keyword)
  (not (null (member keyword '(:zero
                               :one
                               :src-color
                               :one-minus-src-color
                               :dst-color
                               :one-minus-dst-color
                               :src-alpha
                               :one-minus-src-alpha
                               :dst-alpha
                               :one-minus-dst-alpha
                               :constant-color
                               :one-minus-constant-color
                               :constant-alpha
                               :one-minus-constant-alpha
                               :src-alpha-saturate
                               :src1-color
                               :one-minus-src1-color
                               :src1-alpha
                               :one-minus-src1-alpha)))))




;; functions below were written to help me understand the blending process

(defun zero
    (source destination &key (target-rgb t) (blend-color *blend-color*))
  (declare (ignore source destination blend-color))
  (if target-rgb
      (v! 0 0 0)
      0))

(defun one
    (source destination &key (target-rgb t) (blend-color *blend-color*))
  (declare (ignore source destination blend-color))
  (if target-rgb
      (v! 1 1 1)
      1))

(defun src-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:s~ source :xyz))
      (* (v:w (if target-source source destination))
         (v:w source))))

(defun one-minus-src-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v:s~ source :xyz)))
      (* (v:w (if target-source source destination))
         (- 1 (v:w source)))))

(defun dst-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:s~ destination :xyz))
      (* (v:w (if target-source source destination))
         (v:w destination))))

(defun one-minus-dst-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v:s~ destination :xyz)))
      (* (v:w (if target-source source destination))
         (- 1 (v:w destination)))))

(defun src-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v! (v:w source) (v:w source) (v:w source)))
      (* (v:w (if target-source source destination))
         (v:w source))))

(defun one-minus-src-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v! (v:w source) (v:w source) (v:w source))))
      (* (v:w (if target-source source destination))
         (- 1 (v:w source)))))

(defun dst-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v! (v:w destination) (v:w destination) (v:w destination)))
      (* (v:w (if target-source source destination))
         (v:w destination))))

(defun one-minus-dst-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v! (v:w destination) (v:w destination) (v:w destination))))
      (* (v:w (if target-source source destination))
         (v:w destination))))

(defun constant-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:s~ blend-color :xyz))
      (* (v:w (if target-source source destination))
         (v:w blend-color))))

(defun one-minus-constant-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v:s~ blend-color :xyz)))
      (* (v:w (if target-source source destination))
         (- 1 (v:w blend-color)))))

(defun constant-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore ))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v! (v:w blend-color) (v:w blend-color) (v:w blend-color)))
      (* (v:w (if target-source source destination))
         (v:w blend-color))))

(defun one-minus-constant-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v! (v:w blend-color) (v:w blend-color) (v:w blend-color))))
      (* (v:w (if target-source source destination))
         (- 1 (v:w blend-color)))))

;; Destination color multiplied by the minimum of the source and (1 – destination)
(defun src-alpha-saturate
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*))
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (let ((factor (min (v:w source) (- 1 (v:w destination)))))
             (v! factor factor factor)))
      (* (v:w (if target-source source destination))
         1)))

(defun src1-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*) source-2)
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:s~ source-2 :xyz))
      (* (v:w (if target-source source destination))
         (v:w source-2))))

(defun one-minus-src1-color
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*) source-2)
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v:s~ source-2 :xyz)))
      (* (v:w (if target-source source destination))
         (- 1 (v:w source-2)))))

(defun src1-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*) source-2)
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v! (v:w source-2) (v:w source-2) (v:w source-2)))
      (* (v:w (if target-source source destination))
         (v:w source-2))))

(defun one-minus-src1-alpha
    (source destination &key (target-rgb t) (target-source t)
                          (blend-color *blend-color*) source-2)
  (declare (ignore blend-color))
  (if target-rgb
      (v:* (v:s~ (if target-source source destination) :xyz)
           (v:- (v! 1 1 1) (v! (v:w source-2) (v:w source-2) (v:w source-2))))
      (* (v:w (if target-source source destination))
         (- 1 (v:w source-2)))))

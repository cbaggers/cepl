(in-package :cepl.context)

;;------------------------------------------------------------
;; Cull Face

(define-context-func cull-face () (or symbol function)
    (cull-face)
  cull-face)

(define-context-func (setf cull-face) ((face symbol)) symbol
    (cull-face)
  (assert (member face '(nil :front :back :front-and-back)))
  (if face
      (progn
        (gl:enable :cull-face)
        (%gl:cull-face (gl-enum face)))
      (gl:disable :cull-face))
  (setf cull-face face))

;;------------------------------------------------------------
;; Front Face

(define-context-func front-face () symbol
    (front-face)
  front-face)

(define-context-func (setf front-face) ((winding-direction symbol))
    symbol
    (front-face)
  (assert (or (eq winding-direction :ccw)
              (eq winding-direction :cw)))
  (%gl:front-face (gl-enum winding-direction))
  (setf front-face winding-direction))

;;------------------------------------------------------------

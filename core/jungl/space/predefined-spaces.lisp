(in-package :jungl.space)

;; ;; here we define the common spaces

(defvar *ndc-space*
  (make-relational-space nil))

(defvar *clip-space*
  (make-relational-space `((,*ndc-space*))))


;; technically anyone can make a world space. But for optimization
;; jungl wants to own this one.
(defvar *world-space*
  (make-relational-space nil))

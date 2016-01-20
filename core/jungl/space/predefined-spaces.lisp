(in-package :jungl.space)

;; here we define the common spaces

(defvar *ndc-space*
  (make-space (m4:identity)))

(defvar *clip-space*
  (make-space (m4:identity)))


;; technically anyone can make a world space. But for optimization
;; jungl wants to own this one.
(defvar *world-space*
  (make-space (m4:identity)))

;; will be replaced
(defvar *model-space* (space! (m4:identity)))

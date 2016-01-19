(in-package :jungl)

;; here we define the common spaces

(defvar *ndc-space*
  (space::make-space (m4:identity)))

(defvar *clip-space*
  (space::make-space (m4:identity)))


;; technically anyone can make a world space. But for optimization
;; jungl wants to own this one.
(defvar *world-space*
  (space::make-space (m4:identity)))

;; will be replaced
(defvar *model-space* (space::space! (m4:identity)))

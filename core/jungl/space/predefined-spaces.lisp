(in-package :jungl.space)

;; ;; here we define the common spaces

(defvar *screen-space*
  (make-relational-space nil))

(defvar *ndc-space*
  (make-relational-space `((,*screen-space*))))

(defvar *clip-space*
  (make-relational-space `((,*ndc-space*))))

;; this defines a link between world and clip space.
;;
;; usually you will define your own eye spaces and then use
;; #'get-transform-via to force the routing through your camera.
;;
;; It may seem annoying to have this but identity is a valid transform
;; and it means we have a valid space graph.
(defvar *identity-eye-space*
  (make-relational-space `((,*clip-space*))))

;; technically anyone can make a world space. But for optimization
;; jungl wants to own this one.
(defvar *world-space*
  (make-relational-space `((,*identity-eye-space*))))

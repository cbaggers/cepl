(in-package :cepl-backend)

(defvar *backend* nil)

;; ultimately need to be able to support more backends than sdl
;; first candidate for this is glop.

;; Very little here as the seperation in cepl isnt well defined yet
;; this is one of goals for getting to beta

(defgeneric init (backend-name))
(defgeneric start (backend-name width height title fullscreen
                  no-frame alpha-size depth-size stencil-size
                  red-size green-size blue-size buffer-size
                  double-buffer hidden resizable))
(defgeneric shutdown (backend-name))
(defgeneric get-event-pump (backend-name))
(defgeneric get-swap-func (backend-name))

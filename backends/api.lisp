(in-package :cepl-backend)

(defvar *backend* nil)

;; This is what the backend has to implement
(defgeneric init (backend-name))
(defgeneric start (backend-name width height title fullscreen
                  no-frame alpha-size depth-size stencil-size
                  red-size green-size blue-size buffer-size
                  double-buffer hidden resizable))
(defgeneric shutdown (backend-name))
(defgeneric get-step-func (backend-name))
(defgeneric get-swap-func (backend-name))

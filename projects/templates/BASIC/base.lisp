(in-package :(#| TMPL_VAR NAME |#))
(named-readtables:in-readtable fn_:fn_lambda)

;;--------------------------------------------------------------
;; draw

(defun draw ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:flush)
  (cgl:update-display))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions cgl:+default-resolution+))
  (apply #'gl:viewport 0 0 new-dimensions))

(evt:observe (evt.sdl:|window|)
  (when (eq (evt.sdl:action e) :resized)
    (reshape (evt.sdl:vec e))))

;;--------------------------------------------------------------
;; controls

;; (evt:observe (evt.sdl:*mouse*)
;;   )

;; (evt:observe (evt.sdl:*keyboard*)
;;   )

;;--------------------------------------------------------------
;; step

(defun step-demo ()
  (update-swank)
  (evt.sdl:pump-events)
  (draw))

;;--------------------------------------------------------------
;; main loop

(defvar *running* nil)

(defun run-demo ()
  (init)
  (setf *running* t)
  (print "-starting up-")
  (loop :while *running* :do (continuable (step-demo)))
  (print "-shutting down-")
  nil)

(defun stop-demo () (setf *running* nil))

(evt:observe (evt.sdl:|sys|)
  (when (typep e 'evt.sdl:will-quit)
    (stop-demo)))


;;--------------------------------------------------------------
;; setup

(defun init ()
  )

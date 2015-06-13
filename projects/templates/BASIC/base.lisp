(in-package :(#| TMPL_VAR NAME |#))

;;--------------------------------------------------------------
;; draw

(defun draw ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:flush)
  (cgl:update-display))

;;--------------------------------------------------------------
;; window

(defun reshape (new-dimensions)
  )

(evt:observe (e evt:|window|)
  (when (eq (evt:action e) :resized)
    (reshape (evt:vec e))))

;;--------------------------------------------------------------
;; controls

;; (evt:observe (e evt:*mouse*)
;;   )

;; (evt:observe (e evt:*keyboard*)
;;   )

;;--------------------------------------------------------------
;; step

(defun step-demo ()
  (update-swank)
  (evt:pump-events)
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

(evt:observe (e evt:|sys|)
  (when (typep e 'evt:will-quit)
    (stop-demo)))


;;--------------------------------------------------------------
;; setup

(defun init ()
  )

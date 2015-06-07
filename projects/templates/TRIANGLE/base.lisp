(in-package :(#| TMPL_VAR NAME |#))

;;--------------------------------------------------------------
;; draw

(defpipeline prog-1 ((vert g-pc))
  (:vertex (setf gl-position (v! (cgl:pos vert) 1.0))
           (out (the-color :smooth) (cgl:col vert)))
  (:fragment (out outputColor the-color)))

(defun draw ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'prog-1 *stream*)
  (gl:flush)
  (cgl:update-display))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions *current-viewport*))
  (setf (viewport-resolution (viewport *gl-context*))
        new-dimensions))

(evt:observe (evt:|window|)
  (when (eq (evt:action e) :resized)
    (reshape (evt:vec e))))

;;--------------------------------------------------------------
;; controls

;; (evt:observe (evt:*mouse*)
;;   )

;; (evt:observe (evt:*keyboard*)
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

(evt:observe (evt:|sys|)
  (when (typep e 'evt:will-quit)
    (stop-demo)))


;;--------------------------------------------------------------
;; setup

(defvar *array* nil)
(defvar *stream* nil)

(defun init ()
  (setf *array* (make-gpu-array
                 (list (list (v!  0.5 -0.366 0.0) (v! 0.0 1.0 0.0 1.0))
                       (list (v!  0.0    0.5 0.0) (v! 1.0 0.0 0.0 1.0))
                       (list (v! -0.5 -0.366 0.0) (v! 0.0 0.0 1.0 1.0)))
                 :element-type 'g-pc))
  (setf *stream* (make-buffer-stream *array*)))

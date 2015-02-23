(in-package :(#| TMPL_VAR NAME |#))
(named-readtables:in-readtable fn_:fn_lambda)

;;--------------------------------------------------------------
;; draw

(defpipeline prog-1 ((vert g-pc))
  (:vertex (setf gl-position (v! (cgl:pos vert) 1.0))
           (out (the-color :smooth) (cgl:col vert)))
  (:fragment (out outputColor the-color)))

(defun draw ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gmap #'prog-1 *stream*)
  (gl:flush)
  (cgl:update-display))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions cgl:+default-resolution+))
  (apply #'gl:viewport 0 0 new-dimensions))

(evt:observe (evt.sdl::*window*)
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

(evt:observe (evt.sdl::*sys*)
  (when (typep e 'evt.sdl:will-quit)
    (stop-demo)))


;;--------------------------------------------------------------
;; setup

(defparameter *array* nil)
(defparameter *stream* nil)

(defun init ()
  (setf *array* (make-gpu-array
                 (list (list (v!  0.5 -0.366 0.0) (v! 0.0 1.0 0.0 1.0))
                       (list (v!  0.0    0.5 0.0) (v! 1.0 0.0 0.0 1.0))
                       (list (v! -0.5 -0.366 0.0) (v! 0.0 0.0 1.0 1.0)))
                 :element-type 'g-pc))
  (setf *stream* (make-vertex-stream *array*)))

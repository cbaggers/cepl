(in-package :cepl-backend)

;; ultimately need to be able to support more backends than sdl
;; first candidate for this is glop.

;; Very little here as the seperation in cepl isnt well defined yet
;; this is one of goals for getting to beta

(defgeneric init (backend-name width height title fullscreen
                  no-frame alpha-size depth-size stencil-size
                  red-size green-size blue-size buffer-size
                  double-buffer hidden resizable))
(defgeneric shutdown (backend-name))

;; #+sb-thread
;; (defmacro on-main (&body b)
;;     `(let ((thread (first (last (sb-thread:list-all-threads)))))
;;        (sb-thread:interrupt-thread thread
;; 				   #'(lambda () (sb-int:with-float-traps-masked (:underflow :overflow :inexact :invalid :divide-by-zero),@b)))))

;; #+ccl
;; (defmacro on-main (&body b)
;;   `(let ((thread (find 0 (all-processes) :key #'process-serial-number)))
;;      (process-interrupt thread (lambda () ,@b))))

;; #+(and ccl darwin)
;;              (sdl2:in-main-thread ()
;;                (%repl width height))

(defmethod init ((backend-name (eql :sdl))
                 width height title fullscreen
                 no-frame alpha-size depth-size stencil-size
                 red-size green-size blue-size buffer-size
                 double-buffer hidden resizable)
  #+(and ccl darwin)
  (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
  (unless (sdl2:init) (error "Failed to initialise SDL"))
  "Initializes the backend and returns a list containing: (context window)"
    (let* ((win (sdl2:create-window
               :title title :w width :h height
               :flags (remove nil `(:shown :opengl
                                           ,(when fullscreen :fullscreen-desktop)
                                           ,(when resizable :resizable)
                                           ,(when no-frame :borderless)
                                           ,(when hidden :hidden))))))
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 2)
    (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)
    (let ((gl-context (sdl2:gl-create-context win)))
      (sdl2:gl-make-current win gl-context)
      ;; request version, this should fix issue on osx
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 2)
      (sdl2:gl-set-attr :context-profile-mask 1)
      (sdl2:gl-set-attr :alpha-size alpha-size)
      (sdl2:gl-set-attr :depth-size depth-size)
      (sdl2:gl-set-attr :stencil-size stencil-size)
      (sdl2:gl-set-attr :red-size red-size)
      (sdl2:gl-set-attr :green-size green-size)
      (sdl2:gl-set-attr :blue-size blue-size)
      (sdl2:gl-set-attr :buffer-size buffer-size)
      (sdl2:gl-set-attr :doublebuffer (if double-buffer 1 0))
      (list gl-context win))))

(defmethod shutdown ((backend-name (eql :sdl)))
  (sdl2:quit))

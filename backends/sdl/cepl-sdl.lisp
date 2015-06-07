(in-package :cepl-backend)

(defvar *initd* nil)

(defmethod cepl-backend:init ((backend-name (eql :sdl)))
  (setf *backend* :sdl)
  (unless *initd*
    (unless (sdl2:init :everything) (error "Failed to initialise SDL"))
    (setf *initd* t)))

(defmethod cepl-backend:start ((backend-name (eql :sdl))
                               width height title fullscreen
                               no-frame alpha-size depth-size stencil-size
                               red-size green-size blue-size buffer-size
                               double-buffer hidden resizable)
  "Initializes the backend and returns a list containing: (context window)"
  (setf *backend* :sdl)
  (print -1)
  (let ((win (sdl2:create-window
              :title title :w width :h height
              :flags (remove nil `(:shown :opengl
                                          ,(when fullscreen :fullscreen-desktop)
                                          ,(when resizable :resizable)
                                          ,(when no-frame :borderless)
                                          ,(when hidden :hidden))))))
    #+darwin
    (progn
      (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 2)
      (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+))
    (sdl2:gl-set-attr :context-profile-mask 1)
    (sdl2:gl-set-attr :alpha-size alpha-size)
    (sdl2:gl-set-attr :depth-size depth-size)
    (sdl2:gl-set-attr :stencil-size stencil-size)
    (sdl2:gl-set-attr :red-size red-size)
    (sdl2:gl-set-attr :green-size green-size)
    (sdl2:gl-set-attr :blue-size blue-size)
    (sdl2:gl-set-attr :buffer-size buffer-size)
    (sdl2:gl-set-attr :doublebuffer (if double-buffer 1 0))
    (let ((contex (sdl2:gl-create-context win)))
      (sdl2:gl-make-current win contex)
      (list contex win))))

(defmethod cepl-backend:shutdown ((backend-name (eql :sdl)))
  (sdl2:quit))

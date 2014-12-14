(in-package :cepl)

;;helpers

(defun new-window (&key (width 640) (height 480) (title "") fullscreen
                     no-frame (alpha-size 0) (depth-size 16) (stencil-size 8) 
                     (red-size 8) (green-size 8) (blue-size 8) (buffer-size 32)
                     (double-buffer t) hidden (resizable t))
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
      (values gl-context win))))

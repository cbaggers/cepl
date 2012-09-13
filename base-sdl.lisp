;; Here is the code that is used to manage SDL
;; Generally these are tweaks upon lispbuilder-sdl
;; The initializing stuff is VERY subject to change as 
;; I havent really learnt that much about SDL yet

(in-package :base-sdl)

(defun init-sdl (&key (width 640) (height 480) 
                   (resizable t)
                   (fullscreen nil) 
                   (title "cepl window")
                   (alpha-size 0) (depth-size 16)
                   (stencil-size 8) (red-size 8)
                   (green-size 8) (blue-size 8))
  (if (sdl:init-sdl :flags nil)
      (progn      
        (sdl:window width height
                    :opengl t
                    :resizable resizable
                    :fullscreen fullscreen
                    :double-buffer t
                    :icon-caption title
                    :title-caption title
                    :hw t
                    :opengl-attributes 
                    `((:sdl-gl-doublebuffer 1)
                      (:sdl-gl-alpha-size
                       ,alpha-size)
                      (:sdl-gl-depth-size
                       ,depth-size)
                      (:sdl-gl-stencil-size
                       ,stencil-size)
                      (:sdl-gl-red-size ,red-size)
                      (:sdl-gl-green-size
                       ,green-size)
                      (:sdl-gl-blue-size ,blue-size)
                      (:SDL-GL-SWAP-CONTROL 1)))
        (setf cl-opengl-bindings:*gl-get-proc-address* 
              #'sdl-cffi::sdl-gl-get-proc-address)
        t)
      (error "Failed to initialise SDL")))

;; [TODO] Make a macro that wraps init-sdl and this in the same
;;        lexical scope so that 'flags' are shared
(defun quit-sdl (&optional flags)
  (sdl:close-audio)
  (sdl:quit-sdl :flags flags))


(defmacro with-init-sdl ((&key (width 640) (height 480) 
                               (resizable t)
                               (fullscreen nil) 
                               (title "cepl window")
                               (alpha-size 0) (depth-size 16)
                               (stencil-size 8) (red-size 8)
                               (green-size 8) (blue-size 8)) 
                         &body body)
  "This initializes SDL with the assumption that you are writing
a modern shader-based opengl program.
To this end it makes some opionated choices (like that your
program will be double buffered) and makes a reduced set
of options available."
  `(block nil
     (unwind-protect
          (when (init-sdl :width ,width :height ,height
                          :resizable ,resizable 
                          :fullscreen ,fullscreen
                          :title ,title :alpha-size ,alpha-size
                          :depth-size ,depth-size 
                          :stencil-size ,stencil-size
                          :red-size ,red-size
                          :green-size ,green-size
                          :blue-size ,blue-size)
            ,@body)
       (quit-sdl))))

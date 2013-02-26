;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is here if the user of this library wishes to use 
;; it with lispbuilder rather than lispbuilder-mini.
;; lispbuilder is brilliant and so this is just to smooth 
;; the patches where there are differences.

(in-package :lbm-sdl)

(defun init-sdl (&key (width 640) (height 480) 
                   (resizable t)
                   (fullscreen nil) 
                   (title "cepl window")
                   (alpha-size 0) (depth-size 16)
                   (stencil-size 8) (red-size 8)
                   (green-size 8) (blue-size 8))
  (if (sdl:init-sdl)
      (progn      
        (sdl:window width height
                    :opengl t
                    :resizable resizable
                    :fullscreen fullscreen
                    :icon-caption title
                    :title-caption title
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
                      (:sdl-gl-swap-control 1)))
        (setf cl-opengl-bindings:*gl-get-proc-address* 
              #'sdl-cffi::sdl-gl-get-proc-address)
        t)
      (error "Failed to initialise SDL")))

;; [TODO] Make a macro that wraps init-sdl and this in the same
;;        lexical scope so that 'flags' are shared
(defun quit-sdl (&optional flags)
  (sdl:close-audio)
  (sdl:quit-sdl :flags flags))


(defun collect-sdl-event-types ()
  (let ((x (sdl:new-event)))
    (loop until (= 0 (lbm-sdl-cffi::sdl-poll-event x))
	  collect (sdl:event-type x))
    (sdl:free-event x)))

(defun get-sdl-event (&optional event)
  (let ((event (or event (sdl:new-event))))
    (if (= (sdl-cffi::SDL-Poll-Event event) 0)
        (progn (sdl:free-event event)
               nil)
        event)))

(defmacro case-events ((event-var) &body cases)
  (if (symbolp event-var)
      `(let ((,event-var (sdl:new-event)))
         (loop :while (not (eq (sdl-cffi::SDL-Poll-Event 
                                ,event-var) 0))
            :do (case (sdl:event-type ,event-var)
                  ,@cases))
         (sdl:free-event ,event-var))
      (error "event-var must be a symbol")))

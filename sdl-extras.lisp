(in-package :sdl2)

;;helpers

(defun new-window (&key (width 640) (height 480) (title "") fullscreen
                     (resizable t) no-frame (alpha-size 0) 
                     (depth-size 16) (stencil-size 8) (red-size 8)
                     (green-size 8) (blue-size 8) (buffer-size 32)
                     (double-buffer t) hidden)
  (let* ((win (sdl2:create-window 
               :title title :w width :h height 
               :flags (remove nil `(:shown :opengl
                                           ,(when fullscreen :fullscreen-desktop)
                                           ,(when resizable :resizable)
                                           ,(when no-frame :borderless)
                                           ,(when hidden :hidden)))))
         (gl-context (sdl2:gl-create-context win)))
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
    (values gl-context win)))

(defmacro case-events ((event-var) &body event-handlers)
  (if (symbolp event-var)
      `(let ((,event-var (sdl2:new-event)))
         (loop :while (not (eq (sdl2::sdl-poll-event ,event-var) 0))
            :do (case (sdl2::get-event-type ,event-var)
                  ,@(remove nil
                            (mapcar
                             #'(lambda (handler)
                                 (let* ((event-type (first handler))
                                        (params (second handler))
                                        (forms (rest (rest handler))))
                                   (expand-handler event-var
                                                   event-type
                                                   params
                                                   forms)))
                                  event-handlers))))
         (sdl2:free-event ,event-var))
      (error "event-var must be a symbol")))

(defmacro evt-> (event type param)
  "Lets you write following is access event details:
   \(evt-> event :windowevent :data1\)"
  `(,@(cadar (unpack-event-params (utils:kwd event) (utils:kwd) 
                                  type `((,param jeff))))))

(defun collect-event-types ()
  (let* ((x (sdl2:new-event))
         (event-types (loop :until (= 0 (sdl2::sdl-poll-event x))
                         :collect (sdl2::get-event-type x))))
    (sdl2:free-event x)
    event-types))

(export '(make-window case-events collect-event-types evt->) :sdl2)

(in-package :sdl2)

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

(defmacro case-events ((event-sym &optional event-obj) &body event-handlers)
  (if (symbolp event-sym)
      `(let ((,event-sym (or ,event-obj (sdl2:new-event))))
         (loop :while (not (eq (sdl2::sdl-poll-event ,event-sym) 0)) :do
            (case (sdl2::get-event-type ,event-sym)
              ,@(loop :for (type params . forms) :in event-handlers :collect
                   (expand-handler event-sym type params forms) :into results
                   :finally (return (remove nil results)))))
         (sdl2:free-event ,event-sym))
      (error "event-sym must be a symbol")))

(defun expand-handler2 (sdl-event event-type params forms)
  (let ((parameter-pairs nil))
    (do ((keyword params (if (cdr keyword) (cddr keyword) nil)))
        ((null keyword))
      (push (list (first keyword) (second keyword)) parameter-pairs))
    (if (listp event-type)
        (let ((constant-symb (utils:symbolicate-package 
                              :sdl2-ffi '+SDL- (first event-type) '- 
                              (second event-type) '+)))
          (unless (boundp constant-symb) 
            (error "constant ~s doesnt exist" constant-symb))
          `(,(first event-type)
             (when (eql (,@(cadar (unpack-event-params
                                   sdl-event (first event-type) `((:type j)))))
                        ,constant-symb)
               (let (,@(unpack-event-params sdl-event (first event-type)
                                            parameter-pairs))
                 ,@forms))))
        `(,event-type          
          (let (,@(unpack-event-params sdl-event event-type parameter-pairs))
            ,@forms)))))

(defmacro evt-> (event type param)
  "Lets you write following is access event details:
   \(evt-> event :windowevent :data1\)"
  `(,@(cadar (unpack-event-params event (utils:kwd type) `((,param jeff))))))

(defun collect-event-types ()
  (let* ((x (sdl2:new-event))
         (event-types (loop :until (= 0 (sdl2::sdl-poll-event x))
                         :collect (sdl2::get-event-type x))))
    (sdl2:free-event x)
    event-types))

(export '(make-window case-events collect-event-types evt->) :sdl2)

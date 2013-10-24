;; Main loop and SDL2
;; =================
;; Lispbuilder-sdl2 provides some great tools for managing 
;; your games but a couple seem to overeach a little.
;; The most obvious is with-events which in their words:
;;     `WITH-EVENTS` is a convenience macro for managing the
;;      main game loop. It processes incoming SDL2 events and
;;      limits the game loop to the specified number of 
;;      frames per second.
;;
;; IT works, and works well, but the instant you have a 
;; thought like 'how do I control how often things in my 
;; main loop happen' you have to start digging into how the
;; macro works. As great as it is in examples it actually
;; causes more long term confusion as you have to pick apart
;; what it does before you can control it. 
;; At least this was my experience! take with a mighty handful
;; of salt!

;; Here is an example main loop from one of the example files
(defun run-demo () 
  (init)
  (reshape 1024 768)
  (sdl2:with-events () 
    (:quit-event () t)
    (:video-resize-event (:w width :h height) 
                         (reshape width height))
    (:idle () (cepl-utils:update-swank)
           (base-macros:continuable (draw)))))
;; It's VERY tidy but if I want to say have my AI code run
;; once a second, my rendering 60 times a second but still 
;; check my events every loop then this doesnt obviously
;; show how to do it.

;; Here is the equivilent using pure lisp.
;; (using the iterate macro)
(defun run-demo () 
  (init)
  (reshape 1024 768)
  (iter :main-loop
        (while running)
        (iter 
          (let ((event (get-sdl2-event)))
            (if event
                (progn
                  (case (sdl2:event-type event)
                    (:quit-event (in :main-loop (finish)))
                    (:video-resize-event 
                     (reshape (sdl2:video-resize-w event)
                              (sdl2:video-resize-h event))))
                  (sdl2:free-event event))
                (finish))))
        (sdl2:process-audio)
        (cepl-utils:update-swank)
        (base-macros:continuable (draw))))
;; Ok so this one is much busier but you can see where we loop
;; through and get the events. We use well know common lisp 
;; tools for iteration (this could have easily used the loop 
;; macro) so we know their behaviour imediately. 
;; The only bit that sucks is the event loop is a little untidy
;; this is a good candiate for a macro and something that 
;; lispbuilder-sdl2's with-events did REALLY well.
;; So we will add a little code tidying macro called case-events

(defun run-demo () 
  (init)
  (reshape 1024 768)  
  (let ((running t))
    (loop :while running :do
       (case-events (event)
         (:quit-event (setf running nil))
         (:video-resize-event 
          (reshape (sdl2:video-resize-w event)
                   (sdl2:video-resize-h event))))
       (cepl-utils:update-swank)
       (continuable (draw)))))

;; Here we have use the normal loop for iteration.
;; We have a very familiar case syntax for case-events.
;; We can easily put our own controls around the draw and other
;; update routines without having to fundamentally change the 
;; nature of the loop. In my mind it is a little more 
;; composable.



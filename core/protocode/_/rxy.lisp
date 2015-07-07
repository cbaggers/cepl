
;; ok
(defun node (args dead)
  ())

;; this would mean that almost all these funcs would start with (if dead ...)
;; so subscribe should allow one or two funcs

(defun subscribe (callf stream-endingf)
  ...)

(rx:chain event-in (filter (key-events character-control)
                           (mouse-events cursor-control)))

;; some seems to be nodes and some look like function calls.. hmm

;; recompiling this can mean state changes, so state is kept in a packet
;; and we need a way to expose this.

(defun surrender-state (node)
  ...)

(defun incorporate-state (node state)
  ...)

;; 


(rxy:-> #'tick (lambda (x) (print "hi tick")))
(rxy:-> #'tick #'hi-tick)
(rxy:-> tick hi-tick)
(rxy:-> tick (split hi-tick
                    bye-tick))

(rxy:-> tick (split (upcase-tick hi-tick)
                    (downcase-tick bye-tick)))

(rxy:-> tick (split (cache-5 print-batched)
                    hi-tick))
;; cache-5 has state, that must be maintained
;; stateful nodes could be funcallable-objects
;; that makes sense actually

(defclass stateful-rxy-node () ()
  (state)
  :mop funcallable-object)
(defmethod surrender-state ((node stateful-rxy-node))
  ...)
(defmethod incorporate-state ((node stateful-rxy-node))
  ...)

;; hmm should the tick be i nthe form, or should it call the form?
(rxy:-> tick (split (cache-5 print-batched)
                    hi-tick))
;; pretty much has to be the latter, otherwise how do you know
;; when tick is called?
(tlambda () (every (seconds 1) (chain 'tick)))
(defrxy chain (split (cache-5 print-batched)
                     hi-tick))



;; given that this could be a legitimate main loop

(defdemo test :step #'step-demo :swank-update-sec 0.3)

(defun step-demo ()
  (sdl2:pump-events-into #'sdl-input)
  (update-entities)
  (render-passes))

(defrxy sdl-input
    (rxy:tcase (kbd-event keymap-transform
                          (rxy:tcase (character-event character-input)
                                     (interface-event interface-input)))
               (mouse-event mouse-rotate-cam)))

(defvar *rxy-steps* (make-hash-table :test #'eq :size 100))
(defmacro defrxy (name &rest steps)
  (if (hash-table-has-keyp *rxy-steps* name)
      (recompile-rxy name steps)
      (compile-rxy name steps)))

(defun compile-rxy (name steps)
  )

(defun recompile-rxy (name steps)
  )

(defun hash-table-has-keyp (hash-table key)
  (second (multiple-value-list (gethash key hash-table))))

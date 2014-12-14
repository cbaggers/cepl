(in-package :cepl.events)

;; direct sdl helpers

(defmacro case-events ((event &key (method :poll) (timeout nil))
                       &body event-handlers)
  `(let (,(when (symbolp event) `(,event (sdl2:new-event))))
     (loop :until (= 0  (sdl2:next-event ,event ,method ,timeout)) :do
        (case (sdl2::get-event-type ,event)
          ,@(loop :for (type params . forms) :in event-handlers :collect
               (sdl2::expand-handler event type params forms) :into results
               :finally (return (remove nil results)))))
     (sdl2:free-event ,event)))

(defun collect-event-types ()
  (let* ((x (sdl2:new-event))
         (event-types (loop :until (= 0 (sdl2::sdl-poll-event x))
                         :collect (sdl2::get-event-type x))))
    (sdl2:free-event x)
    event-types))

(defun map-sdl-events (function &rest more-functions)
  (let ((event (sdl2:new-event)))
    (loop :until (= 0 (sdl2:next-event event :poll nil)) :do
       (funcall function event)
       (dolist (f more-functions) (funcall f event)))
    (sdl2:free-event event)))


;; basic stuff 

;; (defun def-e-> (name &rest nodes)
;;   )

;; (defun e-map (function event)
;;   (funcall function event))

;; (defun e-tcase (bindings event)
;;   (funcall (cadr (assoc (event-type event) bindings))
;;            event))

;; (def-e-> thrash-events
;;     (e-map #'sdl->cepl-evt) (e-tcase ((:keyboard-event filter-combos player-kb-input)
;;                                       (:mouse-event player-mouse-input))))

;; (map-sdl-events #'thrash-events)

;; ;; looks nice but mandates special parsing, also has abiguous syntax

;; (def-e-> thrash-events sdl-cepl-evt split-on-evt-type)

;; ;; see how this means the rest of the data flow has to be described 
;; ;; in split-on-evt-type?

;; ;; so this has to be seen a constructor signatures

;; (def-e-> thrash-events
;;     (e-map #'sdl->cepl-evt) (e-tcase :keyboard-event (filter-combos player-kb-input)
;;                                      :mouse-event (player-mouse-input)))

;; ;; ok so you can have keyword args, and what follows has to be a tree, so 
;; ;; intenally e-tcase decides what maps to :keyboard-event

;; (defun e-tcase (event keyboard-event mouse-event)
;;   (case (get-event-type event)
;;     (:kbd (funcall keyboard-event))
;;     (:mouse (funcall mouse-event))))

;; (defclass enode () (child))
;; (defmacro e! (etype &rest keys) 
;;   `(make-instance ',etype ,@keys))
;; (defmethod e-cons-end (node-a node-b)
;;   (setf (slot-value node-a 'child) node-b))
;; (defun e-list (&rest nodes)
;;   (let ((last (first nodes)))
;;     (loop for n in (rest nodes) do 
;;          (setf (slot-value last 'child) n
;;                last n))))


;; (defclass emap (enode) ((func :initarg :func)))
;; (defclass eid (enode) ())

;; (defmethod epush (evt (node eid))
;;   (epush (slot-value node 'child) evt))

;; (defmethod epush (evt (node emap))
;;   (with-slots (func child) node
;;     (epush (funcall func evt) child)))


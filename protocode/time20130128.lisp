
(let ((count 0))
  (tlambda* () 
    (t (incf count))
    ((each (seconds 1)) (print count) (setf count 0) (force-output))))

;; actually if (not (listp (first root-form))) then can we just assume 't'?

;; :then :repeat (sequential is default)
(let ((count 0))
  (tlambda* ()
    (repeat ((before ((from-now (seconds 3)) :progress p))
             (setf (pos *entity*) (lerp (pos *entity*) (v! 0 0 -30))))
            ((before ((from-now (seconds 3)) :progress p))
             (setf (pos *entity*) (lerp (pos *entity*) (v! 0 0 -5)))))
    (t (incf count))))

;; oh wow this is nuts but could be cool, it will have to fully support loop
;; though! This would make writing main-loops super familiar.
(tloop :before (from-now (seconds 10)) :do (print "oh my"))

(tloop :before (from-now (seconds 10)) :each (seconds 1)
  :do (print "oh my"))

(tloop :while running
       :each (seconds 1) :do (update-fps)
       :each (seconds 0.5) :do (update-swank)
       :do (update-game-logic))

(tloop :for i :below 10 :step-time (seconds 0.3) :collect i) ;;weird!


;; the most annoying thing about main loops is not being able to change them
(tloop :name jam :before (from-now (seconds 10)) :do (print "oh my"))

(progn (defun jam () )) ;; an imediately you see the problem, if we use a 
                        ;; function then we have to capture vars or pass
                        ;; them in. We cant do this so this is impossible
                        ;; to implement...boo




;;from example 1.lisp
(tloop :while running 
       :each (seconds 0.5) :do (update-swank)
       :do (continuable (gl:clear :color-buffer-bit)
                        (case-events (event)
                          (:quit () (setf running nil)))
                        (prog-1 *stream*)
                        (gl:flush)
                        (cgl:update-display)))

(tloop :while running :catching 'error
       :each (seconds 0.5) :do (update-swank)
       :each 1 :do
       (gl:clear :color-buffer-bit)
       (case-events (event)
         (:quit () (setf running nil)))
       (prog-1 *stream*)
       (gl:flush)
       (cgl:update-display))

;; The :do following :do syntax is actually perfectly inline with the standard
;; loop macro, for example:
(loop :for i :below 10
   :if (oddp i) :do (format t "~a~%" i)
   :do (format t "anyway ~s~%" i))


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


;; hmm need to knwo format of tlambda*
(let ((count 0))
  (tlambda* ()
    (sequential (t (incf count))
                (then ((before (from-now (seconds 10))) (print "hi"))
                      ((each (seconds 1)) (print count) (setf count 0) (force-output))
                      (repeat ((before (from-now (seconds 20))) (print "hi2"))
                              ((before (from-now (seconds 30))) (print "hi3")))))))

So if the first of form is a list or t then it is a test-pair-block
else if it is symbol then it must be handled by that thing...this explanatin is bad 

;; Ok more ideas!
;; each tlambda should shadow #'signal-expired so that the use can use it in their code
;; to break out of the temporal block.
;; I fucking love lisp

(tlambda () 1)
(tlambda () ((lambda (x) x) 1))
(tlambda () (+ 1 2))

(tlambda () ((before (from-now (seconds 10))) (print "hi")))

(tlambda () 
  ((before (from-now (seconds 20))) (print "hi"))
  ((before (from-now (seconds 10))) (print "there")))

(tlambda () 
  (then ((before (from-now (seconds 20))) (print "hi"))
        ((before (from-now (seconds 10))) (print "there"))))

(tlambda () 
  (repeat ((before (from-now (seconds 20))) (print "hi"))
          ((before (from-now (seconds 10))) (print "there"))))

(tlambda () 
  ((each (seconds 3)) (print "3 seconds have passed")))

(tdefun step-demo ()
  ((each (seconds 1)) (update-ai))
  ((each (seconds (/ 1 60)) (render-scene)))
  (step-physics))


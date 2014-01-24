(in-package :base-time)

(loop :until (after (from-now 3000)) :finally (print "Hi")) ;; doesnt work

(let ((deadline (from-now (seconds 5))))
  (loop :until (afterp deadline) :finally (print "hi"))) ;; works

(t-manage (t-every (seconds 1) (print 'still-running)))

(t-manage (cfn () (before (from-now (seconds 3))) (t-every 300 (print 'lots)))) ;;nope

(let ((deadline (from-now (seconds 3))))
  (cfn () (before deadline) (t-every 300 (print 'allo)))) ;; nope

;;ok so we really dont have composition done yet

;; lets try using the follwing and then do some tests:
(defun compose (&rest funcs)
  (cond ((null funcs) #'identity)
        ((eql (length funcs) 1) (first funcs))
        (t (lambda (&rest args)
             (loop :for f :in funcs :do
                (setf args (list (apply f args))))
             (values-list args)))))


;; well I had to implment a mini time language as this was the only way to get 
;; the syntax this needed.
;; The result will loop until the tlambda has expired
(let ((f (tlambda () (and (before (from-now (seconds 5))) (each (seconds 0.4)))
               (print 'weee) (force-output))))
      (loop :until (expiredp (funcall f))))

(t-manage (tlambda () (and (before (from-now (seconds 5))) (each (seconds 0.4)))
            (print 'weee) (force-output)))

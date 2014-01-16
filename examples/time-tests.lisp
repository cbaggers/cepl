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


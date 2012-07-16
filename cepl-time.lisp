;; Functions and macros for handling time in games

(defun make-time-buffer ()
  (let ((last-time (get-internal-real-time)))
    (lambda () 
      (let* ((now (get-internal-real-time))
	     (delta (- now last-time)))
	(setf last-time now)
	delta))))


(defun make-stepper (step-size)
  (let ((time-cache 0))
    (lambda (time) 
      (if (eq time t)
	  step-size
	  (progn
	    (setf time-cache (+ (abs time) time-cache))
	    (if (> time-cache step-size)
		(progn
		  (setf time-cache (- time-cache step-size))
		  (min 1.0 (/ time-cache step-size)))
		nil))))))


;; passes a time to a stepper and when the stepper returns is 
;; step progress the body code is executed.
;; You can also declare variables to hold the value returned from
;; the step (the step progress) and the step size of the stepper.
;; These variables can then obviously be used in the body.
(defmacro on-step-call ((stepper time 
		        &optional (step-progress (gensym))
		   		  (step-size nil)) 
			&body body)
  (if (not (symbolp step-progress))
      (error "Only symbols may be passed to the key arguments.")
      (let ((!step-prog (gensym (mkstr step-progress)))
	    (!step-size (gensym (mkstr step-size))))
	`(let ((,!step-prog (funcall ,stepper ,time))
	       ,@(when step-size
		       `((,!step-size (funcall ,stepper t)))))
	   (when ,!step-prog
	     ,(utils:walk-replace step-progress !step-prog 
		   (utils:walk-replace step-size !step-size body)))))))

;; CL-USER> (macroexpand `(with-stepper-call jam 100 sp
;;                          (call-thing arg arg2 sp)))
;;
;; (LET ((#:SP871 (FUNCALL JAM 100)))
;;   (WHEN #:SP871 ((CALL-THING ARG ARG2 #:SP871))))
;;

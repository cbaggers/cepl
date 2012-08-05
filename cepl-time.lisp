;; Functions and macros for handling time in games
;; This area is very prone to change as time is so integral
;; to game building.

(in-package cepl-time)

(defun make-time-buffer ()
  "This make a time buffer. A time buffer is a lambda which each
   time it is called reuns the ammount of time since it was last
   called. 
   It is called a time buffer as it can be imagined as
   'storing up' time for use later."
  (let ((last-time (get-internal-real-time)))
    (lambda () 
      (let* ((now (get-internal-real-time))
	     (delta (- now last-time)))
	(setf last-time now)
	delta))))


(defun make-stepper (step-size)
  "Makes a stepper. 
   A stepper is a lambda which is created with a step size. When
   the lambda is called and passed an amount of time it stores 
   it. When the amount stored is greater than the step size then
   the lambda returns a scalar between 0 and 1.0 which is how 
   much overflow there was (the overflow remains in the store).
   Here is an example:
    CEPL-EXAMPLES> (setf stepper (cepl-time:make-stepper 1000))
     #<CLOSURE (LAMBDA (TIME) :IN CEPL-TIME:MAKE-STEPPER) {10085B249B}>
    CEPL-EXAMPLES> (funcall stepper 400)
     NIL
    CEPL-EXAMPLES> (funcall stepper 400)
     NIL
    CEPL-EXAMPLES> (funcall stepper 400)
     1/5
   This scalar returned is useful for handling temporal aliasing.
   
   If you call the stepper with 't' rather than a time then the
   stepper returns the step size."
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



(defmacro on-step-call ((stepper time 
		        &optional (step-progress (gensym))
		   		  (step-size nil)) 
			&body body)
  "This passes a time to a stepper and when the stepper returns
   is step progress the body code is executed.
   You can also declare variables to hold the value returned from
   the step (the step progress) and the step size of the stepper.
   These variables can then obviously be used in the body.

    CL-USER> (macroexpand `(with-stepper-call jam 100 sp
                          (call-thing arg arg2 sp)))

    (LET ((#:SP871 (FUNCALL JAM 100)))
       (WHEN #:SP871 ((CALL-THING ARG ARG2 #:SP871))))"
  (if (not (symbolp step-progress))
      (error "Only symbols may be passed to the key arguments.")
      (let ((!step-prog (gensym (cepl-utils:mkstr step-progress)))
	    (!step-size (gensym (cepl-utils:mkstr step-size))))
	`(let ((,!step-prog (funcall ,stepper ,time))
	       ,@(when step-size
		       `((,!step-size (funcall ,stepper t)))))
	   (when ,!step-prog
	     ,@(utils:walk-replace step-progress !step-prog 
		   (utils:walk-replace step-size !step-size body)))))))

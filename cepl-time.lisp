;; Functions and macros for handling time in games
;; This area is very prone to change as time is so integral
;; to game building.

(in-package cepl-time)

;; This is just an alias with a more fitting name
(setf (symbol-function 'absolute-system-time) 
      #'get-internal-real-time)

(defun make-time-buffer (&optional (abs-time-source
				    #'get-internal-real-time))
  "This make a time buffer. A time buffer is a lambda which each
   time it is called retuns the ammount of time since it was last
   called. 
   It is called a time buffer as it can be imagined as
   'storing up' time for use later.
   In short: a time-buffer converts absolute time into relative
   time."
  (let ((last-time (funcall abs-time-source)))
    (lambda () 
      (let* ((now (funcall abs-time-source))
	     (delta (- now last-time)))
	(setf last-time now)
	delta))))

(defun make-time-cache (&optional (rel-time-source
				   (make-time-buffer)))
  "This make a time cache. A time cache is a lambda which each
   time it is called retuns the ammount of time since it was 
   created. 
   In short: a time-cache converts relative time into absolute 
   time."  
  (let ((cached-time 0))
    (lambda () 
      (setf cached-time (+ cached-time (funcall rel-time-source)))
      cached-time)))


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


;----------------------------------------------------

(define-condition temporally-expired (condition) ())
;; '(until before between within at when unless by)

(defun withinp (current-time start end)
  (if current-time
      (if (> current-time start)
          (if (< current-time end)
              current-time
              (progn (signal 'temporally-expired)
                     nil))
          nil)
      nil))

(setf (symbol-function 'betweenp)
      #'withinp)

(defun beforep (current-time time)
  "test"
  (if current-time
      (if (< current-time time)
          current-time
          (progn (signal 'temporally-expired)
                     nil))
      nil))

(setf (symbol-function 't<)
      #'beforep)

(setf (symbol-function 'untilp)
      #'beforep)

(defun afterp (current-time time)
  (if current-time
      (if (> current-time time)
          current-time
          nil)
      nil))

(setf (symbol-function 't>)
      #'beforep)

;; at is tricky, what if you miss it?

(defmacro with-expired ((expired-var) &body body)
  "This macro is used to capture whether and temporal lambda
   has expired. The sole argument is the name of the variable
   you wish to contain the state of the tlambda and after the 
   tlambda has been called the specified var will contain
   either nil meaning that it hasnt expired, or t meaning it 
   has.
   It is perfectly safe to call regular functions or lambdas
   within this form as they are seen as the equivilent of a
   tlambda with it's temporal predicate set to 'always'.
   Here is an example usage, note that before the funcall 
   'expired?' will always be nil.

     (with-expired (expired?)
       (let ((result (funcall tlam)))
	 (when expired?
	   (setf result 'NOOOOOO!'))
	 result))"
  `(let ((,expired-var nil))
     (handler-bind ((temporally-expired 
		     #'(lambda (x) 
			 (declare (ignore x))
			 (setf ,expired-var t))))
       ,@body)))


(defmacro tlambda (time-source temporalp args &body body)
  "Create a temporal lambda
   ----
   Temporal lambdas are quite simply 'lambdas with an expiry date!'
   They are defined in a similar way to regular lambdas except that
   you also have to provide a time source and a temporal-predicate.
   When you call the lambda it will only evaluate it's body if the 
   conditions of the predicate are met. When the conditions are not
   met the lambda will return nil
   For example you could specify that the temporal lambda only work
   for 20 seconds.
   Of course in the above example, after the 20 seconds has passed
   the temporal lambda will never evaluate again. In this state it 
   is said to have expired, when a temporal lambda that has expired 
   is called it emits a 'temporally-expired' condition. This can be
   caught to allow you to clean up expired tlambdas.)"
  (labels ((same-sym-namep (sym compare)
	     (and (symbolp sym)
		  (symbolp compare)
		  (string= (symbol-name sym) 
			   (symbol-name compare)))))
    (let ((internalf (gensym "internalf"))
	  (time-source-sym (gensym "time-source"))
	  (time-sym (gensym "current-time")))
      (cepl-utils:walk-replace 
       '!time time-sym
       `(let ((,time-source-sym ,time-source))
	  (labels ((,internalf ,args ,@body))
	    (lambda (&rest args)
	      (let ((,time-sym (funcall ,time-source-sym)))
		(if ,temporalp
		    (apply #',internalf args)
		    nil)))))
       :test #'same-sym-namep))))


;; probably useless...this was just me playing about
(defmacro tdefun (name (time-source temporalp args) &body body)
  `(setf (symbol-function ',name)
         (tlambda ,time-source ,temporalp ,args ,@body)))

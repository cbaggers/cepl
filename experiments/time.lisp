
;; Right so lets have a play with time.

(defun make-time-well ()
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
;; ok what about the calling of the function
;; and more than that.. what about the time-step?
;; lets keep stepper for stepping, we wil handle 
;; function calling further down.

;;---------------------

;; (with-stepcall 100 :state-lerp sl :step-size ss
;;   (render sl ss))

;; (with-stepcall 3000
;;   (update-ai))

;;---------------------

;;---------------------

;; (with-stepper-call a time :state-progress sl
;;   (render sl ss))

;; (with-stepper-call a time
;;   (update-ai))

;;---------------------

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;;this will be pretty inefficient, but shoudl be fine for code trees
(defun walk-replace (to-replace replace-with form)
  (cond ((null form) nil)
	((atom form) (if (eql form to-replace)
			 replace-with
			 form))
	(t (cons (walk-replace to-replace 
			       replace-with 
			       (car form)) 
		 (walk-replace to-replace 
			       replace-with 
			       (cdr form))))))

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
	     ,(walk-replace step-progress !step-prog 
		   (walk-replace step-size !step-size body)))))))

;; CL-USER> (macroexpand `(with-stepper-call jam 100 sp
;;                          (call-thing arg arg2 sp)))
;;
;; (LET ((#:SP871 (FUNCALL JAM 100)))
;;   (WHEN #:SP871 ((CALL-THING ARG ARG2 #:SP871))))
;;

;; The step-progress shouldn't be mandatory...how do we use optional 
;; or keys in a macro? ..ugh have to put it in a list.
;; at least that handles the optionals




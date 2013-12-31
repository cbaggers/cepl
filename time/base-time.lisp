;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; Functions and macros for handling time in games
;; This area is very prone to change as time is so integral
;; to game building.

(in-package base-time)

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

(defun make-itime-buffer (&optional (abs-time-source
                                     #'get-internal-real-time))
  "This make an interactive time buffer. A time buffer is a lambda 
   which each time it is called retuns the ammount of time since 
   it was last called.
   It is called a time buffer as it can be imagined as
   'storing up' time for use later.
   In short: a time-buffer converts absolute time into relative
   time.
   The interactive part is that you can pass and optional command
   :reset to zero the time buffer. This can save creating a new 
   time-cache, which can be handy when you are outside the original
   context but you want to maintain the lexical scope."
  (let ((last-time (funcall abs-time-source)))
    (lambda (&optional command) 
      (case command
        (:reset (setf last-time (funcall abs-time-source))))
      (let* ((now (funcall abs-time-source))
             (delta (- now last-time)))
        (setf last-time now)
        delta))))

(defun make-time-cache (&optional (rel-time-source
                                   (make-time-buffer)))
  "This make an interactive time cache. A time cache is a lambda 
   which each time it is called retuns the ammount of time since
   it was created. 
   In short: a time-cache converts relative time into absolute 
   time."  
  (let ((cached-time 0))
    (lambda () 
      (setf cached-time (+ cached-time (funcall rel-time-source)))
      cached-time)))

(defun make-itime-cache (&optional (rel-time-source
                                    (make-time-buffer)))
  "This make a time cache. A time cache is a lambda which each
   time it is called retuns the ammount of time since it was 
   created. 
   In short: a time-cache converts relative time into absolute 
   time.
   The interactive part is that you can pass and optional command
   :reset to zero the time buffer. This can save creating a new 
   time-cache, which can be handy when you are outside the original
   context but you want to maintain the lexical scope."  
  (let ((cached-time 0))
    (lambda (&optional command) 
      (setf cached-time (+ cached-time (funcall rel-time-source)))
      (case command
        (:reset (progn (setf cached-time 0)
                       0))
        (t cached-time)))))

(defun make-stepper (step-size)
  "Makes a stepper. 
   A stepper is a lambda which is created with a step size. When
   the lambda is called and passed an amount of time it stores 
   it. When the amount stored is greater than the step size then
   the lambda returns a scalar between 0 and 1.0 which is how 
   much overflow there was (the overflow remains in the store).
   Here is an example:
    CEPL-EXAMPLES> (setf stepper (make-stepper 1000))
     #<CLOSURE (LAMBDA (TIME) :IN MAKE-STEPPER) {10085B249B}>
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


;;----------------------------------------------------

;; predicates '(until before between within at when unless by)

(defun withinp (current-time start end)
  (if current-time
      (if (> current-time start)
          (if (< current-time end)
              current-time
              (progn (signal 'c-expired)
                     nil))
          nil)
      nil))

(setf (symbol-function 'betweenp)
      #'withinp)

(defun beforep (current-time time)
  "test"
  (if current-time
      (let ((ctime (funcall current-time)))
        (if (< ctime time)
            ctime
            (progn (signal 'c-expired)
                   nil)))
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





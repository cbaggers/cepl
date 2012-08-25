;; I'm trying to discover some kind of decent way of handling 
;; time that is lispy and tranparent to other code. Expressive
;; enough to handle all time conditions in games and simple
;; enough that it can be combined so we can compose new 
;; time functionality..should be a doddle...<cough>..*tumbleweed*

;; ideas for terminology
;; '(until before between within at when unless by)

;;---------------------------------------------------------------

;; ignore these two...on te wrong track here

(defmacro within ((end-time &optional (start-time 0)) &body body)
  (let ((time-cache (gensym "time-cache")))
   `(let ((,time-cache 0)) 
      (lambda (time)
	(setf ,time-cache (+ ,time-cache time))
	(if (> ,time-cache ,start-time)
	    (if (< ,time-cache ,end-time)
		,@body
		(values nil t))
	    (values nil nil))))))

(defun within-f (func &key (start-time 0) (end-time 0) (absolute-time nil))
  (let* ((time-cache (if absolute-time
			 (funcall absolute-time)
			 time-cache))
	 (start-time (if absolute-time
			 (+ time-cache start-time)
			 start-time))
	 (end-time (if absolute-time
		       (+ start-time end-time)
		       end-time))) 
    (lambda (&optional time)
      (if absolute-time
	  (setf time-cache (+ time-cache (funcall absolute-time)))
	  (setf time-cache (+ time-cache time)))
      (if (> time-cache start-time)
	  (if (< time-cache end-time)
	      (funcall func)
	      (values nil t))
	  (values nil nil)))))

;;---------------------------------------------------------------

;; in time is there lexical time ?

;;---------------------------------------------------------------

;; temporal-lambda 
(tlambda (x) (within 0 5000) (print x))

(defmacro tlambda (&whole whole args temporalp &body body)
  '(lambda ,whole ,args 
    (multiple-value-bind (satisfied expired) ())))

;; nope not like that.. temporal predicates must be like regular
;; predicates (listp a) says whether a is a list. (within a x y)
;; needs to return whether the temporal lambda 'a' is within x
;; and y (for whatever that means!).
;; 
;; really?...is that what I mean?
;; temporal lambdas should work just like regular lambda
;; you give them arguments and they work as long as they satisfy
;; a temporal predicate.
;; 
;; For a temporal predicate to work there must be time, or rather
;; a time source which the predicate uses to evaluate whether to
;; execute.

;; there are two things a temporal predicate must return:
;;
;; - t or nil saying whether the predicate has been met
;; - t or nil saying whether the predicate will ever be met again
;;
;; these are returned using values so that you can ignore the 
;; latter if you choose. The latter is essential for, for the 
;; lack of a better phrase, temporal garbage collection.
;;
;; for a temporal predicate to work against a temporal lambda
;; it is going to have to look inside the lexical scope of the 
;; closure...this sounds ugly.

;; time-buffer - converts absolute time to relative time
;; time-cache - converts relative time to absolute time
;;
;; both of these have time sources.
;; time-cache can be made using time-buffer so may not be equal
;; in 'specialness'
;;
;; need time sources ..we have absolute, its 
;; #'get-internal-real-time. 

;; time buffer makes the absolute #'get-internal-real-time into
;; a relative time. So really they are the two time sources.
;; Absolute & Relative

;; From them we need time warp functions. They wrap a time source
;; and give a (usualy) different version of time.

(setf time (make-time-buffer))
;;*one second passes*
; > (funcall time)
; 1000

(setf double-time (time-multiplier time 2.0))
;;*one second passes*
; > (funcall double-time)
; 2000

;; aren't time-warpy functions a bit pointless in that they are
;; just regular functions

(setf tcache (make-time-cache double-time))
;;*one second passes*
; > (funcall tcache)
; 2000
;;*one second passes*
; > (funcall tcache)
; 4000



(defmacro tlambda (time-source temporal-predicate args &body body)
  ;;let time source
  (let ((time (gensym "time-source")))
    `(let ((,time ,time-source))
       (labels ((internal-lam ,args ,@body))
	 (lambda (&rest args) 
	   (case (car args)
	     (:*expired* 
	      (second (multiple-value-list
		       (funcall #',temporal-predicate 
				(funcall ,time)))))
	     (t 
	      (if (car (multiple-value-list
			(funcall #',temporal-predicate
				 (funcall ,time))))
		  (apply #'internal-lam args)
		  nil))))))))



;;--------------------------------------------------------------
;; Not bad but I opened a new file as ran through this afresh
;;--------------------------------------------------------------

(defun make-time-buffer ()
  (let ((last-time (get-internal-real-time)))
    (lambda () 
      (let* ((now (get-internal-real-time))
	     (delta (- now last-time)))
	(setf last-time now)
	delta))))

(defun make-time-cache ()
  (let ((origin-time (get-internal-real-time)))
    (lambda () 
      (let* ((now (get-internal-real-time))
	     (delta (- now origin-time)))
	delta))))

(defun withinp (start end time-source)
  (let ((current-time (funcall time-source)))
    (if (> current-time start)
        (if (< current-time end)
            (values t nil)
            (values nil t))
        (values nil nil))))

(defun make-withinp (start end)
  (lambda (time-source) (withinp start end time-source)))

(defun make-tlambda (time-source temporalp func-to-call)
  (lambda (&rest args) 
    (if (eq (car args) :*expired?*)
        (second 
         (multiple-value-list (funcall temporalp time-source)))
        (if (first 
             (multiple-value-list (funcall temporalp time-source)))
            (apply func-to-call args)
            nil))))

(defmacro tlambda (time-source temporalp args &body body)
  (let ((internalf (gensym "internalf")))
    `(labels ((,internalf ,args ,@body))
      (make-tlambda ,time-source ,temporalp #',internalf))))

(defun expired (temporal-lambda)
  (funcall temporal-lambda :*expired?*))

;; look into variable capture as is not currently portable for 
;; regular lambdas as passing :*expired?* will crash them.

(defvar *expired?* nil)

(defun make-tlambda (time-source temporalp func-to-call)
  (lambda (&rest args)
    (multiple-value-bind (in-scope expired) 
	(funcall temporalp time-source)
      (setf *expired?* expired)
      (if in-scope
	  (apply func-to-call args)
	  nil))))

(defun expiredp (temporal-lambda)
  (let ((*expired?* nil))
    (funcall temporal-lambda)
    *expired?*))

;; the above sucks as we have to evaluate the lambda, this could
;; have side-effects. It shouldn't though...bah. Also if you call
;; a regular lambda without its args it will fail. damn.

;; Ok so I'm tapping out now...expiredp must only be used on 
;; temporal-lambdas. Everything else is still transparent 
;; between regular and temporal lambdas. I'm going with the 
;; :*expired?* method as I expect this will be safer when 
;; multi-threading (i'm not usre how dynamic vars are handled in
;; multiple threads).

;; But we still have the problem of.'what if a lambda does get
;; passed :*expired?* and DOESNT crash, we have undefined 
;; behaviour.

;; how about conditionals?...been chatting and could be good
;; idea. Speed is obviously a concern, so I will have to look
;; into this.

;; Ok so I'm not tapping out...more fun to come


;; use conditions emit if dynamic var is true
;; definitely the irght direction of thought

(define-condition temporally-expired (condition))

(defun make-tlambda (time-source temporalp func-to-call)
  (lambda (&rest args)
    (multiple-value-bind (in-scope expired) 
	(funcall temporalp time-source)
      (when expired
	(signal 'temporally-expired))
      (if in-scope
	  (apply func-to-call args)
	  nil))))

;;--ideas--
;;this works fine with regular lambdas
(with-expired (expired)
  (let ((result (funcall tlam)))
    (print result)
    (if expired
	(print "expired")
	(print "not expired"))))

;;this won't work fine with all regular lambdas
(expiredp tlam)


;;this is how we can catch the expired condition
(let ((expired nil))
  (handler-bind ((temporally-expired #'(lambda () 
					 (setf expired t))))
    (print (funcall tlam))))

;;-------

(defmacro with-expired ((expired-var) &body body)
  `(let ((,expired-var nil))
     (handler-bind ((temporally-expired 
		     #'(lambda () 
			 (setf ,expired-var t))))
       ,@body)))



(defun test (tlam)
  (with-expired (expired?)
    (print (funcall tlam))
    (when expired?
      (print "NOOOOOO!"))))

;;----------------

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
   caught to allow you to clean up expired tlambdas."
  (let ((internalf (gensym "internalf"))
        (time-source-sym (gensym "time-source"))
        (time-sym (gensym "current-time")))
    (cepl-utils:walk-replace '!time time-sym
                  `(let ((,time-source-sym ,time-source))
                     (labels ((,internalf ,args ,@body))
                       (lambda (&rest args)
                         (let ((,time-sym (funcall ,time-source-sym)))
                           (if ,temporalp
                               (apply #',internalf args)
                               nil))))))))

;;Example
(tlambda (make-time-buffer) (within 10 20 !time) (x) (print x))

(tlambda (make-time-cache) (afterp 3000 (beforep 15000 !time)) ()
  (print "wooo"))

(defmacro tdefun (name (time-source temporalp args) &body body)
  `(setf (symbol-function ',name)
         (tlambda ,time-source ,temporalp ,args ,@body)))


;; SHOULD TEMPORAL PREDICATES TAKE TIME SOURCE OR JUST NUMBERS
;; also should they return t or time?
;; how do tperds handle passing of expired?
;; how about we move the 'temporally-expired signal into the temporal 
;; predicates. This way it works in any structure we devise

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

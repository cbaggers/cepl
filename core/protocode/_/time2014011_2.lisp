;; Functions and macros for handling time in games
;; This area is very prone to change as time is so integral
;; to game building.

(in-package :base-time)

;;----------------------------------------------------------------------
;; Time units
;;------------

(defmacro def-time-units (&body forms)
  (unless (numberp (second (first forms)))
    (error "base unit must be specified as a constant"))
  (let ((defined nil))
    `(progn
       ,@(loop :for (type expression) :in forms
            :for count = (if (numberp expression)
                             expression
                             (if (and (listp expression)
                                      (= (length expression) 2)
                                      (numberp (second expression))
                                      (assoc (first expression) defined))
                                 (* (second expression)
                                    (cdr (assoc (first expression) defined)))
                                 (error "invalid time expression")))
            :collect `(defun ,type (quantity) (* quantity ,count))
            :do (push (cons type count) defined)))))

(def-time-units
  (milliseconds 1)
  (seconds (milliseconds 1000))
  (minutes (seconds 60))
  (hours (minutes 60)))

;;----------------------------------------------------------------------
;; Relative values
;;-----------------

;;{TODO} should also support other source types....not sure what those are
(defgeneric update-relative (val))
(defmacro make-relative-type (type &key (dif '-))
  (let* ((type (if (and (listp type) (eq (first type) 'quote))
                   (second type) type))
         (rtype (symb 'relative- type)))
    `(progn
       (defstruct ,rtype
         (value 0 :type ,type)
         (last-value 0 :type ,type)
         (source (lambda () 0) :type function))

       (defmethod update-relative ((val ,rtype))
         (let ((cval (funcall (,(symb rtype '-source) val)))
               (lval (,(symb rtype '-last-value) val)))
           (setf (,(symb rtype '-value) val) (,dif cval lval))
           (setf (,(symb rtype '-last-value) val) cval)
           (,(symb rtype '-value) val))))))

(make-relative-type 'integer)

;;------------------------------------------------------------

(defparameter *default-time-source* #'get-internal-real-time)

(define-condition c-expired (condition) ())

(defun signal-expired () (signal 'c-expired) nil)

(defmacro expiredp (&body body)
  `(handler-case (progn ,@body nil)
     (c-expired (c) (progn c t))))

;;{TODO} hmm what if the true state pulses? The the expire is incorrect
(defmacro defcon (name args condition &body body)
  (let ((lived (gensym "lived")))
    `(let ((,lived nil))
       (defun ,name ,args
         (if ,condition
             (progn (setf ,lived t) ,@body)
             (when ,lived (signal 'c-expired) nil))))))

(defmacro conditional (args test &body body)
  (let ((lived (gensym "lived"))
        (condit (gensym "condition")))
    `(let ((,lived nil)
           (,condit ,(if (eq (first condition) 'cl:function)
                         condition
                         `(lambda () ,condition))))
       (lambda ,args
         (if (funcall ,condit)
             (progn (setf ,lived t) ,@body)
             (when ,lived (signal 'c-expired) nil))))))

(defmacro cfn (args condition &body body)
  `(conditional ,args ,condition &body ,body))

;; like compose for conditions, makes a lambda that when evaluated runs each
;; form until it expires and then moves to the next, at the end it will return
;; nil and release an expired condition
(defmacro then (args &body forms)
  (let ((step (gensym "step")))
    `(let ((,step 0))
       (lambda (,args)
         (case step
           ,@(loop :for form :in forms :for i :from 0 :collect
                `(,i (handler-case ,form
                       (c-expired (c) (ignore c) (incf ,step) nil))))
           (,(length forms) (signal-expired)))))))

;;--------------------------------------------------------------------
;; 'every' is the temporal implementation of steppers, they eat time from a
;; relative source and call functions when 'time-buffer is full'
;; 'every*' is a version that handles multiple things with one source, perfect
;; for main loops

(defun make-rel-time (&optional (time-source *default-time-source*))
  (let* ((last 0) (step 0)
         (func (lambda (&optional trigger)
                 (when (eq trigger :step)
                   (let ((new (funcall time-source)))
                     (setf step (- new last) last new)))
                 step)))
    (funcall func :step)
    func))

(defun make-time-source (&key (type :absolute) (parent *default-time-source*)
                           (transform nil))
  (case type
    (:absolute (if transform
                   (lambda (&optional x)
                     (declare (ignore x))
                     (funcall transform (funcall parent)))
                   (lambda (&optional x)
                     (declare (ignore x))
                     (funcall parent))))
    (:relative (if transform
                   (error "transforms cant currently be used on relative time")
                   ;; (make-rel-time parent)
                   (let ((result (make-instance 'rel-time :source parent)))
                     (t-step result)
                     result)))
    (t (error "unknown time-source type"))))

(defun from-now (time-offset &optional (time-source *default-time-source*))
  (+ time-offset (funcall time-source)))

(defun beforep (time &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (< current-time time) current-time)))

(defun afterp (time &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (> current-time time) current-time)))

(defun betweenp (start-time end-time
                &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (and (>=  start-time) (<= current-time end-time)) current-time)))



(defmacro with-time-source (time-source &body body)
  `(let ((*default-time-source* ,time-source))
     ,@body))

(defun make-stepper (step-size &optional (default-source *default-time-source*))
  "this takes absolute sources"
  (let ((time-cache 0)
        (last-val (funcall default-source)))
    (lambda (&optional (time-source default-source))
      (if (eq time-source t)
          step-size
          (let* ((time (abs (funcall time-source)))
                 (dif (- time last-val)))
            (setf last-val time)
            (incf time-cache dif)
            (when (> time-cache step-size)
              (setf time-cache (- time-cache step-size))
              (min 1.0 (/ time-cache step-size))))))))


;; (every-t (seconds 1) (print "hi"))
(defmacro every-t (timestep &body forms)
  (let* ((stepper (gensym "stepper"))
         (source (if (eq (first forms) :default-source)
                     (second forms)
                     '*default-time-source*))
         (forms (if (eq (first forms) :default-source)
                    (cddr forms) forms)))
    `(let ((,stepper (make-stepper ,timestep ,source)))
       (lambda (&optional (time-source ,source))
         (when (funcall ,stepper time-source)
           ,@forms)))))

;;{TODO} this gets the behaviour right but performance isnt great

(defmacro every-t* ((&optional (time-source *default-time-source*)) &body forms)
  (loop :for (timestep form) :in forms :for name = (gensym "stepper")
     :collect `(,name (make-stepper ,timestep ,time-source)) :into steppers
     :collect `(when (funcall ,name time-source) ,form) :into clauses
     :finally (return
                `(let (,@steppers)
                   (lambda (&optional (time-source ,time-source)) ,@clauses)))))


(let ((deadline (from-now (milliseconds 500))))
  (loop :until (after deadline) :finally (print "hi")))

;; this can now be a struct
(defclass rel-time ()
  ((time-value :initform 0)
   (last-value :initform 0)
   (parent :initform nil :initarg :source :accessor parent)))

;;----------------------------------------------------------------------

(defmethod get-time ((time-source function))
  (funcall time-source))
(defmethod get-time ((time-source rel-time))
  (slot-value time-source 'time-value))

(defmethod t-step ((time function))
  (funcall time))
(defmethod t-step ((time-source rel-time))
  (let ((ctime (get-time (parent time-source))))
    (setf (slot-value time-source 'time-value)
          (- ctime (slot-value time-source 'last-value)))
    (setf (slot-value time-source 'last-value) ctime)
    (slot-value time-source 'time-value)))

;;----------------------------------------------------------------------

;; The crappiest little time-manager example

(let ((entries '(t)))
  (defun update-time-manager ()
    (let ((last entries)
          (current (cdr entries)))
      (loop :until (null current) :do
         (when (not (expiredp (funcall (car current))))
           (setf (cdr last) current)
           (setf last current))
         (setf current (cdr current)))))
  (defun t-manage (item) (push item entries))
  (defun t-releaae (item) (delete item entries)))


;;----------------------------------------------------------------------
;; ok so last major problem I think

;; this is ok
(let ((end (from-now (seconds 10))))
  (t-manage (conditional () (before end) (print 'still-here))))

;; but this would be better
(t-manage (conditional () (before (from-now (seconds 10)))
            (print 'still-here)))
;; this means that conditional takes a test FUNCTION and before needs to be a
;; lambda itself...well it needs to be a
(defun nbefore (offset &optional (source *default-time-source*))
  (let ((source source)
        (offset offset))
    (lambda () (if (< (funcall source) offset) t
                   (progn (signal-expired) nil)))))

(defmacro nconditional (args test &body body)
  (let ((gtest (gensym "test")))
    `(let ((,gtest ,test ))
      (lambda ,args (when (funcall ,gtest) ,@body)))))

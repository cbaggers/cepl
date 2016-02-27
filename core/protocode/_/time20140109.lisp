;; Functions and macros for handling time in games
;; This area is very prone to change as time is so integral
;; to game building.

(in-package :base-time)

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

;;------------------------------------------------------------

(define-condition c-expired (condition) ())

(defun signal-expired () (signal 'c-expired) nil)

(defmacro defcon (name args condition &body body)
  (let ((lived (gensym "lived")))
    `(let ((,lived nil))
       (defun ,name ,args
         (if ,condition
             (progn (setf ,lived t) ,@body)
             (when ,lived (signal 'c-expired) nil))))))

(defmacro conditional (args condition &body body)
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

(defclass time-source () ())

(defclass absolute-time-source (time-source)
  ((value :initform 0 :initarg :value :type integer
          :accessor time-value)
   (last-updated :initform 0 :initarg :last-updated :type integer
                 :accessor last-updated)
   (parent :initform nil :initarg :parent :type (or null time-source)
           :accessor parent-source)
   (transform :initform nil :initarg :transform :type (or null function)
              :accessor transform)))

(defclass relative-time-source (time-source)
  ((value :initform 0 :initarg :value :type integer :accessor time-value)
   (last-updated :initform 0 :initarg :last-updated :type integer
                 :accessor last-updated)
   (parent :initarg :parent :type time-source
           :accessor parent-source)))

(defparameter *system-time* (make-instance 'absolute-time-source))

(defun update-system-time ()
  (setf (time-value *system-time*) (absolute-system-time)))

(defmethod update-time ((source absolute-time-source))
  (let ((parent-time (time-value (parent-source source))))
    (setf (last-updated source) parent-time)
    (if (parent-source source)
        (if (transform source)
            (setf (time-value source)
                  (floor (funcall (transform source) parent-time)))
            (setf (time-value source) parent-time))
        (time-value source))))

(defmethod update-time ((source relative-time-source))
  (let ((parent-time (time-value (parent-source source))))
    (setf (time-value source) (- parent-time (last-updated source)))
    (setf (last-updated source) parent-time))
  (time-value source))

(defun make-time-source (&key (type :relative) (parent *system-time*)
                           (transform nil))
  (case type
    (:absolute (make-instance 'absolute-time-source :transform transform
                              :parent parent))
    (:relative (if transform
                   (error "transforms cant currently be used on relative time")
                   (make-instance 'relative-time-source :parent parent)))
    (t (error "unknown time-source type"))))

(defun before (time &optional (time-source *system-time*))
  (when (< (time-value time-source) time) (time-value time-source)))
(defun after (time &optional (time-source *system-time*))
  (when (> (time-value time-source) time) (time-value time-source)))
(defun between (start-time end-time &optional (time-source *system-time*))
  (when (and (>= (time-value time-source) start-time)
             (<= (time-value time-source) end-time))
    (time-value time-source)))

;;--------------------------------------------------------------------

;; 'every' is the temporal implementation of steppers, they eat time from a
;; relative source and call functions when 'time-buffer is full'
;; 'every*' is a version that handles multiple things with one source, perfect
;; for main loops

(defun from-now (time-offset &optional (time-source *system-time*))
  (+ time-offset (time-value time-source)))

(loop :until (after 10000 *source*)
   :do (print "hi"))


(let ((deadline (from-now (seconds 10))))
  (loop :while (after deadline *source*)
     :do (print "hi")))

(conditional ((before 12000 *time*) :name @test)
  (lerp (pos *obj*) (v! 0 0 0) (/ (pos *obj*) @test)))

;; the problem is that non-function based time objects dont update themselves
;; so that has to be remebered
(progn
  (update-system-time)
  (let ((deadline (from-now (seconds 2))))
    (loop :until (after deadline)
       :do (update-system-time) :finally (print "hi"))))

;; however relative sources should only be updated in one place as it is
;; relative TO something.

;; want to write this
(let ((deadline (from-now (seconds 2))))
  (loop :until (after deadline) :finally (print "hi")))

;; ...ok so now absolute times are functions

(defun from-now (time-offset &optional (time-source #'absolute-system-time))
  (+ time-offset (funcall time-source)))

(defun before (time &optional (time-source #'absolute-system-time))
  (let ((current-time (funcall time-source)))
    (when (< current-time time) current-time)))

(defun after (time &optional (time-source #'absolute-system-time))
  (let ((current-time (funcall time-source)))
    (when (> current-time time) current-time)))

(defun between (start-time end-time
                &optional (time-source #'absolute-system-time))
  (let ((current-time (funcall time-source)))
    (when (and (>=  start-time) (<= current-time end-time)) current-time)))

;; what does tidy relative time look like?

(let ((deadline (from-now (seconds 2))))
  (loop :until (after deadline) :do (format t "timediff=~a" rel-time)))

(defun make-rel-time (&optional (time-source #'absolute-system-time))
  (let* ((last 0) (step 0)
         (func (lambda (&optional trigger)
                 (when (eq trigger :step)
                   (let ((new (funcall time-source)))
                     (setf step (- new last) last new)))
                 step)))
    (funcall func :step)
    func))

(defun make-time-source (&key (type :relative) (parent #'absolute-system-time)
                           (transform nil))
  (case type
    (:absolute (if transform
                   (lambda (x)
                     (declare (ignore x))
                     (funcall transform (funcall parent)))
                   (lambda (x)
                     (declare (ignore x))
                     (funcall parent))))
    (:relative (if transform
                   (error "transforms cant currently be used on relative time")
                   (make-rel-time parent)))
    (t (error "unknown time-source type"))))

(defun t-step (time) (funcall time :step))

;; dammnit now I am wondering about objects again
;; what could this be
(let ((deadline (from-now (milliseconds 500))))
      (loop :until (after deadline) :finally (print "hi")))
;; well 'after' could call the get-time method passing in the timesource
(get-time timesource)
;; for absolute just returns the parent passed into the transform func
;; for relative it would get the time using and t-step would update the
;; timesource
(let ((deadline (from-now (milliseconds 1500)))
          (rel (make-time-source)))
      (loop :until (after deadline) :finally
         (format t "timediff=~a" (t-step rel))))

;; why is this any better? seems slower for a start
;; well I was worried that steppers would be fed relative time objects and that
;; seemed like a lot of function calls.... I guess its not too bad.

;; ok stick with this for now
(defun make-stepper (step-size)
  (let ((time-cache 0))
    (lambda (time)
      (if (eq time t)
          step-size
          (progn
            (setf time-cache (+ (abs time) time-cache))
            (when (> time-cache step-size)
              (setf time-cache (- time-cache step-size))
              (min 1.0 (/ time-cache step-size))))))))

(defun make-stepper (step-size)
  (let ((time-cache 0))
    (lambda (&optional (time-source #'absolute-system-time))
      (if (eq time-source t)
          step-size
          (let ((time (funcall time-source)))
            (incf time-cache (abs time))
            (when (> time-cache step-size)
              (setf time-cache (- time-cache step-size))
              (min 1.0 (/ time-cache step-size))))))))

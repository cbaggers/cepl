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

(in-package :base-time)

(define-condition c-expired (condition) ())

(defun signal-expired () (signal 'c-expired))

(defmacro defcon (name args condition &body body)
  (let ((lived (gensym "lived")))
    `(let ((,lived nil))
       (defun ,name ,args
         (if ,condition
             (progn (setf ,lived t) ,@body)
             (when ,lived (signal 'c-expired) nil))))))

(defmacro conditional (args condition &body body)
  (let ((lived (gensym "lived")))
    `(let ((,lived nil))
       (lambda ,args
         (if ,condition
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
           (,(length forms) (signal-expired) nil))))))

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


;; 'every' is the temporal implementation of steppers, they eat time from a 
;; relative source and call functions when 'time-buffer is full'
;; 'every*' is a version that handles multiple things with one source, perfect
;; for main loops

(defun from-now (time-offset &optional (time-source *system-time*))
  (+ time-offset (time-value time-source)))

(loop :until (after 10000 *source*)
   :do (print "hi"))


(let ((deadline (from-now (seconds 10))))
  (loop :until (after deadline *source*)
     :do (print "hi")))

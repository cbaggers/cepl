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

;; ;;--------------------------------------------------------------------

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

(defparameter *system-time* (make-time-source))
(defun update-system-time () 
  (setf (time-source-value *system-time*) (absolute-system-time)))

(defstruct time-source
  (value 0.0 :type short-float)
  (last-updated 0.0 :type short-float)
  (source nil :type (or null time-source))
  (transform nil :type (or null function)))

(defun update-time (source)
  (setf (time-source-last-updated source) (absolute-system-time))
  (if (time-source-source source)
      (if (time-source-transform source)
          (setf (time-source-value source) 
                (funcall (time-source-transform source)
                         (time-source-value (time-source-source source))))
          (setf (time-source-value source) 
                (time-source-value (time-source-source source))))
      (time-source-value source)))

(defun before (time time-source)
  (let ((tsv (time-source-value time-source))) 
    (when (< tsv time) tsv)))

(defun after (time time-source)
  (let ((tsv (time-source-value time-source))) 
    (when (> tsv time) tsv)))

(defun during (start-time end-time time-source)
  (let ((tsv (time-source-value time-source))) 
    (when (and (> tsv start-time) (< tsv end-time)) tsv)))



;; 'every' is the temporal implementation of steppers, they eat time from a 
;; relative source and call functions when 'time-buffer is full'
;; 'every*' is a version that handles multiple things with one source, perfect
;; for main loops

(conditional ((before 12000 *time*) :name @test) 
  (lerp (pos *obj*) (v! 0 0 0) (/ (pos *obj*) @test)))

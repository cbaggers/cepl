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

;; ;;--------------------------------------------------------------------

;; (defparameter *system-time* (make-time-source))
;; (defun update-system-time () 
;;   (setf (time-source-value *system-time*) (absolute-system-time)))

;; (defstruct time-source
;;   (value 0.0 :type short-float)
;;   (source nil :type (or null time-source))
;;   (transform nil :type (or null function)))

;; (defun update-time (source)
;;   (if (time-source-source source)
;;       (if (time-source-transform source)
;;           (setf (time-source-value source) 
;;                 (funcall (time-source-transform source)
;;                          (time-source-value (time-source-source source))))
;;           (setf (time-source-value source) 
;;                 (time-source-value (time-source-source source))))
;;       (time-source-value source)))

;; (defun before (time &optional (time-source (make-)))
;;   )

;; ;; 'every' is the temporal implementation of steppers, they eat time from a 
;; ;; relative source and call functions when 'time-buffer is full'
;; ;; 'every*' is a version that handles multiple things with one source, perfect
;; ;; for main loops

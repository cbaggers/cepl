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

;;--------------------------------------------------------------------

(define-condition c-expired (condition) ())

(defun signal-expired () (signal 'c-expired) nil)

(defmacro expiredp (&body body) 
  `(handler-case (progn ,@body nil)
     (c-expired (c) (progn c t))))

;;--------------------------------------------------------------------

;;{TODO} ugh...this isnt as fast as I'd like, now do I specify the 
;;       test as code? compiler macro?
(defmacro defcon (name args test &body body)   
  (let ((gtest (gensym "test")))
    `(let ((,gtest ,test))
       (defun ,name ,args (when (funcall ,gtest) ,@body)))))

(defmacro conditional (args test &body body)
  (let ((gtest (gensym "test")))
    `(let ((,gtest ,test))
      (lambda ,args (when (funcall ,gtest) ,@body)))))

(defmacro cfn (args condition &body body)
  `(conditional ,args ,condition ,@body))

(defun cfn+ (&rest funcs)
  (lambda () (every #'funcall funcs)))

;;--------------------------------------------------------------------

(defmacro then (args &body forms)
  (let ((step (gensym "step")))
    `(let ((,step 0)) 
       (lambda (,args)         
         (case step
           ,@(loop :for form :in forms :for i :from 0 :collect
                `(,i (handler-case ,form 
                       (c-expired (c) (ignore c) (incf ,step) nil))))
           (,(length forms) (signal-expired)))))))

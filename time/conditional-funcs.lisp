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

(define-condition c-expired (condition) ())

(defun signal-expired () (signal 'c-expired))

(defmacro with-expired ((expired-var) &body body)
  `(let ((,expired-var nil))
     (handler-bind ((c-expired 
                     #'(lambda (x) 
                         (declare (ignore x))
                         (setf ,expired-var t))))
       ,@body)))

(defmacro defcfun (name args condition &body body)
  `(defun ,name ,args
     (if ,condition
         ,@body
         (progn (signal 'c-expired) nil))))

;; predicates

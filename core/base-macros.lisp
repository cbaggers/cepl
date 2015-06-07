;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; a load of handy macros to use around the place
;; the base-* packages are meant to be 'used' so that
;; there is no need to write the package name.

(in-package :base-macros)

;;;--------------------------------------------------------------

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;----------------------------------------------------------------
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

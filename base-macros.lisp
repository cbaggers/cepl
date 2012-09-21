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

;; This macro makes it very simple to create functions
;; where the args passed in are cached and used to 
;; evaluate whether the body should be eval'd

;; [TODO] This won't handle complex argument lists well. 
;;        Probably not something I care about yet, but 
;;        it'll bite me another day for sure.

(defmacro defmemo (name (&rest args) &body body)
  "This creates a function called 'name' which when called
   will only evalute if the values of the arguments passed in
   are different from the values passed to the arguements the 
   previous call.
   For example:
    CEPL-EXAMPLES> (base-macros:defmemo thing (x) (print 'yay))
     THING
    CEPL-EXAMPLES> (thing 5)
     YAY 
     5
    CEPL-EXAMPLES> (thing 5)
     NIL
    CEPL-EXAMPLES> (thing 3)
     YAY 
     3
   This is used in cepl-gl so that you can call bind-vao any 
   number of times and it will only eval if the vao to be bound
   is different (otherwise there is no need). This gives a speed
   boost as the cost of rebinding the vao over and over is 
   replaced with a simple if.

    (defmemo memo-bind-buffer (target buffer-id) 
      (gl:bind-buffer target buffer-id))

    (LET ((#:CACHE874 NIL) (#:CACHE875 NIL))
      (DEFUN MEMO-BIND-BUFFER (TARGET BUFFER-ID)
        (UNLESS (AND (EQ TARGET #:CACHE874) 
                     (EQ BUFFER-ID #:CACHE875))
          (gl:BIND-BUFFER TARGET BUFFER-ID)
          (SETF #:CACHE874 TARGET)
          (SETF #:CACHE875 BUFFER-ID))))"
  (let ((sym-args (loop for arg in args
                     collect (gensym "CACHE"))))
    `(let ,(loop for sym-arg in sym-args
                collect (list sym-arg nil))
       (defun ,name ,args
         (unless (and ,@(loop for arg in args
                             for sym-arg in sym-args
                             collect (list `eq arg sym-arg)))
           ,@body
           ,@(loop for arg in args
                             for sym-arg in sym-args
                             collect (list `setf sym-arg arg)))))))

;;;--------------------------------------------------------------

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;;;--------------------------------------------------------------

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so 
   errors don't kill the app."
  `(restart-case 
       (progn ,@body)
     (continue () :report "Continue")))

;;;--------------------------------------------------------------

(defmacro do-until (test &body body)
  (let ((ignorable (gensym)))
    `(do ((,ignorable t)) (,test)
       (declare (ignore ,ignorable))
       ,@body)))


;----------------------------------------------------------------
;; 
(defmacro apply-across-elements (call array-forms 
			     num-of-elms &body body)
  "This is a helper macro to limit the amount of ugly code 
   in some of the maths libraries. See the following for 2 
   examples of how it is used:
   ;; (apply-across-elements make-vector3 ((vc-a vector-a)) 3
   ;;   (* vc-a b)))

   ;; (MAKE-VECTOR3 (* (AREF VECTOR-A 0) B)
   ;;               (* (AREF VECTOR-A 1) B)
   ;; 	         (* (AREF VECTOR-A 2) B))


   ;; (apply-to-elements make-matrix3 ((vc-a mat-a) 
   ;;                                  (vc-b mat-b)) 9
   ;;   (+ vc-a vc-b))

   ;; (MAKE-MATRIX3 (+ (AREF MAT-A 0) (AREF MAT-B 0))
   ;; 	         (+ (AREF MAT-A 1) (AREF MAT-B 1)) 
   ;; 	         (+ (AREF MAT-A 2) (AREF MAT-B 2))
   ;; 	         (+ (AREF MAT-A 3) (AREF MAT-B 3))
   ;; 	         (+ (AREF MAT-A 4) (AREF MAT-B 4))
   ;; 	         (+ (AREF MAT-A 5) (AREF MAT-B 5))
   ;; 	         (+ (AREF MAT-A 6) (AREF MAT-B 6))
   ;; 	         (+ (AREF MAT-A 7) (AREF MAT-B 7))
   ;; 	         (+ (AREF MAT-A 8) (AREF MAT-B 8)))"
  (labels ((subst-many (el-num tree swap-list) 
	     (if (null swap-list)
		 tree
		 (subst-many el-num 
			     (let* ((form (car swap-list))
				    (old (car form))
				    (new `(aref ,(cadr form) 
						,el-num)))
			       (subst new old tree)) 
			     (cdr swap-list))))
	   (gen-line (&optional (el-num 0))
	     (if (< el-num num-of-elms)
		 (cons (subst-many el-num (car body) array-forms) 
		       (gen-line (+ el-num 1)))
		 nil)))
    `(,call ,@(gen-line))))

;----------------------------------------------------------------

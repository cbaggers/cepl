;; a load of handy macros to use around the place

(in-package :base-macros)

;; (base-macros:DEFUN-MEMOBIND buffer (gl:bind-buffer target buffer-id) 0)
;; This creates a memoized bind and unbind for a bind-call
;; It wont call the bind clause unless it is needed and contains
;; a cache so it calls the bind clause with the old value on
;; unbind

;; Actually, This is over the top for a lot of what we need, see 
;; below this for a different memoise function.
(defmacro defun-memobind (bind-type-name bind-expression &optional (default nil))
  (let* ((args (cdr bind-expression))
	 (curs-n-caches (loop for arg in args
			   collect (list (gensym "CUS") 
					 (gensym "CAS") 
					 (gensym "CU")))))
    `(let ,(loop for i in (utils:flatten (loop for trip in curs-n-caches
					       collect (list (first trip) (second trip))))
	      collect (list i default))
       (defun ,(utils:symb 'bind- bind-type-name) ,args
	 (unless (and ,@(loop for trip in curs-n-caches
			   for arg in args
			   collect (list 'eq 
					   (first trip)
					   arg)))
	   ,bind-expression)
	 ,@(loop for trip in curs-n-caches
	      collect (list `cons (first trip) (second trip)))
	 ,@(loop for trip in curs-n-caches
	      for arg in args
	      collect (list `setf arg (first trip))))
       
       (defun ,(utils:symb 'unbind- bind-type-name) ()
	 (let ,(loop for trip in curs-n-caches
		  collect (list (third trip) 
				  (list 'first (second trip))))
	   (unless (and ,@(loop for trip in curs-n-caches
			     collect (list 'eq 
					   (first trip)
					   (third trip))))
	     ,(cons (first bind-expression) 
		    (loop for trip in curs-n-caches
		       collect (third trip))))
	   ,@(loop for trip in curs-n-caches
		collect (list 'setf (first trip) (third trip)))
	   ,@(loop for trip in curs-n-caches
		collect (list 'setf 
			      (second trip)
			      (list 'cdr (second trip)))))))))



;; This macro makes it very simple to create functions
;; where the args passed in are cached and used to 
;; evaluate whether the body should be eval'd

;; [TODO] This won't handle complex argument lists well. 
;;        Probably not something I care about yet, but 
;;        it'll bite me another day for sure.

(defmacro defmemo (name (&rest args) &body body)
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

;; (defmemo memo-bind-buffer (target buffer-id) 
;;   (gl:bind-buffer target buffer-id))

;; (LET ((#:CACHE874 NIL) (#:CACHE875 NIL))
;;   (DEFUN MEMO-BIND-BUFFER (TARGET BUFFER-ID)
;;     (UNLESS (AND (EQ TARGET #:CACHE874) (EQ BUFFER-ID #:CACHE875))
;;       (gl:BIND-BUFFER TARGET BUFFER-ID)
;;       (SETF #:CACHE874 TARGET)
;;       (SETF #:CACHE875 BUFFER-ID))))


;;;--------------------------------------------------------------

(defmacro continuable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case 
       (progn ,@body)
     (continue () :report "Continue")))

;;;--------------------------------------------------------------

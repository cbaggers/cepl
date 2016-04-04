(in-package :cepl.memory)

;;----------------------------------------------------------------------

(defgeneric initialized-p (object))

;;----------------------------------------------------------------------

(defvar *post-context-init* nil)

(defstruct delayed
  (waiting-on nil :type list)
  (thunk (error "delayed must have a constructor thunk")
	 :type function))

(defun delay-initialization (init-thunk waiting-on-these-resources)
  (push (make-delayed :waiting-on waiting-on-these-resources
		      :thunk init-thunk)
	*post-context-init*)
  t)

;;----------------------------------------------------------------------

(defun on-context ()
  (initialize-all-delayed *post-context-init*)
  (setf *post-context-init* nil))

(defun initialize-all-delayed (thunks)
  (let ((delayed-further (reduce #'initialize-delayed thunks
				 :initial-value nil)))
    (when delayed-further
      (initialize-all-delayed delayed-further))))

;;----------------------------------------------------------------------

(defun initialize-delayed (delay-again item)
  (let ((still-waiting-on
	 (remove-if #'initialized-p (delayed-waiting-on item))))
    (if still-waiting-on
	(progn
	  (setf (delayed-waiting-on item)
		still-waiting-on)
	  (cons item delay-again))
	(progn
	  (funcall (delayed-thunk item))
	  delay-again))))

(defmacro if-context (init-func-call pre-context-form &optional depends-on)
  (let ((pre (cepl-utils:symb :%pre%)))
    `(let ((,pre ,pre-context-form))
       (if cepl.context:*gl-context*
	   (let ((,pre ,pre))
	     ,init-func-call)
	   (cepl.memory::delay-initialization (lambda () ,init-func-call)
					      ,depends-on))
       ,pre)))

(in-package :cepl.context)

;;----------------------------------------------------------------------
;; Delayed resource initialization

(defvar *post-context-init* nil)
(defvar *on-context* nil)

(defstruct delayed
  (waiting-on nil :type list)
  (thunk (error "delayed must have a constructor thunk")
         :type function))

(defun delay-initialization (cepl-context init-thunk waiting-on-these-resources)
  (with-slots (uninitialized-resources) cepl-context
    (push (make-delayed :waiting-on waiting-on-these-resources
                        :thunk init-thunk)
          uninitialized-resources))
  t)

(defun initialize-all-delayed (thunks)
  (let ((delayed-further (reduce #'initialize-delayed thunks
                                 :initial-value nil)))
    (when delayed-further
      (initialize-all-delayed delayed-further))))


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

(defmacro if-gl-context (init-func-call pre-context-form &optional depends-on)
  (let ((pre (cepl-utils:symb :%pre%)))
    `(let ((,pre ,pre-context-form))
       (if (slot-value *cepl-context* 'gl-context)
           (let ((,pre ,pre))
             ,init-func-call)
           (delay-initialization
            *cepl-context*
            (lambda () ,init-func-call)
            ,depends-on))
       ,pre)))

;;----------------------------------------------------------------------

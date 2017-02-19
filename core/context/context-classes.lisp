(in-package :cepl.context)

(defclass gl-context ()
  ((cache :initform (make-hash-table))
   (handle :initarg :handle :reader handle)
   (window :initarg :window :reader window)
   (fbo :initarg :window :reader context-fbo)))

(defconstant +unknown-id+ -1)

;; Caching details
;; ---------------
;; IDs: Unknown = -1
(defclass cepl-context ()
  ((uninitialized-resources :initform nil)
   (gl-context :initform nil)
   (array-buffer-binding-id :initform +unknown-id+
                            :type (signed-byte 32))
   (element-array-buffer-binding-id :initform +unknown-id+
                                    :type (signed-byte 32))
   (gpu-buffers :initform (make-array 0 :element-type 'gpu-buffer
                                      :initial-element +null-gpu-buffer+
                                      :adjustable t
                                      :fill-pointer 0))))

(defvar *cepl-context*
  (make-instance 'cepl-context))

;;----------------------------------------------------------------------

(defun register-gpu-buffer (cepl-context gpu-buffer)
  (with-slots (gpu-buffers) cepl-context
    (let ((id (gpu-buffer-id gpu-buffer)))
      (when (<= (array-dimension gpu-buffers 0) id)
        (adjust-array gpu-buffers (1+ id) :initial-element +null-gpu-buffer+))
      (when (<= (fill-pointer gpu-buffers) id)
        (setf (fill-pointer gpu-buffers) (1+ id)))
      (setf (aref gpu-buffers id) gpu-buffer))))

;;----------------------------------------------------------------------

(defun buffer-bound (cepl-context target)
  (ecase target
    (:array-buffer (array-buffer-bound cepl-context))
    (:element-array-buffer (element-array-buffer-bound cepl-context))))

(defun (setf buffer-bound) (value cepl-context target)
  (ecase target
    (:array-buffer (setf (array-buffer-bound cepl-context) value))
    (:element-array-buffer (setf (element-array-buffer-bound cepl-context) value))))

(define-compiler-macro buffer-bound (&whole whole cepl-context target)
  (case target
    (:array-buffer `(array-buffer-bound ,cepl-context))
    (:element-array-buffer `(element-array-buffer-bound ,cepl-context))
    (otherwise whole)))

;;----------------------------------------------------------------------

(defun array-buffer-bound (cepl-context)
  (with-slots (gl-context gpu-buffers array-buffer-binding-id) cepl-context
    (let* ((id (if (= array-buffer-binding-id +unknown-id+)
                   (setf array-buffer-binding-id
                         (array-buffer-binding gl-context))
                   array-buffer-binding-id))
           (buffer (when (> id 0) (aref gpu-buffers id))))
      (assert (not (eq buffer +null-gpu-buffer+)))
      buffer)))

(defun (setf array-buffer-bound) (gpu-buffer cepl-context)
  (with-slots (gl-context gpu-buffers array-buffer-binding-id) cepl-context
    (let ((id (if gpu-buffer
                  (gpu-buffer-id gpu-buffer)
                  0)))
      (setf (array-buffer-binding gl-context) id
            array-buffer-binding-id id)
      gpu-buffer)))

;;----------------------------------------------------------------------

(defun element-array-buffer-bound (cepl-context)
  (with-slots (gl-context gpu-buffers element-array-buffer-binding-id)
      cepl-context
    (let* ((id (if (= element-array-buffer-binding-id +unknown-id+)
                   (setf element-array-buffer-binding-id
                         (element-array-buffer-binding gl-context))
                   element-array-buffer-binding-id))
           (buffer (when (> id 0) (aref gpu-buffers id))))
      (assert (not (eq buffer +null-gpu-buffer+)))
      buffer)))

(defun (setf element-array-buffer-bound) (gpu-buffer cepl-context)
  (with-slots (gl-context gpu-buffers element-array-buffer-binding-id)
      cepl-context
    (let ((id (if gpu-buffer
                  (gpu-buffer-id gpu-buffer)
                  0)))
      (setf (element-array-buffer-binding gl-context) id
            element-array-buffer-binding-id id)
      gpu-buffer)))

;;----------------------------------------------------------------------

(defun on-gl-context (cepl-context new-gl-context)
  (with-slots (gl-context uninitialized-resources) cepl-context
    (setf gl-context new-gl-context)
    (initialize-all-delayed uninitialized-resources)
    (setf uninitialized-resources nil)))

;;----------------------------------------------------------------------
;; Delayed resource initialization

(defvar *post-context-init* nil)

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

(defmacro cepl.memory::if-context (init-func-call pre-context-form &optional depends-on)
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

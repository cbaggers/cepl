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

;;------------------------------------
;; Time compiler internals
;;------------------------------------

(defun %compile-tprogn-form (form)
  (if (atom form) ;; if lisp literal then it is fine
      (make-instance 't-compile-obj :code form :run-test t)
      (case (first form) ;; handler special forms and t-syntax
        (repeat (tthen/repeat (rest form) :repeat t))
        (then (tthen/repeat (rest form) :repeat nil))
        (progn (tprogn (rest form)))
        (otherwise (%process-tform form)))))

(defun %process-tform (form)
  (if (or (atom (first form))
          (and (listp (first form)) (eq (caar form) 'lambda)))
      (make-instance 't-compile-obj :code form :run-test t)
      (%compile-time-cond-form form)))

(defun %compile-time-cond-form (form)
  (if (atom form)
      (make-instance 't-compile-obj :code form)
      (let* ((condition-form (first form))
             (compiled-form (compile-time-syntax condition-form))
             (compiled-body (tprogn (rest form))))
        (with-t-obj () compiled-form
          (make-instance
           't-compile-obj
           :initialize (append initialize (t-initialize compiled-body))
           :code (t-code compiled-body)
           :run-test run-test
           :expired-test expired-test 
           ;; {TODO} Here we essentially throw away the expiry
           ;; tests of the other forms...seems dumb
           :end-time end-time
           :local-vars local-vars
           :closed-vars (append closed-vars (t-closed-vars compiled-body))
           :initialize initialize)))))

;;------------------------------------
;; Temporal Functions Top Level
;;------------------------------------

;; {TODO} cant make simple live version as on every recompile all gensyms will
;;        be different. Need to cache the original start time and use that to 
;;        offset all ops
;; {TODO} can use func-name for block if tdefun
(defun gen-time-function-body (name args body)
  (with-t-obj () (tprogn body)
    (unless (loop :for i :in closed-vars :never (symbol-package i))
      (error "All closed-vars must be gensym'd to avoid macro variable capture"))
    (let ((first-run-sym (gensym "first-run")))
      `(let* ((,current-time-sym (funcall *default-time-source*))
              (,overflow-sym (funcall *default-time-source*))
              ,@(loop :for (a b) :in closed-vars
                   :collect (list a b))
              (,first-run-sym t))
         (declare (ignorable ,current-time-sym ,overflow-sym))
         (,@(if name `(defun ,name) '(lambda)) ,args     
            (setf ,current-time-sym (funcall *default-time-source*))
            (block tlambda-implicit-block
              (tagbody tlambda-start
                 (return-from tlambda-implicit-block
                   (progn
                     (when ,first-run-sym 
                       (setf ,first-run-sym nil)
                       ,@initialize) 
                     (let ,local-vars
                       ,(if expired-test
                            `(if ,expired-test
                                 (cfunc:signal-expired)
                                 ,code)
                            code)))))))))))

(defmacro tdefun (name args &body body) (gen-time-function-body name args body))
(defmacro tlambda (args &body body) (gen-time-function-body nil args body))

;;------------------------------------
;; Time compiler special forms
;;------------------------------------

(defmacro then (&body temporal-statements)
  (declare (ignore temporal-statements))
  (error "'Then' can only be used inside a tlambda*"))
(defmacro repeat (&body temporal-statements)
  (declare (ignore temporal-statements))
  (error "'Repeat' can only be used inside a tlambda*"))

(defun tprogn (forms)
  (let* ((processed-forms (loop :for f :in forms :collect 
                             (%compile-tprogn-form f))))
    (make-instance
     't-compile-obj
     :code
     `(progn
        ,@(loop :for form :in processed-forms :collect
             (with-t-obj () form
               (if (and (eq run-test t) (null expired-test))
                   `(let ,(remove nil local-vars) ,code)
                   `(let ,(remove nil local-vars)
                      (when (and (not ,expired-test) ,run-test)
                        ,code))))))
     :run-test t
     :expired-test (let ((expire-forms (mapcar #'t-expired-test processed-forms)))
                     (when (every #'identity expire-forms)
                       `(and ,@expire-forms)))
     :closed-vars (remove nil (mapcan #'t-closed-vars processed-forms))
     :initialize (mapcan #'t-initialize processed-forms))))


(defun tthen/repeat (forms &key repeat)
  (let* ((compiled-forms (loop :for f :in forms :collect
                            (%compile-tprogn-form f)))
         (counter-sym (gensym "counter"))
         (closed-vars (remove nil (mapcan #'t-closed-vars compiled-forms)))
         (chained (when repeat `(go tlambda-start)))
         (%compiled-forms (cons (make-instance 
                                 't-compile-obj
                                 :initialize (when repeat
                                               (t-initialize 
                                                (first compiled-forms))))
                                (reverse compiled-forms))))
    (loop :for index :from 1 :below (length %compiled-forms) :do
       (setf chained (chain-template (nth index %compiled-forms) 
                                     (nth (1- index) %compiled-forms)
                                     chained counter-sym 
                                     (- (length %compiled-forms) (1+ index))
                                     (if (and repeat (= index 1))
                                         `(setf ,counter-sym 0)
                                         `(incf ,counter-sym)))))
    (make-instance
       't-compile-obj
       :code chained
       :run-test `(< ,counter-sym ,(length forms))
       :expired-test (when (not repeat) `(= ,counter-sym ,(length forms)))
       :closed-vars (cons `(,counter-sym 0) closed-vars)
       :initialize `(,@(t-initialize (first compiled-forms))))))

(defun chain-template (current-step-ob next-step-ob next-body
                       counter-sym current-step-num counter-step-form)
  (with-t-obj ('c) current-step-ob
    (with-t-obj ('n) next-step-ob
      `(let ,local-vars-c
         (if ,expired-test-c
             (let ((,overflow-sym ,overflow-sym))
               (progn 
                 (when (= ,counter-sym ,current-step-num)
                   ,counter-step-form
                   ,(when end-time-c `(setf ,overflow-sym ,end-time-c))
                   ,@initialize-n)
                 ,next-body))
             (when ,run-test-c ,code-c))))))

;;-------------------
;; Time conditionals
;;-------------------

(deftsyntax and (&rest forms)
  (let ((forms (mapcar #'compile-time-syntax forms)))
    (make-instance 
     't-compile-obj
     :run-test `(and ,@(remove nil (mapcar #'t-run-test forms)))
     :expired-test `(or ,@(remove nil (mapcar #'t-expired-test forms)))
     :end-time `(min ,@(mapcar #'t-end-time forms))
     :local-vars (remove nil (mapcan #'t-local-vars forms))
     :closed-vars (mapcan #'t-closed-vars forms)
     :initialize (mapcan #'t-initialize forms))))

(deftsyntax or (&rest forms)
  (let ((forms (mapcar #'compile-time-syntax forms)))
    (make-instance 
     't-compile-obj
     :run-test `(or ,@(remove nil (mapcar #'t-expired-test forms)))
     :expired-test `(and ,@(remove nil (mapcar #'t-run-test forms)))
     :end-time `(max ,@(mapcar #'t-end-time forms))
     :local-vars (remove nil (mapcan #'t-local-vars forms))
     :closed-vars (mapcan #'t-closed-vars forms)
     :initialize (mapcan #'t-initialize forms))))

(deftsyntax after (deadline)
  (let ((deadsym (gensym "deadline")))
    (make-instance
     't-compile-obj
     :initialize `((setf ,deadsym (- ,deadline ,overflow-form)))
     :run-test `(afterp ,deadsym)
     :closed-vars `((,deadsym 0.0)))))

(deftsyntax before (deadline &key progress)
  (unless (symbolp progress) (error "'progress' in 'each' must be a symbol"))
  (let* ((deadsym (gensym "deadline"))         
         (stimesym (gensym "start-time")))
    (make-instance
     't-compile-obj
     :initialize `((setf ,deadsym (- ,deadline ,overflow-form))
                   ,@(when progress `((setf ,stimesym ,overflow-sym))))
     :run-test (if progress
                   `(progn
                      (setf ,progress (float (- 1.0 (/ (- ,deadsym ,current-time-sym)
                                                       (- ,deadsym ,stimesym)))))
                      (beforep ,deadsym))
                   `(beforep ,deadsym))
     :end-time deadsym
     :expired-test `(afterp ,deadsym)
     :local-vars (when progress `((,progress 0.0))) ;; cant this have the calculation?
     :closed-vars `(,@(when progress `((,stimesym ,current-time-sym)))
                      (,deadsym 0.0)))))

(deftsyntax between (start-time end-time &key progress)
  (unless (symbolp progress) (error "'progress' in 'each' must be a symbol"))
  (let* ((deadsym (gensym "deadline"))         
         (stimesym (gensym "start-time")))
    (make-instance
     't-compile-obj
     :initialize `((setf ,deadsym (- ,end-time ,overflow-form))
                   (setf ,stimesym (- ,start-time ,overflow-form)))
     :run-test (if progress
                   `(progn
                      (setf ,progress (float (- 1.0 (/ (- ,deadsym ,current-time-sym)
                                                       (- ,deadsym ,stimesym)))))
                      (betweenp ,stimesym ,deadsym))
                   `(betweenp ,stimesym ,deadsym))
     :end-time deadsym
     :expired-test `(afterp ,deadsym)
     :local-vars (when progress `((,progress 0.0))) ;; cant this have the calculation?
     :closed-vars `(,@(when progress `((,stimesym ,current-time-sym)))
                      (,deadsym 0.0)))))

(deftsyntax each (timestep &key step-var fill-var max-cache-size)
  (unless (symbolp step-var) (error "step-var in 'each' must be a symbol"))
  (unless (symbolp fill-var) (error "fill-var in 'each' must be a symbol"))  
  (let ((stepv (gensym "stepper")))
    (make-instance
     't-compile-obj
     :run-test (if fill-var
                   `(setf ,fill-var (funcall ,stepv))
                   `(funcall ,stepv))
     :local-vars `(,(when step-var `(,step-var ,timestep))
                    ,(when fill-var `(,fill-var ,stepv)))
     :closed-vars `((,stepv (make-stepper ,timestep 
                                          ,@(when max-cache-size 
                                                  (list max-cache-size))))))))

(deftsyntax once ()
  (let ((runsym (gensym "run"))
        (end-time (gensym "end-time")))
    (make-instance 
     't-compile-obj
     :initialize `((setf ,runsym nil))
     :run-test `(unless ,runsym (setf ,runsym t))
     :expired-test `(progn (setf ,end-time ,current-time-sym) runsym)
     :end-time end-time
     :closed-vars `((,runsym nil)
                    (,end-time ,overflow-sym)))))

(deftsyntax while (test)
  (let ((test-result (gensym "test-result"))
        (end-time (gensym "end-time")))
    (make-instance
     't-compile-obj
     :run-test test-result
     :expired-test `(progn (setf ,end-time ,current-time-sym) (not ,test-result))
     :end-time nil ;; this is set as a side effect by the expired test
     :local-vars `((,test-result ,test)
                   (,end-time ,overflow-sym)))))

;;==============================================================================
;; Below is not part of the time compiler but can be used with it
;;==============================================================================

;;----------------------------------------------------------------------
;; Time units
;;------------

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
            :append `((defun ,type (quantity) (floor (* quantity ,count))))
            :do (push (cons type count) defined)))))

(def-time-units
  (milliseconds 1)
  (seconds (milliseconds 1000))
  (minutes (seconds 60))
  (hours (minutes 60)))

;;--------------------------------------------------------------------

(defparameter *default-time-source* #'get-internal-real-time)

(defun make-time-source (&key (parent *default-time-source*) (transform nil))
  (if transform
      (lambda () (funcall transform (funcall parent)))
      (lambda () (funcall parent))))

(defmacro with-time-source (time-source &body body)
  `(let ((*default-time-source* ,time-source))
     ,@body))

;;--------------------------------------------------------------------

;;{TODO} add compiler macros to inline default timesource when not specified
;;{TODO} the return is an issue

(defun from-now (time-offset &optional (time-source *default-time-source*))
  (+ time-offset (funcall time-source)))

(defun beforep (time &optional (time-source *default-time-source*))
  (< (funcall time-source) time))

(defun afterp (time &optional (time-source *default-time-source*))
  (> (funcall time-source) time))

(defun betweenp (start-time end-time
                 &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (and (>= current-time start-time)
         (<= current-time end-time))))

;;--------------------------------------------------------------------

(defun make-stepper (step-size &optional (max-cache-size (max (* 10 step-size) 10000.0))
                                 (default-source *default-time-source*))
  "this takes absolute sources"
  ;; if max-cache-size is set to zero
  (when (< max-cache-size step-size)
    (error "Make-Stepper: max-cache-size is smaller than step-size.~%max-cache-size: ~a~%step-size: ~a~%" max-cache-size step-size))
  (let ((time-cache 0)
        (last-val (funcall default-source)))
    (lambda (&optional (time-source default-source))
      (let* ((time (abs (funcall time-source)))
             (dif (- time last-val)))
        (setf last-val time)
        (setf time-cache (min max-cache-size (+ time-cache dif)))
        (when (>= time-cache step-size)
          (setf time-cache (- time-cache step-size))
          (min 1.0 (/ time-cache step-size)))))))

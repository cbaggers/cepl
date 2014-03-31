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

;; -----------------------------------------------------------------------------
;; + tlambda is the top level and triggers the expire signal
;;
;; + tprogn has given back one progn with multiple if forms, one for
;;   each step. It also returns the expire-test that check that they have all
;;   expired
;;
;; + %process-tprogn-forms either calls %compile-time-syntax, handles the 
;;   time-special-forms or if it is a lisp statement, returns it correctly.
;;   it returns a list of tcompile-objs so is useful in multiple special forms
;;
;; + time-syntax-expand takes a form and calls the function named by the first
;;   item in the form (in the time-syntax package). IT passes the rest of the 
;;   form as args to that function
;;
;; + %compile-time-syntax takes a form in the format ((condition) &rest body)
;;   and returns the result in the correct format for other functions to use.
;;   It does not wrap in if statements or handle any of the expiry as that 
;;   should be done by the special forms (or the tlambda root form)
;;
;; + ~time-syntax~ these functions return a list of data in the following order:
;;   code, expired-test, local-vars, closed-vars
;; -----------------------------------------------------------------------------

(defmacro tlambda (args &body body)
  ;;COPY CODE FROM TLAMBDA-DEBUG WHEN WORKING
  (apply #'tlambda-debug (cons args body)))
;; * tlambda has to have an expired-test as only it may release the expired signal
;;   otherwise we have to have a web of event handlers
(defun tlambda-debug (args &rest body)
  (with-t-obj () (tprogn body)
    `(let (,@closed-vars)
       (when (or expired-test local-vars run-test ) 
         (format t "unhanded~%---------~%local-vars:~s~%run-test:~s"
                 local-vars run-test))
       (lambda ,args
         `(if ,expired-test
              (cfunc:signal-expired)
              ,code)))))

;;------------------------------------
;; Time compiler internals
;;------------------------------------

(defun %process-tprogn-forms (forms)
  (loop :for form :in forms :collect
     (if (atom form) ;; if lisp literal then it is fine
         (make-instance 'tcompile-obj :code form :run-test t)
         (case (first form) ;; handler special forms and t-syntax 
           (repeat (trepeat (rest form)))
           (then (tthen (rest form)))
           (progn (tprogn (rest form)))
           (otherwise 
            (if (or (atom (first form)) 
                    (and (listp (first form)) (eq (caar form) 'lambda)))
                (make-instance 'tcompile-obj :code form :run-test t)
                (%compile-time-syntax form)))))))

(defun time-syntaxp (name)
  (symbol-function (symbolicate-package :time-syntax name)))
(defun time-syntax-expand (form)
  (let ((name (first form))
        (rest (rest form)))
    (apply (symbol-function (symbolicate-package :time-syntax name)) rest)))
(defun t-chain-expand (counter forms)
  (loop :for (expire-test run-test code initialize-next) :in (reverse forms)
     :for i :from (1- (length forms)) :downto 0 :with accum = nil :do
     (setf accum `(if ,expire-test
                      (progn (when (= ,counter ,i) ,@initialize-next) ,accum)
                      (when ,run-test ,code)))
     :finally (return accum)))

(defun %compile-time-syntax (form) ;;((condition) (body1) ..etc)
  (if (atom form)
      (make-instance 'tcompile-obj :code form)
      (let* ((condition-form (first form))             
             (expanded-form (time-syntax-expand condition-form))
             (compiled-body (tprogn (rest form))))
        (destructuring-bind 
              (&optional run-test expired-test local-vars closed-vars override)
            expanded-form
          (make-instance 
           'tcompile-obj
           :code (t-code compiled-body)
           :run-test run-test
           :expired-test (if (and (symbolp expired-test) 
                                  (t-expired-test compiled-body))
                             (cons expired-test (t-expired-test compiled-body))
                             expired-test)
           :local-vars local-vars
           :closed-vars (append closed-vars (t-closed-vars compiled-body))
           :override override)))))

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
  (let* ((processed-forms (%process-tprogn-forms forms))
         (expire-forms (remove nil (mapcar #'t-expired-test processed-forms)))
         (let-forms (remove nil (mapcan #'t-local-vars processed-forms))))
    (make-instance 
     'tcompile-obj
     :code `(let ,let-forms
              ,@(loop :for form :in processed-forms :collect
                   (with-t-obj () form
                     (if (and (eq run-test t) (null expired-test))
                         code
                         `(when ,run-test ,code))))) ;; this should be an if
     :run-test t
     :expired-test (when expire-forms `(and ,@expire-forms))
     :closed-vars (remove nil (mapcan #'t-closed-vars processed-forms)))))

(defun tthen (forms)
  (unless (null forms)
    (let* ((processed-forms (%process-tprogn-forms forms))
           (counter-sym (gensym "counter"))
           (%closed-vars (remove nil (mapcan #'fifth (copy-tree processed-forms))))
           (let-forms (remove nil (mapcar #'fourth processed-forms)))
           (chain-forms
            (loop :for (body test expired-test inner-let closed-vars overridep)
               :in processed-forms :for index :from 0
               :do (print index)
               :collect
               (list expired-test
                     test
                     body
                     ;;when (< index (1- (length forms))) {TODO} why?
                     (cons `(incf ,counter-sym)
                           (loop :for i :in (fifth (nth (+ index 1) processed-forms))
                              :collect `(setf ,(first i) (second i)
                                              ,@(when overridep
                                                      `((:override ,expired-test))))))))))
      (list `(let ,(when let-forms (list let-forms))
               ,(t-chain-expand counter-sym (print chain-forms)))
            `(< ,counter-sym ,(length forms))
            `(= ,counter-sym ,(length forms))
            nil
            (cons `(,counter-sym 0)
                  (cons (first %closed-vars)
                        (mapcar #'listify (rest %closed-vars))))))))

(defun trepeat (forms)
  (let* ((processed-forms (%process-tprogn-forms forms))
         (counter-sym (gensym "counter"))
         (%closed-vars (remove nil (mapcan #'fifth (copy-tree processed-forms))))
         (let-forms (remove nil (mapcar #'third (copy-list processed-forms))))
         (chain-forms
          (loop :for (body test expired-test) :in processed-forms
             :for index :from 0 :collect
             (list expired-test test body
                   (if (= index (1- (length forms)))
                       (cons `(setf ,counter-sym 0)
                             (loop :for i :in (fifth (first processed-forms)) :collect `(setf ,@i)))
                       (cons `(incf ,counter-sym)
                             (loop :for i :in (fifth (nth (+ index 1) processed-forms)) :collect `(setf ,@i))))))))
    (list `(let ,(when let-forms (list let-forms))
             ,(t-chain-expand counter-sym chain-forms))
          t
          nil
          nil
          (cons `(,counter-sym 0)
                (cons (first %closed-vars)
                      (mapcar #'listify (rest %closed-vars)))))))

;;------------------------------------
;; Time compiler syntax
;;------------------------------------

(add-time-syntax and (&rest forms) nil (list `(and ,@forms) 'and))
(add-time-syntax or (&rest forms) nil (list `(or ,@forms) 'or))

(add-time-syntax once () nil
  (let ((runsym (gensym "run")))
    (list `(unless ,runsym (setf ,runsym t))
          runsym
          nil
          `((,runsym nil)))))

(add-time-syntax after (deadline &key override) t `(afterp ,deadline))

(add-time-syntax before (deadline &key progress override) t
  (unless (symbolp progress) (error "'progress' in 'each' must be a symbol"))
  (let ((deadsym (gensym "deadline"))
        (ctimesym (gensym "current-time"))
        (stimesym (gensym "start-time")))
    (list
     (if progress
         `(let ((,deadsym ,deadline)
                (,ctimesym (funcall *default-time-source*)))
            (setf ,progress (float (- 1.0 (/ (- ,deadsym ,ctimesym)
                                             (- ,deadsym ,stimesym)))))
            (beforep ,deadsym))
         `(beforep ,deadline))
     `(afterp ,deadline)
     (when progress `((,progress 0.0)))
     (when progress `((,stimesym (funcall *default-time-source*)))))))

(add-time-syntax between (start-time end-time &key progress override) t
  (let ((deadsym (gensym "deadline"))
        (ctimesym (gensym "current-time")))
    (list (if progress
              `(let ((,deadsym ,end-time)
                     (,ctimesym (funcall *default-time-source*)))
                 (setf ,progress (float (- 1.0 (/ (- ,deadsym ,ctimesym)
                                                  (- ,deadsym ,start-time)))))
                 (betweenp ,start-time ,end-time))
              `(betweenp ,start-time ,end-time))
          `(afterp ,end-time)
          (when progress `((,progress 0.0))))))

(add-time-syntax from-now (offset) nil
  (let ((offsetv (gensym "offset")))
    (list offsetv nil `((,offsetv (from-now ,offset))))))

(add-time-syntax each (timestep &key step-var fill-var) nil
  (unless (symbolp step-var) (error "step-var in 'each' must be a symbol"))
  (unless (symbolp fill-var) (error "fill-var in 'each' must be a symbol"))
  (let ((stepv (gensym "stepper")))
    (list (if fill-var
              `(setf ,fill-var (funcall ,stepv))
              `(funcall ,stepv))
          nil
          `(,(when step-var `(,step-var ,timestep))
             ,(when fill-var `(,fill-var ,stepv)))
          `((,stepv (make-stepper ,timestep))))))

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
            :append `((defun ,type (quantity) (floor (* quantity ,count)))
                      (add-time-syntax ,type (quantity) nil ;; {TODO} what is this for?
                        (list ',type quantity)))
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

(defun from-now (time-offset &optional (time-source *default-time-source*))
  (+ time-offset (funcall time-source)))

(defun beforep (time &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (< current-time time) time)))

(defun afterp (time &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (> current-time time) current-time)))

(defun betweenp (start-time end-time
                 &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (and (>= current-time start-time)
               (<= current-time end-time))
      end-time)))

;;--------------------------------------------------------------------

(defun make-stepper (step-size &optional (default-source *default-time-source*))
  "this takes absolute sources"
  (let ((time-cache 0)
        (last-val (funcall default-source)))
    (lambda (&optional (time-source default-source))
      (if (eq time-source t)
          step-size
          (let* ((time (abs (funcall time-source)))
                 (dif (- time last-val)))
            (setf last-val time)
            (incf time-cache dif)
            (when (> time-cache step-size)
              (setf time-cache (- time-cache step-size))
              (min 1.0 (/ time-cache step-size))))))))

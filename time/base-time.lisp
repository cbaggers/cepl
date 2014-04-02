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
;; + mapcar %process-tprogn-form either calls %compile-time-cond-form, handles the
;;   time-special-forms or if it is a lisp statement, returns it correctly.
;;
;; + time-syntax-expand takes a form and calls the function named by the first
;;   item in the form (in the time-syntax package). IT passes the rest of the
;;   form as args to that function
;;
;; + %compile-time-cond-form takes a form in the format ((condition) &rest body)
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
    ;; (when (or expired-test local-vars run-test)
    ;;   (format t "unhanded~%---------~%local-vars:~s~%run-test:~s"
    ;;           local-vars (when (not (eq t run-test)) run-test)))
    (let ((lbody (if expired-test
                      `(if ,expired-test
                           (cfunc:signal-expired)
                           ,code)
                      code)))
      `(let (,@(loop :for (a b) :in closed-vars collect (list a b)))
         (lambda ,args
           (block tlambda-implicit-block
             (tagbody tlambda-start
                (return-from tlambda-implicit-block
                  ,lbody))))))))

;;------------------------------------
;; Time compiler internals
;;------------------------------------

(defun %process-tprogn-form (form)
  (if (atom form) ;; if lisp literal then it is fine
      (make-instance 'tcompile-obj :code form :run-test t)
      (case (first form) ;; handler special forms and t-syntax
        (repeat (tthen/repeat (rest form) :repeat t))
        (then (tthen/repeat (rest form) :repeat nil))
        (progn (tprogn (rest form)))
        (otherwise (%process-tform form)))))

(defun %process-tform (form)
  (if (or (atom (first form))
          (and (listp (first form)) (eq (caar form) 'lambda)))
      (make-instance 'tcompile-obj :code form :run-test t)
      (%compile-time-cond-form form)))

(defun time-syntaxp (name)
  (symbol-function (symbolicate-package :time-syntax name)))
(defun time-syntax-expand (form)
  (let ((name (first form))
        (rest (rest form)))
    (apply (symbol-function (symbolicate-package :time-syntax name)) rest)))
(defun t-chain-expand (counter forms &optional accum)
  (loop :for (expire-test run-test code initialize-next) :in (reverse forms)
     :for i :from (1- (length forms)) :downto 0 :do
     (setf accum `(if ,expire-test
                      (progn (when (= ,counter ,i) ,@initialize-next) ,accum)
                      (when ,run-test ,code)))
     :finally (return accum)))

(defun %compile-time-cond-form (form)
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
  (let* ((processed-forms (mapcar #'%process-tprogn-form forms))
         (let-forms (remove nil (mapcan #'t-local-vars processed-forms))))
    (make-instance
     'tcompile-obj
     :code `(let ,let-forms
              ,@(loop :for form :in processed-forms :collect
                   (with-t-obj () form
                     (if (and (eq run-test t) (null expired-test))
                         code
                         `(when (and ,run-test (not ,expired-test))
                            ,code))))) ;; this should be an if
     :run-test t
     :expired-test (let ((expire-forms
                          (remove nil (mapcar #'t-expired-test processed-forms))))
                     (when expire-forms `(and ,@expire-forms)))
     :closed-vars (remove nil (mapcan #'t-closed-vars processed-forms)))))

(defun tthen/repeat (forms &key repeat)
  (unless (null forms)
    (let* ((processed-forms (mapcar #'%process-tprogn-form forms))
           (counter-sym (gensym "counter"))
           (%closed-vars (remove nil (mapcan #'t-closed-vars processed-forms)))
           (let-forms (remove nil (mapcan #'t-local-vars processed-forms)))
           (chain-forms
            (loop :for form :in processed-forms :for index :from 0
               :collect
               (with-t-obj () form
                 (if repeat
                     (list expired-test
                           run-test
                           code
                           (if (= index (1- (length forms)))
                               `((setf ,counter-sym 0)                                 
                                 ,@(loop :for i :in (t-closed-vars (first processed-forms))
                                      :if (and override (third i))
                                      :collect `(setf ,(first i) (- ,(second i) (- (funcall *default-time-source*) ,expired-test)))
                                      :else 
                                      :collect `(setf ,(first i) ,(second i)))
                                 ,(when repeat '(go tlambda-start)))
                               (cons `(incf ,counter-sym)
                                     (loop :for i :in (t-closed-vars 
                                                       (nth (+ index 1) processed-forms))
                                        :if (and override (third i))
                                        :collect `(setf ,(first i) (- ,(second i) (- (funcall *default-time-source*) ,expired-test)))
                                        :else 
                                        :collect `(setf ,(first i) ,(second i))))))
                     (list expired-test
                           run-test
                           code
                           (if (< index (1- (length forms)))
                               (cons `(incf ,counter-sym)
                                     (loop :for i :in (t-closed-vars
                                                       (nth (+ index 1) processed-forms))
                                        :if (and override (third i))
                                        :collect `(setf ,(first i) (- ,(second i) (- (funcall *default-time-source*) ,expired-test)))
                                        :else 
                                        :collect `(setf ,(first i) ,(second i)))))))))))
      (make-instance
       'tcompile-obj
       :code `(let ,(when let-forms (list let-forms))
                ,(t-chain-expand counter-sym chain-forms))
       :run-test `(< ,counter-sym ,(length forms))
       :expired-test (when (not repeat) `(= ,counter-sym ,(length forms)))
       :closed-vars (cons `(,counter-sym 0)
                          (cons (first %closed-vars)
                                (mapcar #'listify (rest %closed-vars))))))))

;;------------------------------------
;; Time compiler syntax
;;------------------------------------

(def-time-condition and (&rest forms) nil (list `(and ,@forms) 'and))
(def-time-condition or (&rest forms) nil (list `(or ,@forms) 'or))

(def-time-condition once () nil
  (let ((runsym (gensym "run")))
    (list `(unless ,runsym (setf ,runsym t))
          runsym
          nil
          `((,runsym nil)))))

(def-time-condition after (deadline) t
  (let ((deadsym (gensym "deadline")))
    (list `(afterp ,deadsym)
          nil
          nil
          `((,deadsym ,deadline t)))))

(def-time-condition before (deadline &key progress) t
  (unless (symbolp progress) (error "'progress' in 'each' must be a symbol"))
  (let* ((deadsym (gensym "deadline"))
         (ctimesym (gensym "current-time"))
         (stimesym (gensym "start-time")))
    (list
     (if progress
         `(let ((,ctimesym (funcall *default-time-source*)))
            (setf ,progress (float (- 1.0 (/ (- ,deadsym ,ctimesym)
                                             (- ,deadsym ,stimesym)))))
            (beforep ,deadsym))
         `(beforep ,deadsym))
     `(afterp ,deadsym)
     (when progress `((,progress 0.0)))
     `(,@(when progress `((,stimesym (funcall *default-time-source*))))
         ,@`((,deadsym ,deadline t))))))

(def-time-condition between (start-time end-time &key progress) t
  (let ((deadsym (gensym "deadline"))
        (ctimesym (gensym "current-time")))
    (list (if progress
              `(let ((,ctimesym (funcall *default-time-source*)))
                 (setf ,progress (float (- 1.0 (/ (- ,deadsym ,ctimesym)
                                                  (- ,deadsym ,start-time)))))
                 (betweenp ,start-time ,end-time))
              `(betweenp ,start-time ,end-time))
          `(afterp ,end-time)
          (when progress `((,progress 0.0)))
          `((,deadsym ,end-time t)))))

(def-time-condition each (timestep &key step-var fill-var) nil
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
                      (def-time-condition ,type (quantity) nil ;; {TODO} what is this for?
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
;;{TODO} the return is an issue

(defun from-now (time-offset &optional (time-source *default-time-source*))
  (+ time-offset (funcall time-source)))

(defun beforep (time &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (< current-time time) current-time)))

(defun afterp (time &optional (time-source *default-time-source*))
  (when (> (funcall time-source) time) time))

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

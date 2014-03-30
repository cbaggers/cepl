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

(defclass tcompile-obj ()
  ((code :initarg :code)
   (run-test :initarg :run-test)
   (expired-test :initarg :expired-test)
   (local-vars :initarg :local-vars)
   (closed-vars :initarg :closed-vars)
   (override :initarg :override)))

(defmethod merge-tcompile-obj ((a tcompile-obj) (b tcompile-obj))
  (with-slots ((code-a code) (run-test-a run-test) 
                (expired-test-a expired-test) (local-vars-a local-vars)
                (closed-vars-a closed-vars) (override-a override)) a
    (with-slots ((code-b code) (run-test-b run-test) 
                (expired-test-b expired-test) (local-vars-b local-vars)
                (closed-vars-b closed-vars) (override-b override)) b
      (make-instance 
       'tcompile-obj
       :code (append (copy-tree code-a) (copy-tree code-b))
       :run-test `(and ,(copy-tree run-test-a) ,(copy-tree run-test-b))
       :expired-test `(and ,(copy-tree expired-test-a) ,(copy-tree expired-test-b))
       :local-vars (append (copy-tree local-vars-a) (copy-tree local-vars-b))
       :closed-vars (append (copy-tree closed-vars-a) (copy-tree closed-vars-b))
       :override (append (copy-tree override-a) (copy-tree override-b))))))

(defmacro add-time-syntax (name args has-time-override &body body)
  (when has-time-override
    (when (member "&OPTIONAL" args :key #'symbol-name :test #'equal)
      (error "&optional is not allowed in time predicates"))
    (unless (and (member "OVERRIDE" args :key #'symbol-name :test #'equal)
                 (> (or (position "OVERRIDE" args :key #'symbol-name :test #'equal) 0)
                    (or (position "&KEY" args :key #'symbol-name :test #'equal) 
                        (1+ (length args)))))
      (error "Predicates must have a &key argument called 'override'")))
  (let ((args (if has-time-override
                  (subst 'override "OVERRIDE" args 
                         :key #'(lambda (x) (when (symbolp x) (symbol-name x)))
                         :test #'equal)
                  args))
        (body (if has-time-override
                  (subst 'override "OVERRIDE" body
                         :key #'(lambda (x) (when (symbolp x) (symbol-name x)))
                         :test #'equal)
                  body)))
    `(defun ,(symbolicate-package :time-syntax name) ,args
       (append (progn ,@body)
               (list ,has-time-override)))))

(defun time-syntaxp (name) 
  (symbol-function (symbolicate-package :time-syntax name)))
(defun time-syntax-expand (name rest) 
  (apply (symbol-function (symbolicate-package :time-syntax name)) rest))

;;{TODO} I think ther are ways to have conflicting expired, address this
;;       I was slightly off but had the right idea...only this macro must
;;       signal expiry, the other must idicate it so they can be nested
(defmacro tlambda (args &body body)                               
  (destructuring-bind (&optional body test expired-test inner-let closed-vars)
      (tprogn body)
    (declare (ignore test inner-let))
    `(let (,@closed-vars)
       (lambda ,args
         ,(if expired-test              
              `(if ,expired-test
                   (cfunc:signal-expired)
                   ,body)
              body)))))

(defun %compile-time-syntax (form)
  (if (atom form) (list form nil nil nil)
      (let* ((tname (first form))
             (closed-arg-vars nil)
             (arg-expire-tests nil)
             (local-arg-vars nil)
             (body (loop :for item :in (rest form) :collect
                      (destructuring-bind (&optional code expired-test
                                                     local-vars closed-vars
                                                     override)
                          (%compile-time-syntax item)
                        (declare (ignore override))
                        (setf closed-arg-vars (append closed-vars closed-arg-vars))
                        (setf local-arg-vars (append local-vars local-arg-vars))
                        (when expired-test (push expired-test arg-expire-tests))
                        code
                        ))));;{TODO} handle override here
        (destructuring-bind (&optional code expired-test local-vars closed-vars
                                       override) 
            (time-syntax-expand tname body)
          (list code
                ;;{TODO} what if not symb and arg-expire-tests?
                (if (and (symbolp expired-test) arg-expire-tests) 
                    (cons expired-test arg-expire-tests) 
                    expired-test)
                (remove nil (append local-vars (reverse arg-expire-tests)))
                (remove nil (append closed-vars (reverse closed-arg-vars)))
                override)))))

(tlambda () ((before 100) (print "hi")))

(defun tprogn (forms)
  (let* ((processed-forms (%process-tprogn-forms forms))
         (expire-forms (remove nil (mapcar #'third processed-forms)))
         (let-forms (remove nil (mapcan #'fourth processed-forms))))
    (list `(let ,(when let-forms let-forms)
             ,@(loop :for (body test expired-test inner-let closed-vars) 
                  :in processed-forms :collect
                  (if (and (eq test t) (null expired-test))
                      body
                      `(when ,test ,body))))
          t          
          (when expire-forms `(and ,@expire-forms))
          nil
          (remove nil (mapcan #'fifth processed-forms)))))

(defun %process-tprogn-forms (forms)
  (loop :for form :in forms :collect
     (if (atom form) (list form t)
         (case (first form)
           (repeat (trepeat (rest form)))
           (then (tthen (rest form)))
           (progn (tprogn (rest form)))
           (otherwise (if (or (atom (first form))
                              (eq (caar form) 'lambda))
                          (list form t)
                          (cons (second form)
                                (%compile-time-syntax (first form)))))))))

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

(defun t-chain-expand (counter forms)
  (loop :for (expire-test run-test code initialize-next) :in (reverse forms)
     :for i :from (1- (length forms)) :downto 0 :with accum = nil :do
     (setf accum `(if ,expire-test
                      (progn (when (= ,counter ,i) ,@initialize-next) ,accum)
                      (when ,run-test ,code)))
     :finally (return accum)))

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

(defmacro then (&body temporal-statements)
  (declare (ignore temporal-statements))
  (error "'Then' can only be used inside a tlambda*"))
(defmacro repeat (&body temporal-statements)
  (declare (ignore temporal-statements))
  (error "'Repeat' can only be used inside a tlambda*"))

(add-time-syntax and (&rest forms) nil (list `(and ,@forms) 'and))
(add-time-syntax or (&rest forms) nil (list `(or ,@forms) 'or))

(add-time-syntax once () nil
  (let ((runsym (gensym "run")))    
    (list `(unless ,runsym (setf ,runsym t))
          runsym
          nil
          `((,runsym nil)))))

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

(add-time-syntax after (deadline &key override) t `(afterp ,deadline))

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

;; (add-time-syntax the-next (quantity &key override) nil
;;   (let ((offsetv (gensym "offset")))
;;     (list `(beforep ,offsetv) `((,offsetv (from-now ,quantity))))))

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

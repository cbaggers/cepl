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

;;----------------------------------------------------------------------

(defmacro add-time-syntax (name args &body body)
  `(defun ,(symbolicate-package :time-syntax name) ,args
     ,@body))

(defun time-syntaxp (name) 
  (symbol-function (symbolicate-package :time-syntax name)))
(defun time-syntax-expand (name rest) 
  (apply (symbol-function (symbolicate-package :time-syntax name)) rest))

;;{TODO} I think ther are ways to have conflicting expired, address this
;;       I was slightly off but had the right idea...only this macro must
;;       signal expiry, the other must idicate it so they can be nested
(defmacro tlambda (args &body body)
  (destructuring-bind (body test expired-test inner-let closed-vars)
      (tprogn+ body)
    (declare (ignore test inner-let))
    `(let (,@closed-vars)
       (lambda ,args
         ,(if expired-test              
              `(if ,expired-test
                   (cfunc:signal-expired)
                   ,body)
              body)))))

(defun %compile-time-syntax+ (form)
  (if (atom form) (values form nil nil nil)
      (let* ((tname (first form))
             (state nil)
             (expiredp nil)
             (anaphora nil)
             (body (loop :for item :in (rest form) :collect
                      (multiple-value-bind (c e a s) (%compile-time-syntax+ item)
                        (setf state (append s state))
                        (setf anaphora (append a anaphora))
                        (when e (push e expiredp))
                        c))))
        (multiple-value-bind (c s e a) (time-syntax-expand tname body)
          (values c 
                  (if (and (symbolp e) expiredp) (cons e expiredp) e)
                  (remove nil (append a (reverse anaphora)))
                  (remove nil (append s (reverse state))))))))

;; (body test expired-test inner-let closed-vars)

(defun %process-tprogn-forms (forms)
  (loop :for form :in forms :collect
     (if (atom form) (list form t)
         (case (first form)
           (repeat (trepeat+ (rest form)))
           (then (tthen+ (rest form)))
           (progn (tprogn+ (rest form)))
           (otherwise (if (or (atom (first form))
                              (eq (caar form) 'lambda))
                          (list form t)
                          (cons (second form)
                                (multiple-value-list 
                                 (%compile-time-syntax+ (first form))))))))))

(defun tprogn+ (forms)
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

(defun tthen+ (forms)
  (let* ((processed-forms (%process-tprogn-forms forms))
         (counter-sym (gensym "counter"))
         (%closed-vars (remove nil (mapcan #'fifth (copy-tree processed-forms))))
         (let-forms (remove nil (mapcar #'fourth processed-forms))))
    (list `(let ,(when let-forms (list let-forms))
             (case ,counter-sym
               ,@(loop :for (body test expired-test inner-let closed-vars)
                    :in processed-forms
                    :for index :from 0 :collect
                    `(,index 
                      (if ,test 
                          ,body
                          ,(when expired-test
                                 `(when ,expired-test
                                    (incf ,counter-sym)
                                    ,@(when (< index (1- (length forms)))
                                            (loop :for i :in (fifth (nth (+ index 1) processed-forms)) :collect `(setf ,@i)))
                                    nil)))))))
          `(< ,counter-sym ,(length forms))
          `(= ,counter-sym ,(length forms))
          nil
          (cons `(,counter-sym 0)
                (cons (first %closed-vars) 
                      (mapcar #'listify (rest %closed-vars)))))))

(defun trepeat+ (forms) 
  (let* ((processed-forms (%process-tprogn-forms forms))
         (counter-sym (gensym "counter"))
         (%closed-vars (remove nil (mapcan #'fifth (copy-tree processed-forms))))
         (let-forms (remove nil (mapcar #'fourth (copy-list processed-forms)))))
    (list `(let ,(when let-forms (list let-forms))
             (case ,counter-sym
               ,@(loop :for (body test expired-test inner-let closed-vars)
                    :in processed-forms
                    :for index :from 0 :collect
                    `(,index 
                      (if ,test 
                          ,body
                          ,(when expired-test
                                 `(when ,expired-test
                                    ,@(if (= index (1- (length forms)))
                                          (cons `(setf ,counter-sym 0)
                                                (loop :for i :in (fifth (first processed-forms)) :collect `(setf ,@i)))
                                          (cons `(incf ,counter-sym)
                                                (loop :for i :in (fifth (nth (+ index 1) processed-forms)) :collect `(setf ,@i))))
                                    nil)))))))
          t
          nil
          nil
          (cons `(,counter-sym 0)
                (cons (first %closed-vars)
                      (mapcar #'listify (rest %closed-vars)))))))


;; another try at tlambda* logic
(defun compile-tlambda-root-form (form)
  (if (atom form)
      (error "blah")
      (if (listp (first form))
          (compile-default-time-form form)
          (funcall (first form) (rest form)))))

(defun compile-test-body-pairs (test-body-pairs)
  (let* ((compiled (loop :for (test . body) :in test-body-pairs :collect
                      (append (multiple-value-list (%compile-time-syntax test))
                              body))))
    (list 
     (remove nil (mapcan #'second compiled))
     (loop :for (ctest cstate expiredp anaphora . body) :in compiled :collect
        `(let ,(remove nil anaphora)
           (if ,ctest (progn ,@body)
               ,(when expiredp `(when ,expiredp (signal-expired)))))))))

;; {TODO} Write tloop, which is a loop compatible construct with temporal 
;;        features, should expand to loop macro

;; only here for style reasons, bad really
(defmacro then (&body temporal-statements)
  (declare (ignore temporal-statements))
  (error "'Then' can only be used inside a tlambda*"))
(defmacro repeat (&body temporal-statements)
  (declare (ignore temporal-statements))
  (error "'Repeat' can only be used inside a tlambda*"))

(add-time-syntax and (&rest forms) (values `(and ,@forms) nil 'and))
(add-time-syntax or (&rest forms) (values `(or ,@forms) nil 'or))

(add-time-syntax before (deadline &key progress) 
  (unless (symbolp progress) (error "'progress' in 'each' must be a symbol"))
  (let ((deadsym (gensym "deadline"))
        (ctimesym (gensym "current-time"))
        (stimesym (gensym "start-time")))
    (values (if progress
                `(let ((,deadsym ,deadline)
                       (,ctimesym (funcall *default-time-source*)))
                   (setf ,progress (float (- 1.0 (/ (- ,deadsym ,ctimesym) 
                                                    (- ,deadsym ,stimesym)))))
                   (beforep ,deadsym))
                `(beforep ,deadline))
            (when progress `((,stimesym (funcall *default-time-source*))))
            `(afterp ,deadline)
            (when progress `((,progress 0.0))))))

(add-time-syntax once () 
  (let ((runsym (gensym "run")))
    (values `(unless ,runsym (setf ,runsym t))
            `((,runsym nil)) ;;maybe move to last
            runsym
            nil)))

;; (body test expired-test inner-let closed-vars)

(add-time-syntax after (deadline) `(afterp ,deadline))

(add-time-syntax between (start-time end-time &key progress)
  (let ((deadsym (gensym "deadline"))
        (ctimesym (gensym "current-time")))    
    (values (if progress
                `(let ((,deadsym ,end-time)
                       (,ctimesym (funcall *default-time-source*)))
                   (setf ,progress (float (- 1.0 (/ (- ,deadsym ,ctimesym)
                                                    (- ,deadsym ,start-time)))))
                   (betweenp ,start-time ,end-time))
                `(betweenp ,start-time ,end-time))
            nil
            `(afterp ,end-time)
            (when progress `((,progress 0.0))))))

(add-time-syntax from-now (offset)
  (let ((offsetv (gensym "offset")))
    (values offsetv `((,offsetv (from-now ,offset))))))

(add-time-syntax the-next (quantity)
  (let ((offsetv (gensym "offset")))
    (values `(beforep ,offsetv) `((,offsetv (from-now ,quantity))))))

(add-time-syntax each (timestep &key step-var fill-var)
  (unless (symbolp step-var) (error "step-var in 'each' must be a symbol"))
  (unless (symbolp fill-var) (error "fill-var in 'each' must be a symbol"))
  (let ((stepv (gensym "stepper")))
    (values (if fill-var 
                `(setf ,fill-var (funcall ,stepv))
                `(funcall ,stepv)) 
            `((,stepv (make-stepper ,timestep)))
            nil
            `(,(when step-var `(,step-var ,timestep))
               ,(when fill-var `(,fill-var ,stepv))))))

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
                      (add-time-syntax ,type (quantity) 
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
      (lambda () (declare (ignore x)) (funcall transform (funcall parent)))
      (lambda () (declare (ignore x)) (funcall parent))))

(defmacro with-time-source (time-source &body body)
  `(let ((*default-time-source* ,time-source))
     ,@body))

;;--------------------------------------------------------------------

(defun from-now (time-offset &optional (time-source *default-time-source*))
  (+ time-offset (funcall time-source)))

(defun beforep (time &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (< current-time time) current-time)))

(defun afterp (time &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (> current-time time) current-time)))

(defun betweenp (start-time end-time
                &optional (time-source *default-time-source*))
  (let ((current-time (funcall time-source)))
    (when (and (>= current-time start-time)
               (<= current-time end-time)) 
      current-time)))

;; those these could be useful it creates to much confusion with the 
;; mini language used in tlambda & tlambda*
;;
;; (defun before (offset &optional (source *default-time-source*))
;;   (let ((source source) (offset offset))
;;     (lambda () (or (beforep offset source) (signal-expired)))))

;; (defun after (offset &optional (source *default-time-source*))
;;   (let ((source source) (offset offset))
;;     (lambda () (afterp offset source))))

;; (defun between (start-time end-time &optional (source *default-time-source*))
;;   (let ((source source) (start-time start-time) (end-time end-time))
;;     (lambda () (or (betweenp start-time end-time source)
;;                    (when (afterp end-time source) (signal-expired))))))


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

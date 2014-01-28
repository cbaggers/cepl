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

(defun %compile-time-syntax (form)
  (if (atom form) form
      (let* ((tname (first form))
             (state nil)
             (expiredp nil)
             (anaphora nil)
             (body (loop :for item :in (rest form) :collect
                      (multiple-value-bind (c s e a) (%compile-time-syntax item)
                        (setf state (append s state))
                        (setf anaphora (append a anaphora))
                        (when e (push e expiredp))
                        c))))
        (multiple-value-bind (c s e a) (time-syntax-expand tname body)
          (values c 
                  (remove nil (append s (reverse state)))
                  (if (and (symbolp e) expiredp) (cons e expiredp) e)
                  (remove nil (append a (reverse anaphora))))))))

(defmacro tlambda (args test &body body)
  "tlambda is a special case of conditional function, it has one timesource 
   which it will use with all time predicates. As it has it's own timesource
   you can use time predicates rather than the closure generating funcs"
  (multiple-value-bind (ctest cstate expiredp anaphora) (%compile-time-syntax test)
    `(let ,(remove nil cstate)
       (lambda ,args 
         (let ,(remove nil anaphora)
           (if ,ctest (progn ,@body)
               ,(when expiredp `(when ,expiredp (signal-expired)))))))))

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

(defun before (offset &optional (source *default-time-source*))
  (let ((source source) (offset offset))
    (lambda () (or (beforep offset source) (signal-expired)))))

(defun after (offset &optional (source *default-time-source*))
  (let ((source source) (offset offset))
    (lambda () (afterp offset source))))

(defun between (start-time end-time &optional (source *default-time-source*))
  (let ((source source) (start-time start-time) (end-time end-time))
    (lambda () (or (betweenp start-time end-time source)
                   (when (afterp end-time source) (signal-expired))))))


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

;;{TODO} this gets the behaviour right but performance isnt great
(defmacro each* ((&optional (time-source *default-time-source*)) &body forms)
  (loop :for (timestep form) :in forms :for name = (gensym "stepper")
     :collect `(,name (make-stepper ,timestep ,time-source)) :into steppers
     :collect `(when (funcall ,name time-source) ,form) :into clauses
     :finally (return
                `(let (,@steppers)
                   (lambda (&optional (time-source ,time-source)) ,@clauses)))))


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
            :collect `(defun ,type (quantity) (* quantity ,count))
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
    (when (and (>=  start-time) (<= current-time end-time)) current-time)))

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

(defmacro t-every2 (timestep &optional (default-source '*default-time-source*))
  `(make-stepper ,timestep ,default-source))

(defmacro t-every (timestep &body forms)
  (let* ((stepper (gensym "stepper"))
         (source (if (eq (first forms) :default-source)
                     (second forms)
                     '*default-time-source*))
         (forms (if (eq (first forms) :default-source)
                    (cddr forms) forms)))
    `(let ((,stepper (make-stepper ,timestep ,source)))
       (lambda (&optional (time-source ,source))
         (when (funcall ,stepper time-source)
           ,@forms)))))

(defun c+ (&rest funcs)
  (lambda () (every #'funcall funcs)))

;;{TODO} this gets the behaviour right but performance isnt great
(defmacro t-every* ((&optional (time-source *default-time-source*)) &body forms)
  (loop :for (timestep form) :in forms :for name = (gensym "stepper")
     :collect `(,name (make-stepper ,timestep ,time-source)) :into steppers
     :collect `(when (funcall ,name time-source) ,form) :into clauses
     :finally (return
                `(let (,@steppers)
                   (lambda (&optional (time-source ,time-source)) ,@clauses)))))

;;----------------------------------------------------------------------

;; The crappiest little time-manager example

(let ((entries (list t)))
  (defun update-time-manager ()
    (let ((last entries)
          (current (cdr entries)))
      (loop :until (null current) :do
         (if (expiredp (funcall (car current)))
             (print "removed")
             (progn (setf (cdr last) current)
                    (setf last current)))
         (setf current (cdr current)))))
  (defun t-manage (item) (setf (cdr entries) (list item)))
  (defun t-releaae (item) (delete item entries))
  (defun t-clean () (setf (cdr entries) nil)))


;;----------------------------------------------------------------------

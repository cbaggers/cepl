(in-package #:cepl.events)

(defvar *event-source-names* nil)

(defgeneric subscribe (func source))
(defgeneric unsubscribe (func source))

(defmethod subscribe (func source)
  (assert (typep func 'function))
  (error "No event source named ~a was found. Possible alternatives are:~{~%~a~}"
         source *event-source-names*))

(defmethod unsubscribe (func source)
  (assert (typep func 'function))
  (error "No event source named ~a was found. Possible alternatives are:~{~%~a~}"
         source *event-source-names*))

(defmethod unsubscribe (func (source (eql t)))
  (assert (typep func 'function))
  (mapcar (lambda (s) (unsubscribe func s)) *event-source-names*))

(defmacro %def-event-listener (special name parent filter var body
                               allow-subscribers)
  (if special
      (assert (symbolp parent))
      (assert (and (symbolp parent) (not (null parent)))))
  (assert (and (symbolp var) (not (or (keywordp var) (null var)))))
  (assert (or (eq (first filter) 'function) (null filter)))
  (let ((sym-name (if special (kwd (string-upcase name)) name))
        (subscribers (gensym "subscribers"))
        (var (symb (symbol-name var))))
    (when (and (fboundp name) parent)
      (unsubscribe (symbol-function name) parent))
    `(let ,(when allow-subscribers `((,subscribers nil)))
       (defun ,name (,var)
         (declare (ignorable ,var))
         (,@(if filter `(when (funcall ,filter ,var)) '(progn))
            ,@body
            ,(when allow-subscribers
                   `(loop :for subscriber :in ,subscribers :do
                       (funcall subscriber ,var)))))
       ,@(when allow-subscribers
               `((defmethod subscribe ((func function) (source (eql ',sym-name)))
                   (unless (member func ,subscribers)
                     (setf ,subscribers (append ,subscribers (list func))))
                   func)

                 (defmethod unsubscribe ((func function) (source (eql ',sym-name)))
                   (setf ,subscribers (delete func ,subscribers))
                   func)

                 (defmethod unsubscribe-all-from ((source (eql ',sym-name)))
                   (setf ,subscribers nil))
                 (setf *event-source-names*
                       (remove-duplicates
                        (cons ',sym-name *event-source-names*) ))))

       ,(when parent `(subscribe #',name ,parent))
       ',name)))

(defmacro def-event-listener
    (name (var parent &key filter allow-subscribers) &body body)
  `(%def-event-listener nil ,name ,parent ,filter ,var ,body ,allow-subscribers))

(defmacro def-special-event-listener (name (&key parent filter (var 'event))
                                      &body body)
  `(%def-event-listener t ,name ,parent ,filter ,var ,body t))

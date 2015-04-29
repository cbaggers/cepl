(in-package :cgl)
(named-readtables:in-readtable fn_:fn_lambda)

(defvar *equivalent-type-specs* (make-hash-table))
(defun equiv-spec (lisp-type) (gethash lisp-type *equivalent-type-specs*))
(defun (setf equiv-spec) (value lisp-type)
  (setf (gethash lisp-type *equivalent-type-specs*) value))
(defun equivalent-typep (lisp-type)
  (not (null (gethash lisp-type *equivalent-type-specs*))))
(defun equivalent-type (lisp-type)
  (first (equiv-spec lisp-type)))

(defun swap-equivalent-types (args)
  (mapcar #'swap-equivalent-type args))
(defun swap-equivalent-type (arg)
  (let ((type (second arg)))
    (if (equivalent-typep type)
        `(,(car arg) ,(first (equiv-spec type)) ,@(cddr arg))
        arg)))

;; {TODO} Currently equivalent types cannot be used in the same file
;;        as they are defined. Try fixing this
(defmacro def-equivalent-type (lisp-type-name &body options)
  (if (symbolp (first options))
      (if (null (rest options))
          (det-simple-mapping lisp-type-name (first options))
          (apply #'det-exisiting-type lisp-type-name options))
      (apply #'det-new-type lisp-type-name options)))

;; simple mapping from one to another, unshadowing is actually done
;; in defpipeline so the uniform can be ascertained
;;
;; (def-equivalent-type 'single-float :float)
(defun det-simple-mapping (lisp-type-name varjo-type)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (varjo::add-type-shadow ',lisp-type-name ',varjo-type)))


;; uses existing type
;;
;; (def-equivalent-type 'pos-norm
;;   'g-pn
;;   (position (pos %))
;;   (normal (norm %)))
(defun det-exisiting-type (lisp-type-name varjo-type &rest slots)
  (assert (every λ(= (length _) 2) slots))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (equiv-spec ,lisp-type-name) ,(list varjo-type slots)))
     (let ((vsn (mapcar #'first (varjo::v-slots (varjo:type-spec->type
                                                ',varjo-type))))
           (sn ',(remove-duplicates (mapcar #'first slots))))
       (assert (and (= (length vsn) ,(length slots))
                    (every (lambda (x) (member x sn)) vsn))))))


;; makes new varjo type
;;
;; (def-equivalent-type cepl:camera
;;   (cam->clip :mat4 (slot-value % 'cam->clip) :accessor cam->clip)
;;   (world->cam :mat4 (world->cam %) :accessor world->cam))
(defun det-new-type (lisp-type-name &rest slots)
  (let ((alt-type (symb lisp-type-name '-)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (equiv-spec ',lisp-type-name)
               ',(list alt-type
                       (mapcar λ(destructuring-bind (name _ form &key accessor)
                                    _
                                  (declare (ignore _))
                                  (list (or accessor name) form)) slots))))
       (defstruct-g ,alt-type
           (:constructor nil :varjo-constructor ,lisp-type-name :populate nil
                         :attribs nil :readers nil :pull-push nil)
         ,@(mapcar λ(append (subseq _ 0 2) (subseq _ 3))
                   slots))
       ',lisp-type-name)))

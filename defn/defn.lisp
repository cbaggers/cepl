(in-package :cepl.defn)

(defun parse-defn-args (typed-args result-types)
  (let ((seen-&key nil)
        (seen-&rest nil)
        (seen-&optional nil)
        (f-args nil)
        (f-sigs nil)
        (f-decls nil))
    (labels ((kwd (x) (intern (symbol-name x) :keyword)))
      (loop :for x :in typed-args :do
         (destructuring-bind (name &optional (type t) opt-val) (ensure-list x)
           (cond
             ((string= name "&KEY")
              (when seen-&optional
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&key t)
              (push name f-args)
              (push name f-sigs))
             ((string= name "&REST")
              (when seen-&optional
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&rest t)
              (push name f-args)
              (push name f-sigs))
             ((string= name "&OPTIONAL")
              (when seen-&rest
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&optional t)
              (push name f-args)
              (push name f-sigs))
             ((char= #\& (char (symbol-name name) 0))
              (error "~a not valid in defn forms" name))
             (t
              (let ((decl-type (cond
                                 (seen-&key `(or ,type null))
                                 (seen-&optional `(or ,type null))
                                 (t type)))
                    (f-sig (cond
                             (seen-&key `(,(kwd name) ,type))
                             (seen-&optional `(or ,type null))
                             (t type)))
                    (f-arg (cond
                             (seen-&key `(,name ,opt-val))
                             (seen-&optional `(,name ,opt-val))
                             (t name))))
                (unless seen-&rest
                  (push `(type ,decl-type ,name) f-decls))
                (push f-sig f-sigs)
                (push f-arg f-args)))))))
    ;;
    (values (reverse f-args)
            `(function ,(reverse f-sigs) ,result-types)
            (reverse f-decls))))

(defvar *standard-declarations*
  '(dynamic-extent  ignore     optimize
    ftype           inline     special
    ignorable       notinline  type))

(defgeneric handle-defn-declaration (name %func-name args)
  (:method (name %func-name args)
    (declare (ignore name %func-name args))
    nil))

(defmacro define-defn-declaration (name args &body body)
  (assert (not (member name *standard-declarations*)) (name))
  (with-gensyms (gname d-args)
    (let ((func-name (intern "%FUNC-NAME")))
      `(defmethod handle-defn-declaration ((,gname (eql ',name)) ,func-name
                                           ,d-args)
         (declare (ignore ,gname)
                  (ignorable ,func-name))
         (destructuring-bind ,args ,d-args
           ,@body)))))

(defun process-defn-declares (func-name decls)
  (let ((data
         (loop :for decl :in decls :collect
            (let ((name (first decl)))
              (if (or (listp name) (member name *standard-declarations*))
                  (list decl nil nil)
                  (cons nil (multiple-value-list
                             (funcall #'handle-defn-declaration
                                      name func-name (rest decl)))))))))
    (list (remove nil (mapcar #'first data))    ;; cl-decl
          (remove nil (mapcar #'second data))   ;; heads
          (remove nil (mapcar #'third data))))) ;; tails

(defun %defn (name typed-args result-types inlinable-p inline-p body)
  (multiple-value-bind (args ftype type-decls)
      (parse-defn-args typed-args result-types)
    (multiple-value-bind (body decls doc) (parse-body body :documentation t)
      (destructuring-bind (decls heads tails)
          (process-defn-declares name (reduce #'append (mapcar #'rest decls)))
        (let* ((decls (if inline-p
                          (cons `(inline ,name) decls)
                          decls))
               (body (if heads
                         (append heads body)
                         body))
               (body (if tails
                         `((multiple-value-prog1 (progn ,@body) ,@tails))
                         body)))
          `(progn
             (declaim
              ,@(when inline-p `((inline ,name)))
              (ftype ,ftype ,name))
             (defun ,name ,args
               ,@(when doc (list doc))
               (declare ,@type-decls)
               (declare ,@decls)
               ,@body)
             ,@(when (and inlinable-p (not inline-p))
                 `((declaim (notinline ,name))))
             ',name))))))
(defmacro defn (name typed-args result-types &body body)
  "Define a typed function"
  (%defn name typed-args result-types nil nil body))

(defmacro defn-inline (name typed-args result-types &body body)
  "Define a typed function and request that it be inlined"
  (%defn name typed-args result-types t t body))

(defmacro defun+ (name args &body body)
  `(defun ,name ,args
     ,@(parse-body+ name body '((profile t)))))

(defmacro defmethod+ (name &rest args)
  (let* ((arg-pos (position-if #'listp args))
         (qual (subseq args 0 arg-pos))
         (body (subseq args (1+ arg-pos)))
         (args (elt args arg-pos)))
    `(defmethod ,name ,@qual ,args
                ,@(parse-body+ name body '((profile t))))))

(defun parse-body+ (name body &optional extra-decls)
  (multiple-value-bind (body decls doc) (parse-body body :documentation t)
    (destructuring-bind (decls heads tails)
        (process-defn-declares
         name (append (reduce #'append (mapcar #'rest decls)) extra-decls))
      (let* ((body (if heads
                       (append heads body)
                       body))
             (body (if tails
                       `((multiple-value-prog1 (progn ,@body) ,@tails))
                       body)))
        `(,@(when doc (list doc))
            (declare ,@decls)
            ,@body)))))

(defmacro locally+ (name &body body)
  `(locally ,@(parse-body+ name body)))
;;
;; Example usage
;;
;; (define-defn-declaration tester (&key foo bar)
;;   (values `(print ',foo)
;;           `(print ',bar)))
;;
;; (defn-inline foo ((a float)) float
;;   (declare (tester :foo 1 :bar 2))
;;   (* a a))

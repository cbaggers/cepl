;; This is a stack of useful functions not really thought of as
;; tools for writing games specifically, but rather for writing
;; cepl.
;; Saying that though, any use is wonderful so enjoy.

(in-package :cepl-utils)

(defmacro gdefun (name lambda-list &body body/options)
  (if (or (null body/options)
          (consp (car body/options))
          (keywordp (car body/options)))
      `(defgeneric ,name ,lambda-list ,@body/options)
      `(defmethod ,name ,lambda-list ,@body/options)))

(defun listify (x) (if (listp x) x (list x)))

(defmacro dbind (lambda-list expressions &body body)
  `(destructuring-bind ,lambda-list ,expressions ,@body))

(defmacro assoc-bind (lambda-list alist &body body)
  (let* ((g (gensym "alist"))
         (bindings (loop :for l :in lambda-list :collect
                      (let ((var (if (listp l) (first l) l))
                            (key (if (listp l)
                                     (second l)
                                     l)))
                        `(,var (cdr (assoc ',key ,g)))))))
    `(let ((,g ,alist))
       (let ,bindings
         (declare (cl:ignorable ,@(mapcar #'first bindings)))
         ,@body))))

(defun sn-equal (a b) (equal (symbol-name a) (symbol-name b)))

(defun replace-nth (list n form)
  `(,@(subseq list 0 n) ,form ,@(subseq list (1+ n))))

(defun hash-values (hash-table)
  (loop for i being the hash-values of hash-table collect i))

(defun hash-keys (hash-table)
  (loop for i being the hash-keys of hash-table collect i))

(defun intersperse (symb sequence)
  (rest (mapcan #'(lambda (x) (list symb x)) sequence)))

;; This will be pretty inefficient, but shoudl be fine for code trees
;; {TODO} how is this not subst?
(defun walk-replace (to-replace replace-with form
		     &key (test #'eql))
  "This walks a list tree ('form') replacing all occurences of
   'to-replace' with 'replace-with'. This is pretty inefficent
   but will be fine for macros."
  (cond ((null form) nil)
	((atom form) (if (funcall test form to-replace)
			 replace-with
			 form))
	(t (cons (walk-replace to-replace
			       replace-with
			       (car form)
			       :test test)
		 (walk-replace to-replace
			       replace-with
			       (cdr form)
			       :test test)))))

(defun file-to-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated
   string, returning two values: the string and the number of
   bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun flatten (x)
  "Walks a list tree and flattens it (returns a 1d list
   containing all the elements from the tree)"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

;; [TODO] damn this is slow
(defun find-in-tree (item tree &key (test #'eql))
  ""
  (labels ((rec (x)
             (cond ((null x) nil)
                   ((atom x) (funcall test x item))
                   (t (or (rec (car x)) (rec (cdr x)))))))
    (rec tree)))


(defun mkstr (&rest args)
  "Takes a list of strings or symbols and returns one string
   of them concatenated together. For example:
    CEPL-EXAMPLES> (cepl-utils:mkstr 'jam 'ham')
     'JAMHAM'
    CEPL-EXAMPLES> (cepl-utils:mkstr 'jam' 'ham')
     'jamham'"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "This takes a list of symbols (or strings) and outputs one
   symbol.
   If the input is symbol/s then the output is a regular symbol
   If the input is string/s, then the output is
   a |symbol like this|"
  (values (intern (apply #'mkstr args))))

(defun symb-package (package &rest args)
  (values (intern (apply #'cepl-utils:mkstr args) package)))

(defun make-keyword (&rest args)
  "This takes a list of symbols (or strings) and outputs one
   keyword symbol.
   If the input is symbol/s then the output is a regular keyword
   If the input is string/s, then the output is
   a :|keyword like this|"
  (values (intern (apply #'mkstr args) "KEYWORD")))

(defun kwd (&rest args)
  "This takes a list of symbols (or strings) and outputs one
   keyword symbol.
   If the input is symbol/s then the output is a regular keyword
   If the input is string/s, then the output is
   a :|keyword like this|"
  (values (intern (apply #'mkstr args) "KEYWORD")))

(defun group (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n)
				   acc))
		   (nreverse (cons source acc))))))
    (if source
	(rec source nil)
	nil)))

(defvar safe-read-from-string-blacklist
  '(#\# #\: #\|))

(let ((rt (copy-readtable nil)))
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "safe-read-from-string failure"))

  (dolist (c safe-read-from-string-blacklist)
    (set-macro-character
      c #'safe-reader-error nil rt))

  (defun safe-read-from-string (s &optional fail)
    (if (stringp s)
      (let ((*readtable* rt) *read-eval*)
        (handler-bind
          ((error (lambda (condition)
                    (declare (ignore condition))
                    (return-from
                      safe-read-from-string fail))))
          (read-from-string s)))
      fail)))

(defun sub-at-index (seq index new-val)
  (append (subseq seq 0 index)
	  (list new-val)
	  (subseq seq (1+ index))))


(defun lispify-name (name)
  "take a string and changes it to uppercase and replaces
   all underscores _ with minus symbols -"
  (let ((name (if (symbolp name)
                  (mkstr name)
                  name)))
    (string-upcase (substitute #\- #\_ name))))

(defun symbol-name-equal (a b)
  (and (symbolp a) (symbolp b) (equal (symbol-name a) (symbol-name b))))

(defun range (x &optional y z u v)
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :below end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :above end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :below end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :above end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null) (if (> x 0) (basic 0 x) (basic-down 0 x)))
        (number (if (or (null z) (keywordp z))
                    (if (> y x) (basic x y) (basic-down x y))
                    (if (> y x) (fun x y z) (fun-down x y z))))
        (function (if (> x 0) (fun 0 x y) (fun-down 0 x y)))))))

(defun rangei (x &optional y z u v)
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :upto end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :downto end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :upto end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :downto end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null) (if (> x 0) (basic 0 x) (basic-down 0 x)))
        (number (if (or (null z) (keywordp z))
                    (if (> y x) (basic x y) (basic-down x y))
                    (if (> y x) (fun x y z) (fun-down x y z))))
        (function (if (> x 0) (fun 0 x y) (fun-down 0 x y)))))))

(defun arange (x &optional y z u v)
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :below end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :above end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :below end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :above end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null)
         (make-array x :initial-contents
                     (if (> x 0) (basic 0 x) (basic-down 0 x))))
        (number (make-array (abs (- y x))
                            :initial-contents
                            (if (or (null z) (keywordp z))
                                (if (> y x) (basic x y) (basic-down x y))
                                (if (> y x) (fun x y z) (fun-down x y z)))))
        (function (make-array x :initial-contents
                              (if (> x 0) (fun 0 x y) (fun-down 0 x y))))))))

(defun arangei (x &optional y z u v)
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :upto end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :downto end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :upto end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :downto end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null)
         (make-array (1+ x) :initial-contents
                     (if (> x 0) (basic 0 x) (basic-down 0 x))))
        (number (make-array (1+ (abs (- y x)))
                            :initial-contents
                            (if (or (null z) (keywordp z))
                                (if (> y x) (basic x y) (basic-down x y))
                                (if (> y x) (fun x y z) (fun-down x y z)))))
        (function (make-array (1+ x) :initial-contents
                              (if (> x 0) (fun 0 x y) (fun-down 0 x y))))))))






(define-compiler-macro mapcat (function &rest lists)
  `(apply #'concatenate 'list (mapcar ,function ,@lists)))

(defun mapcat (function &rest lists)
  (apply #'concatenate 'list (apply #'mapcar function lists)))

(defun split-seq-by-seq (delim sequence)
  (let* ((delim-len (length delim))
         (seq-len (length sequence))
         (result nil)
         (last 0)
         (i 0))
    (loop :do
       (setf i (1+ i))
       (if (> (+ i delim-len) seq-len)
           (progn (push (subseq sequence last) result)
                  (return (reverse result)))
           (when (equal (subseq sequence i (+ i delim-len)) delim)
             (push (subseq sequence last i) result)
             (setf i (+ -1 i delim-len)
                   last (1+ i)))))))

;; (defmacro dbg (form)
;;   (unless (and (listp form) (symbolp (first form))
;;                (not (member (first form) '(quote function))))
;;     (error "Doesnt look like a function call does it?"))
;;   (let ((gensyms (loop for i in (rest form) collect (gensym))))
;;     `(let ,(loop for a in (rest form) for g in gensyms collect `(,g ,a))
;;        (format t "~%~a -> " (list ',(first form) ,@gensyms))
;;        (let ((res (handler-case (,(first form) ,@gensyms)
;;                     (error (x) (format t "<error>~%") (error x)))))
;;          (format t "~a~%" res)
;;          res))))

;------------ERRORS-----------;

;;[TODO] need better arg test
(defmacro deferror (name (&key (error-type 'error) prefix)
                            (&rest args) error-string &body body)
  (unless (every #'symbolp args) (error "can only take simple args"))
  (loop :for arg :in args :do
     (setf body (subst `(,arg condition) arg body :test #'eq)))
  `(define-condition ,name (,error-type)
     (,@(loop :for arg :in args :collect
           `(,arg :initarg ,(kwd arg) :reader ,arg)))
     (:report (lambda (condition stream)
                (declare (ignorable condition))
                (format stream ,(format nil "~@[~a:~] ~a" prefix error-string)
                        ,@body)))))

(defmacro asserting (form error-name &rest keys-to-error)
  `(unless ,form (error ',error-name ,@keys-to-error)))


;; ------------------------------------------------------------
;; dumb little func to pretty print a memory table

(defgeneric print-mem (thing &optional size-in-bytes offset))

(defmethod print-mem ((thing t) &optional (size-in-bytes 64) (offset 0))
  (typecase thing
    (cffi:foreign-pointer
     (%print-mem (cffi:inc-pointer thing offset)
                 size-in-bytes))
    (autowrap:wrapper
     (%print-mem (cffi:inc-pointer (autowrap:ptr thing) offset)
                 size-in-bytes))
    (otherwise (format t "Error - Unsure how to print memory of object of type: ~a"
                       (type-of thing))))
  nil)

(defun %print-mem (pointer &optional (size-in-bytes 64))
  (let* ((size (if (oddp size-in-bytes) (1+ size-in-bytes) size-in-bytes))
         (data (loop :for i :below size :collect
                  (cffi:mem-ref pointer :uchar i)))
         (batched (utils:group data 16))
         (batched-chars (mapcar
                         (lambda (x)
                           (mapcar
                            (lambda (c)
                              (if (and (> c 31) (< c 126))
                                  (code-char c)
                                  #\.))
                            x))
                         batched)))
    (loop :for batch :in batched :for chars :in batched-chars
       :for i :from 0 :by 16 :do
       (when (= 0 (mod i 256))
         (format t "~%87654321    0011 2233 4455 6677 8899 aabb ccdd eeff    0123456789abcdef~%")
         (format t "-----------------------------------------------------------------------~%"))
       (format t "~8,'0X    ~{~@[~2,'0X~]~@[~2,'0X ~]~}   " i batch)
       (format t "~{~a~}~{~c~}~%"
               (loop :for i :below (max 0 (floor (/ (- 16 (length batch)) 2)))
                  :collect "     ")
               chars))))

(defmacro with-hash ((var-name key) hash-table &body body)
  (let ((k (gensym "key"))
	(ht (gensym "hash-table")))
    `(let ((,k ,key)
	   (,ht ,hash-table))
       (symbol-macrolet ((,var-name (gethash ,k ,ht)))
	 ,@body))))

(defmacro with-hash* (var-key-pairs hash-table &body body)
  (let ((keys (loop :for x in var-key-pairs :collect (gensym "key")))
	(ht (gensym "hash-table")))
    `(let ((,ht ,hash-table)
	   ,@(mapcar (lambda (_ _1) `(,_ ,(second _1)))
		     keys var-key-pairs))
       (symbol-macrolet
	   ,(mapcar (lambda (k p) `(,(first p) (gethash ,k ,ht)))
		    keys var-key-pairs)
	 ,@body))))

(defun map-hash (function hash-table)
  "map through a hash and actually return something"
  (let* ((head (list nil))
         (tail head))
    (labels ((do-it (k v)
               (rplacd tail (setq tail (list (funcall function k v))))))
      (maphash #'do-it hash-table))
    (cdr head)))

(defun last1 (list) (car (last list)))


(defmacro p-> (args &body stages)
  "\(p-> \(1 2 3\) #'a #'b #'c #'d\)
   Calls first function with args provided and uses result as
   arguments for next function. Uses multiple-value-call so you
   can use (values) to specify complex lambda-args."
  (let ((stages (reverse stages)))
    (when stages
      (let ((stage (first stages)))
        (if (eq 'function (first stage))
            `(multiple-value-call ,stage
               ,(if (rest stages)
                    `(p-> ,args ,@(reverse (rest stages)))
                    (if (listp args)
                        `(values ,@args)
                        `(values-list ,args))))
            (destructuring-bind (check-func &rest steps) stage
              `(let ((rest (multiple-value-list
                            ,(if (rest stages)
                                 `(p-> ,args ,@(reverse (rest stages)))
                                 (if (listp args)
                                     `(values ,@args)
                                     `(values-list ,args))))))
                 (let ((args rest))
                   (let ((passes nil))
                     (loop :do (let ((results (multiple-value-list
                                               (p-> ,@(cons 'args steps)))))
                                 (setf args results)
                                 (push results passes))
                        :until (,check-func (first passes) (second passes))))
                   (values-list args)))))))))

(defmacro ---block-doc--- (doc-string &body body)
  (declare (ignore doc-string))
  `(progn ,@body))

(defgeneric make-length-same
    (list list-to-match &optional fill-value error-on-shorten-p))

(defmethod make-length-same ((list list) (list-to-match list)
			     &optional fill-value (error-on-shorten-p t))
  (let ((l1 (length list))
	(l2 (length list-to-match)))
    (cond ((= l1 l2) list)
	  ((< l1 l2) (append list (loop :for i :from l1 :below l2 :collect
				     fill-value)))
	  (t (if error-on-shorten-p
		 (error "make-length-same wont shrink the source list:
source: ~s~%list-to-match: ~s" list list-to-match)
		 (subseq list 0 (length list-to-match)))))))

(defmacro case= (form &body cases)
  (let ((g (gensym "val")))
    (labels ((wrap-case (c) `((= ,g ,(first c)) ,@(rest c))))
      (let* ((cases-but1 (mapcar #'wrap-case (butlast cases)))
	     (last-case (car (last cases)))
	     (last-case (if (eq (car last-case) 'otherwise)
			    `(t ,@(rest last-case))
			    (wrap-case last-case)))
	     (cases (append cases-but1 (list last-case))))
	`(let ((,g ,form))
	   (cond ,@cases))))))

(defun split-string (delimiter string)
  (let* ((string (string-trim (list delimiter) string))
	 (result '(nil)))
    (loop :for c :across string
       :if (char= c delimiter) :do (push nil result)
       :else :do (push c (first result)))
    (mapcar (lambda (x) (concatenate 'string x))
	    (reverse result))))

(defun ni-call (package-name func-name &rest args)
  (let ((p (find-package package-name)))
    (unless p (error "ni-call: package ~s not found" package-name))
    (let ((func-symb (find-symbol (if (keywordp func-name)
				      (symbol-name func-name)
				      func-name)
				  p)))
      (unless func-name (error "ni-call: could not find symbol ~s in package ~s"
			       func-name package-name))
      (apply (symbol-function func-symb) args))))

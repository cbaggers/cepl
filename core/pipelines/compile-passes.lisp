(in-package :cepl.pipelines)
(in-readtable fn:fn-reader)

(defparameter *verbose-compiles* nil)

;; Often we want to run varjo on some code and use the metadata in the
;; varjo-compile-result to inform modifications to the code and uploads.
;;
;; After making the modifications we pass the new code and args into the
;; varjo for another pass.
;;
;; Below we have versions of varjo's translate and rolling-translate functions
;; that take optional extra passes.

(defclass compile-pass ()
  ((name :initarg :name :initform :no-name :reader name)))

(defclass ast-transform-compile-pass (compile-pass)
  ((filter-transform-pairs :initarg :filter-transform-pairs
                           :initform nil
                           :reader filter-transform-pairs)))

(defclass deep-dive-compile-pass (compile-pass)
  ((filter :initarg :filter
	   :initform (error "filter must be provided" )
	   :reader filter)
   (transform :initarg :transform
	      :initform (error "transform must be provided" )
	      :reader transform)))

(defclass %uniform-transform-pass (compile-pass) ())

;;--------------------------------------------------

(defvar *registered-passes* nil)
(defvar *cepl-passes*
  `((remove-unused-uniforms nil ,(make-instance '%uniform-transform-pass))))

(defun %add-compile-pass (name instance depends-on)
  (when depends-on
    (unless (member depends-on *registered-passes* :key #'first)
      (error "Compile pass ~s Cannot depend on non-existant pass ~s"
             name depends-on)))
  (let ((taken (find-if λ(and (eq (second _) depends-on)
                              (not (eq (first _) name)))
                        *registered-passes*)))
    (when taken
      (error "~s's dependency (~s) is already taken by ~s "
             name depends-on (first taken))))
  (let* ((new (list name depends-on instance))
         (passes (remove name *registered-passes* :key #'first))
         (before (position name passes :key #'second))
         (pos (or before
                  (1+ (or (and passes (position depends-on passes :key #'first))
                          (1- (length passes))))))
         (left (when passes (subseq passes 0 pos)))
         (right (when passes (subseq passes pos))))
    (setf *registered-passes* (append left (list new) right))))

(defmacro def-compile-pass (name/options &body filter-transform-pairs)
  (dbind (name &key depends-on) (listify name/options)
    (assert (and (symbolp name)
                 (symbolp depends-on)
                 (not (keywordp name))
                 (every #'listp filter-transform-pairs)
                 (every λ(= (length _) 2) filter-transform-pairs)))
    `(%add-compile-pass
      ',name
      (make-instance 'ast-transform-compile-pass
		     :name ',name
                     :filter-transform-pairs
                     (list ,@(mapcar λ(cons 'list _) filter-transform-pairs)))
      ',depends-on)))

(defmacro def-deep-pass (name/options &key filter transform)
  (dbind (name &key depends-on) (listify name/options)
    (assert (and (symbolp name)
                 (symbolp depends-on)
                 (not (keywordp name))))
    `(%add-compile-pass ',name
			(make-instance 'deep-dive-compile-pass
				       :name ',name
				       :filter ,filter :transform ,transform)
			',depends-on)))

(defun undef-compile-pass (name)
  (setf *registered-passes*
        (remove-if λ(eq name (first _)) *registered-passes*))
  t)

(defun %get-passes () (mapcar #'third *registered-passes*))
(defun %get-internal-passes () (mapcar #'third *cepl-passes*))

;;--------------------------------------------------

(defun make-pass-env (arg-val-map)
  (assert (typep arg-val-map 'hash-table))
  (let ((e (make-hash-table)))
    (setf (gethash 'set-uniforms e) nil
          (gethash 'remove-uniforms e) nil
          (gethash 'uniform-vals e) arg-val-map)
    e))

(defun v-translate (in-args uniforms context body
                    &optional tp-meta)
  "Cepl allows you to use varjo in a multi pass fashion by providing a list of
   transform-passes to be run on the varjo-compile-result of each pass.

   This function runs all the passes until there are no more changes to make.
   It knows this when none of the passes' ast-filter methods return any nodes to
   be transformed."
  (let* ((tp-meta (or tp-meta (make-hash-table)))
         (arg-val-map (or (gethash 'uniform-vals tp-meta)
                          (let ((h (make-hash-table)))
                            (loop :for (n . r) :in uniforms
			       :do (just-ignore r)
                               :do (setf (gethash n h) n))
                            h)))
         (passes (mapcar λ(cons _ (make-pass-env arg-val-map))
                         (append (%get-passes) (%get-internal-passes))))
	 (pass-inputs nil))
    (handler-bind ((varjo-conditions:setq-type-match
		    (lambda (c)
		      (when (typep (varjo::code-type (varjo::new-value c))
				   'cepl.space:pos4-g)
			(invoke-restart
			 'varjo-conditions:setq-supply-alternate-type
			 :vec4)))))
      (labels ((on-pass (accum pass-pair)
		 (dbind (c-result uniforms) accum
		   (dbind (pass . transform-env) pass-pair
		     (multiple-value-bind (new-pass-args there-were-changes)
			 (run-pass c-result pass transform-env
				   in-args uniforms context)
		       (if there-were-changes
			   (progn
			     (push (append new-pass-args (list (name pass)))
				   pass-inputs)
			     (on-pass
			      (list (apply #'varjo:translate new-pass-args)
				    (second new-pass-args))
			      pass-pair))
			   (list c-result uniforms))))))
	       (once-through (initial uniforms)
		 (reduce #'on-pass passes
			 :initial-value (list initial uniforms)))
	       (until-no-change (initial uniforms)
		 (dbind (new-result new-uniforms)
		     (once-through initial uniforms)
		   (if (eq new-result initial)
		       initial
		       (until-no-change new-result new-uniforms)))))
	(let ((translate-result
	       (varjo:translate in-args uniforms context body tp-meta)))
	  (push (list in-args uniforms context body) pass-inputs)
	  (push (list in-args uniforms context (ast->code translate-result))
		pass-inputs)
	  (let ((compile-result
		 (until-no-change translate-result uniforms)))
	    (with-hash (av 'uniform-vals) (third-party-metadata compile-result)
	      (setf av arg-val-map))
	    (when *verbose-compiles*
	      (print-compile-report (reverse pass-inputs)))
	    compile-result))))))


(defun v-rolling-translate (stages)
  (varjo:rolling-translate stages #'v-translate))

;;--------------------------------------------------

(defgeneric run-pass (v-compile-result pass env original-in-args
                      original-uniforms original-context))

;;--------------------------------------------------

(defmethod run-pass (v-compile-result (pass deep-dive-compile-pass)
                     env original-in-args original-uniforms
                     original-context)
  (with-slots (filter transform) pass
    ;; {TODO} ast-deep-replace does not respect env and will walk
    ;;        many paths even though it may result in changes to the
    ;;        env by code changes that arent included in the result.
    (let ((augmented-transform (fn:fn~r transform env)))
      (multiple-value-bind (new-body there-were-changes)
	  (varjo:ast-deep-replace (ast v-compile-result) filter
				  augmented-transform)
        (let ((new-uniforms
               (remove-duplicates
                (append (reduce λ(remove _1 _ :key #'car)
                                (gethash 'remove-uniforms env)
                                :initial-value original-uniforms)
                        (gethash 'set-uniforms env))
                :key #'car)))
          (values (list original-in-args new-uniforms original-context new-body)
                  there-were-changes))))))

;;--------------------------------------------------

(defmethod run-pass (v-compile-result (pass ast-transform-compile-pass)
                     env original-in-args original-uniforms
                     original-context)
  (with-slots (filter-transform-pairs) pass
    (let* ((pairs+env
            (mapcar λ(dbind (filter func) _ `(,filter ,(fn:fn~r func env)))
                    filter-transform-pairs)))
      (multiple-value-bind (new-body there-were-changes)
          (varjo:ast->code v-compile-result :filter-func-pairs pairs+env)
        (let ((new-uniforms
               (remove-duplicates
                (append (reduce λ(remove _1 _ :key #'car)
                                (gethash 'remove-uniforms env)
                                :initial-value original-uniforms)
                        (gethash 'set-uniforms env))
                :key #'car)))
          (values (list original-in-args new-uniforms original-context new-body)
                  there-were-changes))))))

;;--------------------------------------------------

(defmethod run-pass (v-compile-result (pass %uniform-transform-pass)
                     env original-in-args original-uniforms
                     original-context)
  (declare (ignore env))
  (let* ((changed (not (null (find-unused-uniforms v-compile-result))))
         (new-uniforms
          (when changed (remove-unused-uniforms v-compile-result))))
    (values
     (with-slots (uniform-transform) pass
       (list original-in-args (if changed new-uniforms original-uniforms)
             original-context
             (varjo:ast->code v-compile-result)))
     changed)))

(defun find-unused-uniforms (compiled)
  (let* ((base-env (varjo:get-base-env (ast-starting-env (ast compiled))))
         (uniform-names (mapcar #'car (uniforms compiled)))
	 ;; get a list of (flow-id . uniform-name)
         (pairs (mapcar λ(cons (flow-ids (get-var _ base-env)) _)
			uniform-names)))
    ;; walk the ast, any time you see a flow-ids in the uniform-flowid map
    ;; above (pairs) remove it. By the end, pairs will only contain unused
    ;; uniforms
    (labels ((visit (node)
               (setf pairs (remove-if λ(id~= (flow-ids node) (car _))
				      pairs))))
      (visit-ast-nodes #'visit compiled)
      (mapcar #'cdr pairs))))

(defun remove-unused-uniforms (compiled)
  (let ((unused (find-unused-uniforms compiled)))
    (remove-if λ(member (car _) unused) (uniforms compiled))))

;;--------------------------------------------------

(defun set-uniform (name type env)
  (with-hash (su 'set-uniforms) env
    (setf su (remove-duplicates (cons (list name type) su)
                                :key #'car))))

(defun remove-uniform (name env)
  (with-hash (su 'remove-uniforms) env
    (setf su (remove-duplicates (cons name su)))))

(defun set-arg-val (name form env &key (current-val-symb :current))
  (with-hash (arg-vals 'uniform-vals) env
    (with-hash (arg name) arg-vals
      (setf arg (subst arg current-val-symb form)))))

;;--------------------------------------------------

(defun print-compile-report (pass-inputs)
  (labels ((report (f)
	     (when (fifth f)
	       (format t "~%:name ~s" (fifth f)))
	     (format t "~%:in-args ~s" (first f))
	     (format t "~%:uniforms ~s" (second f))
	     (format t "~%:context ~s" (third f))
	     (format t "~%:body-code ~%~s" (fourth f))))
    (print "-----------------------------------")
    (print "- Initial Inputs -")
    (report (first pass-inputs))
    (print "-  -  -  -  -  -  -  -  -  -  -  -")
    (print "- Gave This -")
    (report (second pass-inputs))
    (loop :for a :in (butlast (cddr pass-inputs)) :for i :from 1 :do
       (print "-  -  -  -  -  -  -  -  -  -  -  -")
       (format t "~%- After Pass ~s -" i)
       (report a))
    (print "-  -  -  -  -  -  -  -  -  -  -  -")
    (print "- Final Code -")
    (if (= 2 (length pass-inputs))
	(print "No change")
	(report (car (last pass-inputs))))
    (print "-----------------------------------")))

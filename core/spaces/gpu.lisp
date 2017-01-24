(in-package :cepl.space)
(in-readtable fn:fn-reader)

;;
;; Spaces
;;

;;-------------------------------------------------------------------------
;; Spatial Vectors

;; {TODO} Need to add metadata to svecs on construction.

(varjo::def-metadata-infer svec4 spatial-meta env
  (values :in-space (get-current-space env)))

(defmethod combine-metadata ((meta-a spatial-meta)
                             (meta-b spatial-meta))
  (let ((space-a (in-space meta-a))
        (space-b (in-space meta-b)))
    (if (eq space-a space-b)
        space-a
        (error "Space Analysis Failed: Could not establish at compile time which
space the resulting svec was in between:
~a
and
~a" space-a space-b))))


;;-------------------------------------------------------------------------
;; Vector Space

(defun get-current-space (env)
  (varjo::variable-uniform-name '*current-space* env))

(v-defmacro in (&environment env space &body body)
  (assert body () "CEPL.SPACES: 'In' form found without body.")
  (let* ((vars (varjo::variables-in-scope env))
         (svecs (remove-if-not λ(typep (varjo::variable-type _ env) 'svec4-g) vars))
         (gvecs (mapcar λ(gensym (symbol-name _)) svecs))
         (spaces (mapcar λ(in-space (varjo::metadata-for-variable _ 'spatial-meta env))
                         svecs))
         (forms (mapcar λ`(space-boundary-convert
                           (let* ((*current-space* ,_1))
                             ,_))
                        gvecs
                        spaces))
         (pairs (mapcar #'list svecs forms)))
    `(let ,(mapcar #'list gvecs svecs)
       (space-boundary-convert
        (symbol-macrolet ,pairs
          (let ((*current-space*  ,space))
            ,@body))))))

(defmacro in (space &body body)
  (declare (ignore space body))
  (error "the 'in' macro can only be used inside shaders"))

(v-defun get-transform (x y) "#-GETTRANSFORM(~a)" (vec-space vec-space) 0)

(varjo::v-define-compiler-macro get-transform (&environment env
                                                (from-space vec-space)
                                                (to-space vec-space))
  (declare (ignore from-space to-space))
  ;;
  ;; when we have to transform from *screen-space* or *ndc-space* we are in
  ;; a more tricky situation as the transform is a function of the vector,
  ;; a matrix alone cannot capture the transform.
  ;; To handle this situation (in a way that won't kill performace) we
  ;; require that the source space (screen or ndc) is explicit, in code, at
  ;; compile time.
  ;;
  (let* ((from-name (or (varjo::argument-uniform-name 'from-space env)
                        (error "get-transform: The first argument doesnt appear to be from a uniform")))
         (to-name (or (varjo::argument-uniform-name 'to-space env)
                      (error "get-transform: The second argument doesnt appear to be from a uniform"))))
    (if (or (eq from-name '*screen-space*) (eq from-name '*ndc-space*))
        (error "CEPL: get-transform is not currently supported for transform from *screen-space* or *ndc-space*")
        (compile-implicit-mat4 from-name to-name env))))

(v-defun space-boundary-convert (x) "#-SPACEBOUNDARYCONVERT(~a)" (v-type) 0)

(varjo:v-define-compiler-macro space-boundary-convert (&environment env (form v-type))
  (let ((form-type (varjo::argument-type 'form env))
        (in-a-space-p (varjo::variable-in-scope-p '*current-space* env)))
    (cond
      ((and (v-typep form-type 'svec4-g) in-a-space-p)
       (let* ((inner-name (in-space
                           (varjo::metadata-for-argument 'form 'spatial-meta
                                                         env)))
              (outer-name (get-current-space env)))
         (convert-between-spaces form inner-name outer-name env)))
      ((v-typep form-type 'svec4-g) `(v! ,form))
      (t form))))

(defun convert-between-spaces (form from-name to-name env)
  (let ((new-code
         (if (or (eq from-name '*screen-space*) (eq from-name '*ndc-space*))
             (inject-clip-or-ndc-reverse-transform form from-name to-name env)
             (inject-regular-space-transform form from-name to-name env))))
    (alexandria:with-gensyms (gvar)
      `(let ((,gvar ,new-code))
         (declare (spatial-meta (:in-space ,to-name) ,gvar))
         ,gvar))))

(defun inject-regular-space-transform (form from-name to-name  env)
  ;; we need to add the transform uniform and get it's name
  (let* ((injected (compile-implicit-mat4 from-name to-name env )))
    ;; and here is the replacement code for our crossing the spaces
    `(svec-* ,form ,injected)))

(defun compile-implicit-mat4 (from-name to-name env)
  ;; Inject a conversion uniform
  (let ((implicit-uniform-name (symb from-name :-to- to-name :-mat4)))
    (varjo::add-lisp-form-as-uniform
     `(get-transform ,from-name ,to-name) :mat4 env implicit-uniform-name)))

(defun inject-clip-or-ndc-reverse-transform (form from-name to-name env)
  (cond
    ;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    ;; The cases we dont yet handle
    ((eq from-name '*ndc-space*)
     (error 'from-ndc))
    ((eq to-name '*screen-space*)
     (error 'to-ndc-or-screen))
    ((eq to-name '*ndc-space*)
     (error 'to-ndc-or-screen))
    ;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    ;; The case we do handle
    ((eq from-name '*screen-space*)
     (let* ((vpp-name 'viewport-params)
            (injected (varjo::add-lisp-form-as-uniform
                       `(viewport-params-to-vec4) :vec4 env vpp-name)))
       ;; lets make the code to transform to clip-space
       (let ((code `(screen-space-to-clip-space ,form ,injected)))
         ;; now we have a transform to clip-space, but it is likely we need to
         ;; go further, if that might be the case then tell the next pass that
         ;; we are in clip-space and let the compiler take care of the rest
         (if (eq to-name '*clip-space*)
             `(sv! ,code)
             `(in *clip-space* (sv! ,code))))))
    (t (error "compile bug"))))


;; this func isn't in the ast, which means the build won't
;; stabilize. Add a copy-code and replace the svec-* ast-node
(v-defun svec-* (a b) "(~a * ~a)" (v-mat4 svec4-g) 1)
(v-defun svec-* (a b) "(~a * ~a)" (svec4-g v-mat4) 0)
(v-defun svec-* (a b) "(~a * ~a)" (v-mat4 :vec4) 1)
(v-defun svec-* (a b) "(~a * ~a)" (:vec4 v-mat4) 0)

;;-------------------------------------------------------------------------

;; (defun-g screen-space-to-clip-space ((ss-pos :vec4) (viewport :vec4))
;;   (/ (v! (- (* (v:s~ ss-pos :xy) 2.0)
;; 	    (/ (* (v:s~ viewport :xy) 2.0)
;; 	       (* (v:s~ viewport :zw) 2.0))
;; 	    (v! 1 1))
;; 	 (/ (- (* 2.0 (v:z gl-frag-coord))
;; 	       (near gl-depth-range)
;; 	       (far gl-depth-range))
;; 	    (- (near gl-depth-range) (far gl-depth-range)))
;; 	 1.0)
;;      (v:w gl-frag-coord)))

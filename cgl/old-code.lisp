;;--------------------------------------------------
;; Old code

;; (defmacro defshader (name args &body body)
;;   (let ((recompile-name (symb-package :%cgl name)))
;;     (destructuring-bind (s-in-args s-uniforms s-context)
;;         (varjo:split-arguments args '(&uniform &context))
;;       `(progn
;;          (eval-when (:compile-toplevel :load-toplevel :execute)
;;            (utils:assoc-bind ((in-args nil) (uniforms :&uniform) (context :&context) (instancing :&instancing))
;;                (utils:lambda-list-split '(&uniform &context &instancing) ',args)
;;              (setf (gethash ',name *shader-args*) (list in-args uniforms context instancing))))
;;          (defun ,recompile-name ()
;;            (let ((compile-result (varjo::translate
;;                                   ',s-in-args ',s-uniforms ',s-context
;;                                   '(progn ,@body))))
;;              (update-shader-asset ',name :shader compile-result
;;                                   #',recompile-name (used-external-functions compile-result))))
;;          (,recompile-name)
;;          ',name))))

;; (defmacro defsfun (name args &body body)
;;   (let ((recompile-name (symb-package :%cgl name)))
;;     `(progn
;;        (defun ,recompile-name ()
;;          ,(destructuring-bind
;;            (in-args uniforms context)
;;            (varjo:split-arguments args '(&uniforms &context))
;;            (declare (ignore uniforms))
;;            `(let ((compile-result (varjo::%v-def-external ',name ',in-args
;;                                                           ',context ',body)))
;;               (update-shader-asset ',name :function compile-result
;;                                    #',recompile-name (used-external-functions compile-result)))))
;;        (,recompile-name)
;;        ',name)))

;;--------------------------------------------------
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defvar *shader-args* (make-hash-table)))

;; (let ((assets (make-hash-table)))
;;   (defun update-shader-asset (name type compile-result recompile-function depends-on)
;;     (let ((prog-id (when (eq type :pipeline)
;;                      (or (fourth (gethash name assets))
;;                          (gl:create-program))))
;;           (visited nil))
;;       (loop :for (n cr rf id dp) :being :the :hash-value :of assets
;;          :if (and (not (member n visited)) (find name dp)) :do
;;          (format t "~&; recompiling (~a ...)~&" n)
;;          (push n visited)
;;          (funcall rf))
;;       (setf (gethash name assets) (list name compile-result recompile-function
;;                                         prog-id depends-on type))
;;       prog-id))
;;   (defun get-glsl-code (name)
;;     (let ((asset (gethash name assets)))
;;       (when asset
;;         (destructuring-bind (name compile-result recompile-function prog-id depends-on type)
;;             asset
;;           (declare (ignore name recompile-function depends-on prog-id))
;;           (if (eq type :pipeline)
;;               (mapcar #'varjo:glsl-code compile-result)
;;               (varjo:glsl-code compile-result))))))
;;   (defun get-compiled-asset (name)
;;     (second (gethash name assets)))
;;   (defun flush-assets () (setf assets nil)))

;; (defmacro defpipeline (name args-or-stage &body stages)
;;   (let* ((stages (if (symbolp args-or-stage)
;;                      (cons args-or-stage stages)
;;                      stages))
;;          (args (if (symbolp args-or-stage)
;;                    (extract-args-from-gpu-functions stages)
;;                    args-or-stage)))
;;     (utils:assoc-bind ((in-args nil) (unexpanded-uniforms :&uniform)
;;                        (context :&context) (instancing :&instancing))
;;         (utils:lambda-list-split '(&uniform &context &instancing) args)
;;       (declare (ignore instancing))
;;       (let* ((uniforms (expand-equivalent-types unexpanded-uniforms))
;;              (init-func-name (symb-package :cgl '%%- name))
;;              (invalidate-func-name (symb-package :cgl '££- name))
;;              (dispatch-func-name (symb-package :cgl '$$-dispatch- name))
;;              (uniform-details (mapcar #'make-arg-assigners uniforms))
;;              (varjo-args
;;               `(,@in-args
;;                 ,@(when unexpanded-uniforms
;;                         `(&uniform
;;                           ,@(loop :for (name type) :in unexpanded-uniforms :collect
;;                                (if (gethash type *gl-equivalent-type-data*)
;;                                    `(,name ,(symbol-to-bridge-symbol type))
;;                                    `(,name ,type)))))
;;                 ,@(when context `(&context ,@context)))))
;;         `(let ((program-id nil)
;;                ,@(let ((u-lets (mapcan #'first uniform-details)))
;;                       (loop for u in u-lets collect `(,(first u) -1))))
;;            ,(gen-pipeline-invalidate invalidate-func-name)
;;            ,(gen-pipeline-init init-func-name varjo-args name
;;                                invalidate-func-name (copy-tree uniform-details) stages)
;;            ,(gen-dispatch-func dispatch-func-name init-func-name context
;;                                unexpanded-uniforms (copy-tree uniform-details))
;;            ,(gen-dummy-func init-func-name name unexpanded-uniforms
;;                             (copy-tree uniform-details)))))))

;; (defun shader-args-to-lisp-args (args)
;;   (let* ((uni-pos (position '&uniform args :test #'equal :key #'symbol-name))
;;          (context-pos (position '&context args :test #'equal :key #'symbol-name))
;;          (in-vars (subseq args 0 (or uni-pos context-pos)))
;;          (uniforms (when uni-pos (subseq args (1+ uni-pos) context-pos))))
;;     `(,@(mapcar #'first in-vars)
;;         ,@(when uniforms (cons '&key (mapcar #'first uniforms))))))


;; (defmacro defvshader (name args &body body)
;;   (let ((args (if (find '&context args :test #'symbol-name-equal)
;;                   (append (copy-list args) `(:vertex)) ; :stemcells
;;                   (append (copy-list args) `(&context :vertex))))) ; :stemcells
;;     `(defshader ,name ,args ,@body)))
;; (defmacro deffshader (name args &body body)
;;   (let ((args (if (find '&context args :test #'symbol-name-equal)
;;                   (append (copy-list args) `(:fragment)) ; :stemcells
;;                   (append (copy-list args) `(&context :fragment))))) ; :stemcells
;;     `(defshader ,name ,args ,@body)))
;; (defmacro defgshader (name args &body body)
;;   (let ((args (if (find '&context args :test #'symbol-name-equal)
;;                   (append (copy-list args) `(:geometry)) ; :stemcells
;;                   (append (copy-list args) `(&context :geometry))))) ; :stemcells
;;     `(defshader ,name ,args ,@body)))

(in-package :cepl.pipelines)

(defun bake (pipeline &rest uniforms &key &allow-other-keys)
  (let* ((pipeline (etypecase pipeline
                     ((or list symbol) (pipeline-spec pipeline)))))
    (%test-bake-compile-link-and-upload
     (remove-if-not #'cdr (pipeline-stage-pairs pipeline))
     uniforms)))


(defun %test-bake-compile-link-and-upload (stage-pairs replacements)
  (let* ((stage-pairs (pairs-key-to-stage stage-pairs))
	 (glsl-version (compute-glsl-version-from-stage-pairs stage-pairs))
	 (stage-pairs (swap-versions stage-pairs glsl-version)))
    (mapcar (lambda (pair)
              (test-parsed-gpipe-args->v-translate-args pair replacements))
            stage-pairs)))


(defun test-parsed-gpipe-args->v-translate-args (stage-pair replacements)
  "%varjo-compile-as-pipeline simply takes (stage . gfunc-name) pairs from
   %compile-link-and-upload needs to call v-rolling-translate. To do this
   we need to look up the gpu function spec and turn them into valid arguments
   for the v-rolling-translate function.
   That is what this function does.
   It also:
   [0] if it's a glsl-stage then it is already compiled. Pass the compile result
       and let varjo handle it
   [1] enables implicit uniforms
   [2] validate that either the gpu-function's context didnt specify a stage
       explicitly or that, if it did, that it matches the stage it is being used
       for now"
  (dbind (stage-type . stage) stage-pair
    (if (typep (gpu-func-spec stage) 'glsl-stage-spec)
	(with-glsl-stage-spec (gpu-func-spec stage)
	  compiled);;[0]
	(dbind (in-args uniforms context code) (get-func-as-stage-code stage)
	  ;;[2]
	  (let ((n (count-if (lambda (_) (member _ varjo:*stage-types*))
			     context)))
	    (assert (and (<= n 1) (if (= n 1) (member stage-type context) t))))
	  (let ((context (cons :iuniforms ;;[1]
			       (cons stage-type
				     (remove stage-type context)))))
            (print uniforms)
            (print replacements)
	    (list in-args
		  uniforms
		  context
		  `(progn ,@code)))))))

(in-package :cepl.errors)

(deferror gfun-invalid-arg-format () (gfun-name invalid-pair)
    "CEPL - defun-g: defun-g expects its parameter args to be typed in the~%format (var-name type) but instead ~s was found in the definition for ~s" invalid-pair gfun-name)

(deferror gpu-func-spec-not-found () (name types)
    "CEPL - gpu-func-spec: Could not find spec for the gpu-function named ~s
with the in-arg types ~a"
  name types)

(deferror dispatch-called-outside-of-map-g () (name)
    "Looks like you tried to call the pipeline ~s without using map-g.~%" name)

(deferror invalid-keywords-for-shader-gpipe-args () (pipeline-name keys)
    "Found some invalid keys in the g-> for for the pipeline called ~a:~%~s"
    pipeline-name keys)

(deferror invalid-context-for-assert-gpipe () (context)
    "CEPL: ~a is an invalid context for asserting whether gpipe args are valid"
    context)

(deferror invalid-context-for-assert-options () (context)
    "CEPL: ~a is an invalid context for asserting whether pipeline options are valid"
    context)

(deferror invalid-shader-gpipe-form () (pipeline-name valid-forms invalid-forms)
    "When using defpipeline to compose gpu functions the valid arguments to g-> are function literals~%(optionally with keyword stage names).~%~%In the defpipeline for ~a ~athese forms were not valid:~%~{~s~%~}~%"
  pipeline-name
  (if valid-forms
      (format nil "these forms were valid:~%~{~s~%~}~%However"
              valid-forms)
      "")
  invalid-forms)

(deferror not-enough-args-for-implicit-gpipe-stages () (pipeline-name clauses)
    "Tried to compile the g-> form for the ~a pipeline, however there are not enough functions here for a valid pipeline:~%~s"
    pipeline-name clauses)

(deferror invalid-shader-gpipe-stage-keys () (pipeline-name keys)
    "In the defpipeline form for ~s the gpipe args are incorrect.~%~s"
  pipeline-name
  (let ((unknown-keys (remove-if (lambda (x) (member x varjo:*stage-types*))
                                 keys)))
    (if unknown-keys
        (format nil "The following stages are not supported, or are incorrectly named ~a"
                unknown-keys)
        (format nil "The order of the following stages is incorrect:~%~s~%Valid order of stages is ~a"
                keys varjo:*stage-types*))))

(deferror invalid-compose-gpipe-form () (pipeline-name clauses)
  "In the defpipeline for ~s there are some invalid pass clauses.~%

~{~a~%~}

A pass clause's first element is the destination, the last element is the call
to another cepl pipeline. The elements between these are optional lisp forms to
 run in the context of the destination.

Example valid forms:
~%(f0 (blit stream :tex tx))         -- where f0 is an fbo
~%(nil (blit stream :tex tx))        -- nil stands in for the default fbo
~%(f0 (clear) (blit stream :tex tx)) -- where the #'clear is running inside the implicit with-fbo"
  pipeline-name clauses)

(deferror invalid-defpipeline-options () (pipeline-name invalid-options valid-options)
    "CEPL - defpipeline: The defpipeline for ~a contained the following invalid options:~%~a~%The valid options to this form of defpipeline are:~s" pipeline-name invalid-options valid-options)

(deferror shader-pipeline-non-null-args () (pipeline-name)
    "CEPL - defpipeline: In defpipeline for ~a. Args are not needed in pipelines composed of g-functions"
    pipeline-name)


(deferror make-tex-no-content-no-type () ()
    "CEPL - make-texture: Trying to make texture but have element-type and also
no initial-contents to infer the type from")

(deferror make-tex-array-not-match-type ()
    (element-type image-format array-type)
    "CEPL - make-texture: Trying to make texture but the element-type given was
~s which implies an image-format of ~s. This conflicts with the array
element-type of ~s"
  element-type image-format array-type)

(deferror make-tex-array-not-match-type2 () (element-type initial-contents)
    "CEPL - make-texture: Trying to make texture with an element-type of ~s,
however the initial-contents provided do not seem to be compatible:~%~s"
  element-type initial-contents)

(deferror image-format->lisp-type-failed () (type-name)
    "CEPL - make-texture: to find a conversion from the image-format ~s to a lisp type"
  type-name)

(deferror lisp-type->image-format-failed () (type-name)
    "CEPL - make-texture: to find a suitable conversion from the lisp type ~s to an
internal texture format"
  type-name)

(deferror pixel-format->image-format-failed () (type-name)
    "CEPL - make-texture: to find a suitable conversion from the pixel format ~s to an
internal texture format"
  type-name)

(deferror image-format->pixel-format-failed () (type-name)
    "CEPL - unable to find a suitable conversion from the internal
 texture format ~s to a pixel format"
  type-name)

(deferror buffer-backed-texture-invalid-args () ()
    "CEPL - make-texture: Buffer-backed textures cannot have mipmaps, multiple layers or be cube rectangle or multisample")

(deferror buffer-backed-texture-invalid-samplers () ()
    "CEPL - make-texture: We do not currently support setting any texture sampling parameters on buffer backed textures")

(deferror buffer-backed-texture-invalid-image-format () (type-name)
    "CEPL - make-texture: The internal format ~a is invalid for use with buffer-backed-textures"
  type-name)

(deferror buffer-backed-texture-establish-image-format () (type-name)
  "CEPL - make-texture: Could not establish the correct texture type for a buffer texture: ~a"
  type-name)

(deferror failed-to-test-compile-gpu-func (:error-type warning) (gfunc-name missing-func-names)
    "CEPL - defun-g: Failed to test compile the gpu function named '~s
 due to not all dependent functions having been compiled yet.
 Missing funcs: ~s
 To disable this warning for all future compilations:
 (setf cepl.pipelines:*warn-when-cant-test-compile* nil)" gfunc-name missing-func-names)


(deferror dont-define-space-to-self () (space)
    "with-model-space: please dont try redefining the relationship between ~s and itself."
  space)

(deferror make-buffer-stream-with-no-gpu-arrays () ()
    "Cepl: Invalid attempt to make buffer-stream with no gpu-arrays")

(deferror invalid-context-for-def-glsl-stage () (name context)
    "CEPL: Invalid context supplied for glsl-stage ~a:
The context must, at least, contain:
- One of the following versions: ~a
- One of the following stage names: ~a

Instead recieved: ~a"
  name varjo::*supported-versions* varjo::*supported-stages* context)

(deferror struct-in-glsl-stage-args () (arg-names)
    "Found arguments to def-glsl-stage which have struct types.
Arg names: ~s
This is not currently supported by def-glsl-stage"
  arg-names)

(deferror make-gpu-array-from-c-array-mismatched-dimensions ()
    (c-arr-dimensions provided-dimensions)
    "CEPL: make-gpu-array mismatched dimensions

A call to #'make-gpu-array was made with a c-array as the initial-contents.
The dimensions of the c-array are ~s, however the dimensions given in the
call to #'make-gpu-array were ~s"
  c-arr-dimensions provided-dimensions)

(deferror multiple-gpu-func-matches () (designator possible-choices)
    "CEPL: def-g-> found a stage that was incorrectly specified.

The problematic defintition was: ~s

The problem is in this case was that CEPL found multiple GPU function
definitions with the same name so was unable to pick the correct one.

Instead of ~s please use one of the following:
~{~s~^~%~}"
  designator designator possible-choices)

(deferror stage-not-found () (designator)
    "CEPL - def-g->: Could not find a gpu-function called ~s.
This most likely means it hasn't been compiled yet or that the name is incorrect"
  designator)

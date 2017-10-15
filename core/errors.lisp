(in-package :cepl.errors)

(deferror gfun-invalid-arg-format () (gfun-name invalid-pair)
    "CEPL - defun-g: defun-g expects its parameter args to be typed in the~%format (var-name type), but instead ~s was found in the definition for ~s" invalid-pair gfun-name)

(deferror gpu-func-spec-not-found () (name types)
    "CEPL - gpu-func-spec: Could not find spec for the gpu-function named ~s
with the in-arg types ~s"
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
    "When using defpipeline-g to compose GPU functions, the valid arguments to g-> are function literals~%(optionally with keyword stage names).~%~%In the defpipeline-g for ~a ~athese forms were not valid:~%~{~s~%~}~%"
  pipeline-name
  (if valid-forms
      (format nil "these forms were valid:~%~{~s~%~}~%However"
              valid-forms)
      "")
  invalid-forms)

(deferror not-enough-args-for-implicit-gpipe-stages () (pipeline-name clauses)
    "Tried to compile the g-> form for the ~a pipeline; however, there are not enough functions here for a valid pipeline:~%~s"
  pipeline-name clauses)

(deferror invalid-shader-gpipe-stage-keys () (pipeline-name keys)
    "In the defpipeline-g form for ~s the gpipe args are incorrect.~%~s"
  pipeline-name
  (let ((unknown-keys (remove-if (lambda (x) (member x varjo:*stage-names*))
                                 keys)))
    (if unknown-keys
        (format nil "The following stages are not supported, or are incorrectly named: ~a"
                unknown-keys)
        (format nil "The order of the following stages is incorrect:~%~s~%Valid order of stages is: ~a"
                keys varjo:*stage-names*))))

(deferror invalid-compose-gpipe-form () (pipeline-name clauses)
    "In the defpipeline-g for ~s there are some invalid pass clauses.~%

~{~a~%~}

A pass clause's first element is the destination; the last element is the call
to another CEPL pipeline. The elements between these are optional Lisp forms to
 run in the context of the destination.

Example valid forms:
~%(f0 (blit stream :tex tx))         -- where f0 is an fbo
~%(nil (blit stream :tex tx))        -- nil stands in for the default fbo
~%(f0 (clear) (blit stream :tex tx)) -- where the #'clear is running inside the implicit with-fbo"
  pipeline-name clauses)

(deferror invalid-defpipeline-options () (pipeline-name invalid-options valid-options)
    "CEPL - defpipeline-g: The defpipeline-g for ~a contained the following invalid options:~%~a~%The valid options to this form of defpipeline-g are:~s" pipeline-name invalid-options valid-options)

(deferror shader-pipeline-non-null-args () (pipeline-name)
    "CEPL - defpipeline-g: In defpipeline-g for ~a. Args are not needed in pipelines composed of g-functions"
  pipeline-name)


(deferror make-tex-no-content-no-type () ()
    "CEPL - make-texture: Trying to make texture, but have no element-type or
initial-contents to infer the type from")

(deferror make-tex-array-not-match-type ()
    (element-type pixel-format supposed-type array-type)
    "CEPL - make-texture: Trying to make texture, but the element-type given was
~s which implies an pixel-format of ~s.
That pixel-format would require an array element-type of ~s.
This conflicts with the array element-type of ~s"
  element-type pixel-format supposed-type array-type)

(deferror make-tex-array-not-match-type2 () (element-type initial-contents)
    "CEPL - make-texture: Trying to make texture with an element-type of ~s,
however, the initial-contents provided do not seem to be compatible:~%~s"
  element-type initial-contents)

(deferror image-format->lisp-type-failed () (type-name)
    "CEPL - make-texture: to find a conversion from the image-format ~s to a Lisp type"
  type-name)

(deferror lisp-type->image-format-failed () (type-name)
    "CEPL - make-texture: to find a suitable conversion from the Lisp type ~s to an
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
    "CEPL - make-texture: Buffer-backed textures cannot have mipmaps, multiple layers, or be cube rectangle or multisample")

(deferror buffer-backed-texture-invalid-samplers () ()
    "CEPL - make-texture: We do not currently support setting any texture sampling parameters on buffer-backed textures")

(deferror buffer-backed-texture-invalid-image-format () (type-name)
    "CEPL - make-texture: The internal format ~a is invalid for use with buffer-backed-textures"
  type-name)

(deferror buffer-backed-texture-establish-image-format () (type-name)
    "CEPL - make-texture: Could not establish the correct texture type for a buffer texture: ~a"
  type-name)

(deferror failed-to-test-compile-gpu-func (:error-type warning) (gfunc-name missing-func-names)
    "CEPL - defun-g: Failed to test compile the gpu function named '~s,
 as not all dependent functions having been compiled yet.
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
  name varjo:*supported-versions* varjo:*stage-names* context)

(deferror struct-in-glsl-stage-args () (arg-names)
    "Found arguments to def-glsl-stage which have struct types.
Arg names: ~s
This is not currently supported by def-glsl-stage"
  arg-names)

(deferror make-gpu-array-from-c-array-mismatched-dimensions ()
    (c-arr-dimensions provided-dimensions)
    "CEPL: make-gpu-array mismatched dimensions

A call to #'make-gpu-array was made with a c-array as the initial-contents.
The dimensions of the c-array are ~s; however, the dimensions given in the
call to #'make-gpu-array were ~s"
  c-arr-dimensions provided-dimensions)

(deferror symbol-stage-designator () (designator possible-choices)
    "CEPL: defpipeline-g found a stage that was incorrectly specified.

The problematic defintition was: ~s

The problem: because of potential overloading, CEPL stages must be fully qualified.

~a"
  designator
  (if (= (length possible-choices) 1)
      (format nil "Instead of ~s please use: ~s"
              designator (first possible-choices))
      (format nil "Instead of ~s please use one of the following:~%~{~s~^~%~}"
              designator possible-choices)))

(deferror symbol-stage-designators () (designator-choice-pairs)
    "CEPL: defpipeline-g found a stage that was incorrectly specified.

The problematic stage designators were:
~{~s ~}

The problem: because of potential overloading, CEPL stages must be fully
qualified. ~{~%~%~a~}"
  (mapcar #'first designator-choice-pairs)
  (loop :for (designator choices) :in designator-choice-pairs :collect
     (if (= (length choices) 1)
         (format nil "Instead of ~s, please use: ~s"
                 designator (first choices))
         (format nil "Instead of ~s, please use one of the following:~%~{~s~^~%~}"
                 designator choices))))

(deferror stage-not-found () (designator)
    "CEPL - defpipeline-g: Could not find a gpu-function called ~s.
This most likely means it hasn't been compiled yet or that the name is incorrect"
  designator)

(deferror pixel-format-in-bb-texture () (pixel-format)
    "CEPL: make-texture was making a buffer-backed texture, however a
pixel-format was provided. This is invalid, as pixel conversion is not done when
uploading data to a buffer-backed texture.

Pixel-format: ~s"
  pixel-format)

(deferror glsl-version-conflict () (pairs)
    "CEPL: When trying to compile the pipeline we found some stages which have
conflicting glsl version requirements:
~{~s~%~}" pairs)

(deferror glsl-version-conflict-in-gpu-func () (name context)
    "CEPL: When trying to compile ~a we found multiple glsl versions.
Context: ~a" name context)

(deferror delete-multi-func-error () (name choices)
    "CEPL: When trying to delete the GPU function ~a we found multiple
overloads and didn't know which to delete for you. Please try again using one of
the following:
~{~s~%~}" name choices)

(deferror multi-func-error () (name choices)
    "CEPL: When trying find the gpu function ~a we found multiple overloads and
didn't know which to return for you. Please try again using one of
the following:
~{~s~%~}" name choices)

(defwarning pull-g-not-cached () (asset-name)
    "Either ~s is not a pipeline/gpu-function or the code for this asset
has not been cached yet"
  asset-name)

(deferror pull*-g-not-enabled () ()
    "CEPL has been set to not cache the results of pipeline compilation.
See the +cache-last-compile-result+ constant for more details")

(defwarning func-keyed-pipeline-not-found () (callee func)
    "CEPL: ~a was called with ~a.

When functions are passed to ~a we assume this is a pipeline function and looked
for the details for that pipeline. However we didn't find anything.

Please note that you cannot lookup gpu-functions in this way as, due to
overloading, many gpu-functions map to a single function object."
  callee func callee)

(deferror attachments-with-different-sizes (:print-circle nil) (args sizes)
    "CEPL: Whilst making an fbo we saw that some of the attachments will end up
having different dimensions: ~a

Whilst this is not an error according to GL it can trip people up because
according to the spec:

 > If the attachment sizes are not all identical, rendering will
 > be limited to the largest area that can fit in all of the
 > attachments (an intersection of rectangles having a lower left
 > of (0 0) and an upper right of (width height) for each attachment).

If you want to make an fbo with differing arguments, please call make-fbo
with `:matching-dimensions nil` in the arguments e.g.

 (MAKE-FBO ~{~%     ~a~})

"
  sizes
  (labels ((ffa (a)
             (typecase a
               ((or null keyword) (format nil "~s" a))
               ((or list symbol) (format nil "'~s" a))
               (otherwise (format nil "~s" a)))))
    (append (mapcar #'ffa args)
            '(":MATCHING-DIMENSIONS NIL"))))


(deferror invalid-cube-fbo-args () (args)
    "CEPL: Invalid args for cube-map bound fbo:

args: ~s

You have passed a cube-map texture without an attachment number; this
means you want the fbo to have 6 color attachments which are bound to the
faces of the cube texture.

Whilst using this feature, the only other legal argument is depth
attachment info.
" args)


(deferror functions-in-non-uniform-args () (name)
    "
CEPL: We currently only support functions as uniform arguments.

Pipeline: ~s"
  name)

(deferror mapping-over-partial-pipeline () (name args)
    "CEPL: This pipeline named ~s is a partial pipeline.

This is because the following uniform arguments take functions:
~{~%~s~}

As OpenGL does not itself support passing functions as values you must use
bake-uniforms to create set the uniforms above. This will generate
a 'complete' pipeline which you can then map-g over.
" name args)

(deferror fbo-target-not-valid-constant () (target)
    "CEPL: with-fbo-bound form found with invalid target

The target must be constant and must be one of the following:

- :framebuffer
- :read-framebuffer
- :draw-framebuffer

In this case the compile-time value of 'target' was: ~a
" target)

(deferror bake-invalid-pipeling-arg () (invalid-arg)
    "CEPL: The pipeline argument to #'bake was expected to be a pipeline name or
pipeline function object.

Instead we found: ~s"
  invalid-arg)

(deferror bake-invalid-uniform-name () (proposed invalid)
    "CEPL: An attempt to bake some uniforms in a pipeline has failed.

The arguments to be baked were:
~{~s ~s~^~%~}

However the following uniforms were not found in the pipeline:
~{~s~^ ~}"
  proposed invalid)

(deferror bake-uniform-invalid-values (:print-circle nil) (proposed invalid)
    "CEPL: An attempt to bake some uniforms in a pipeline has failed.

The arguments to be baked were:
~{~s ~s~^~%~}

However the following values are ~a, they are not representable in shaders.
~{~s~^ ~}

Might you have meant to specify a gpu function?"
  proposed
  (cond
    ((every #'symbolp invalid) "symbols")
    ((every #'listp invalid) "lists")
    (t "invalid"))
  invalid)

(deferror partial-lambda-pipeline (:print-circle nil) (partial-stages)
    "CEPL: G-> was called with at least one stage taking functions as uniform
arguments.

If this were defpipeline-g we would make a partial pipeline however we don't
currently support partial lambda pipelines.

Sorry for the inconvenience. It is a feature we are interested in adding so if
this is causing you issues please reach out to us on Github.

The problem stages were:
~{~%~s~}"
  partial-stages)

(deferror glsl-geom-stage-no-out-layout (:print-circle nil) (glsl-body)
    "CEPL: def-glsl-stage was asked to make a geometry stage however it
could not find a valid 'out' layout declaration. These lines look something
like this:

layout(<primitive-name>, max_vertices=20) out;

Where <primitive-name> is one of points, line_strip or triangle_strip and
max_vertices is any integer.

Here is the block of glsl we search in:

~a" glsl-body)

(deferror invalid-inline-glsl-stage-arg-layout (:print-circle nil) (name arg)
    "CEPL: Invalid arg layout found in ~a. The correct layout for a argument to
a glsl-stage is (\"string-name-of-arg\" arg-type ,@keyword-qualifiers)

Problematic arg was: ~a"
  name arg)


(deferror adjust-gpu-array-mismatched-dimensions () (current-dim new-dim)
    "CEPL: adjust-gpu-array cannot currently change the number of dimensions in
a gpu-array.

current dimensions: ~a
proposed new dimensions: ~a

If working around this limitation proves to be too difficult please report it
on github so we can re-evaluate this limitation."
  current-dim new-dim)

(deferror adjust-gpu-array-shared-buffer () (array shared-count)
    "CEPL: adjust-gpu-array cannot currently adjust the size of gpu-array
which is sharing a gpu-buffer with other gpu-arrays.

Array: ~a is sharing a gpu buffer with ~a other gpu-arrays"
  array shared-count)

(deferror buffer-stream-has-invalid-primtive-for-stream ()
    (name pline-prim stream-prim)
    "CEPL: The buffer-stream passed to ~a contains ~s, however ~a
was expecting ~s.

You can either change the type of primtives the pipeline was expecting e.g:

 (defpipeline-g ~s (~s)
   ..)

Or you can create a stream with containing ~a e.g:

  (make-buffer-stream gpu-array :draw-mode ~s)

It is also worth noting that it is possible to pass triangles to a
pipeline declared to take (:patch 3), to pass lines to pipelines declared
to take (:patch 2) and points to pipelines taking (:patch 1)"
  name stream-prim name pline-prim
  name stream-prim
  pline-prim pline-prim)

(deferror invalid-options-for-texture ()
    (buffer-storage
     cubes dimensions layer-count mipmap multisample rectangle)
    "CEPL: We could not establish the correct texture type for the following
combination of options:

buffer-storage - ~a
cubes          - ~a
dimensions     - ~a
layer-count    - ~a
mipmap         - ~a
multisample    - ~a
rectangle      - ~a

If the GL spec says this is valid then we are sorry for the mistake. If you
have the time please report the issue here:
https://github.com/cbaggers/cepl/issues"
  buffer-storage cubes dimensions layer-count mipmap multisample rectangle)

(deferror gpu-func-symbol-name () (name alternatives env)
    "CEPL: We were asked to find the gpu function named ~a. Now we did find
~a however as gpu-functions can be overloaded we now require that you specify
the types along with the name. This is slightly more annoying when there is
only one match, however it eliminates the ambiguity that occurs as soon as
someone does overload the gpu-function.

Here are the possible names for this function:
~{~a~}
~@[
You may pick a implementation to use this time but, as this will not update
your code, you will get this error on the next compile unless it is fixed~]"
  name
  (if (> (length alternatives) 1)
      "matches"
      "a match")
  alternatives
  (not (null env)))

(deferror gl-context-initialized-from-incorrect-thread ()
    (ctx-thread init-thread)
    "CEPL: This CEPL context is tied to thread A (below) however something tried
to create the gl-context from thread B:
A: ~a
B: ~a"
  ctx-thread init-thread)

(deferror tried-to-make-context-on-thread-that-already-has-one ()
    (context thread)
    "CEPL: An attempt was made to create a context on thread ~a however
that thread already has the CEPL context ~a.

In CEPL you may only have 1 CEPL context per thread. That context then holds
the handles to any GL contexts and any caching related to those gl contexts"
  thread context)

(deferror max-context-count-reached () (max)
    "CEPL: Currently CEPL has a silly restriction on the number of contexts
that can be active at once. The current maximum is max.

It's silly in that GL itself doesnt have this restriction and it was only
introduced in CEPL to make the implementation of multi-threading simpler.

This restriction does need to be removed however so if you are hitting
this then please report it at https://github.com/cbaggers/cepl. This lets us
know that this is causing real issues for people and we can prioritize it
accordingingly.")


;; Please remember the following 2 things
;;
;; - add your condition's name to the package export
;; - keep this comment at the bottom of the file.
;;
;; Thanks :)

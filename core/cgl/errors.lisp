(in-package :cgl)

(deferror invalid-stages () (invalid-names)
    "CEPL - Pipeline: The following stages don't have specifications ~s.~%This most likely means they havent been compiled yet or that the names are incorrect" invalid-names)

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

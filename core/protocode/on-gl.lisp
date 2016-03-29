;; Kill defpipeline compose
;;
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Delayed Init
;;
;; The fbo section is a symptom of a bigger problem, make-* before context.
;;
;; for every gpu type:
;;
;; - make a function that makes a placeholder
;;
;; - change the init function to take the placeholder and update it
;;   instead of making their own object
;;
;; - make all public make-* functions just make a placeholder and pass it
;;   and the args to the real init function
;;
;; - make all public make-* functions check for context, if missing capture
;;   all args in closure and add to on-context-callback list
;;
;; Now all make-* can be used in top-level closures, defvar, defparameter
;; this is awesome!
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Auto Uniforms
;;
;; Are too complex, explaining the feature properly is hard, doesnt justify
;; whole defpipeline macro.
;;
;; Keep uniform query code for livecoding
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Attachment Shorthand
;;
;; Make with-fbo-bound prettier
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Render into FBO syntax
;;
;; Make map-g-into have inline with-fbo-bound syntax handling
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Fast attachment binding
;;
;; Hmm we could have a primitive here.
;;
;; Then with-fbo-bound can have some slow semantics as we can use _the-name_
;;
;; Needs to be the dual of sampler...maybe gbuffer? maybe render-target..
;; but maybe gbuffer?
;;
;; Nah render-target is a better name in this case. Ooo this could work
;;
;; MAYBE map-g-into ONLY works on render-targets
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;;
;; With the above we could get rid of the defpipeline for composing pipelines
;; and instead use defun.
;;
;; Less complexity, it's worth it
;;

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

;;
;; WAIT
;;
;; with-sampler sucks...why would you want the same sampler across all textures
;; I mean look at this:

(with-sampler s
  (map-g #'test stream :tex-a a :tex-b b))

;; fuck that
;;
;; sampler should really be like buffer-stream, it refers to a texture.
;; then you sample from the sampler, nice.
;;
;; Also we can have specific types for all of these, a little extra type
;; checking wont hurt :)
;;
;; Then maybe let's ditch sampling params from the texture and go full sampler.
;;
;; Got some real data on this
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; @gl-people: Are there any cases where setting the sampling params
;; directly on a texture is better than using a sampler object? (putting
;; aside the amount of code to use them & supporting v<3.3)

;; erik [5:16 PM] @baggers: No, always use sampler objects, they
;; rule. (edited)

;; baggers [5:22 PM] @erik: badass, thanks!
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Ok so lose sampler params from texture, sampler object only, adds semantic
;; dual with gpu-stream so woo!

;;----------------------------------------------------------------------
;;
;; High level view
;;
;; Data is stored in gpu-buffers and textures
;;
;; Both have contain gpu-arrays
;;
;; Depending on whether the array is contained in a gpu-buffer or a texture it is said to be buffer backed or texture backed
;;
;; Pipelines are made by composing gpu-functions with g-> and def-g->
;;
;; Data is read from gpu-arrays using gpu-streams or ubos
;;
;; Data is read from textures using samplers
;;
;; Data leaving a pipeline gets written into an FBO

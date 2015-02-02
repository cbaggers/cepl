(in-package :cgl)

;; {TODO} move this to gl-context.lisp
(defstruct default-framebuffer)
(defparameter %current-fbo (make-default-framebuffer))

;; {TODO} need to put this in some macros utils package
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun function-formp (x) x))

;; This macro runs the shader pipeline and returns the currently bound fbo
;; This with-bind-fbo macro will shadow the gmap macro in order to use the
;; hard-bind-fbo argument and optimize the return of the current-fbo
;; 
;; We have to make %current-fbo special so that we can have a gmap inside 
;; a function that was called from inside a block in which with-bind-fbo
;; was used.
(defmacro %gmap (hard-bind-fbo pipeline-func stream uniforms)
  (assert (function-formp pipeline-func))
  (let ((pipeline-name (second pipeline-func)))
    `(progn
       (,(symb-package :cgl :$$-dispatch- pipeline-name) ,stream ,@uniforms)
       ,(if hard-bind-fbo
            hard-bind-fbo
            'cgl::%current-fbo))))


;; This is a passthrough macro that exists so that with-bind-fbo can shadow it
(defmacro gmap (pipeline-func stream &rest uniforms)
  `(%gmap nil ,pipeline-func ,stream ,uniforms))

;; Ok got some macro madness going on here so I want to be clear what
;; is happening. Here are some facts
;;
;; - Gmap runs a pipeline with the input and then returns a framebuffer
;; - the current framebuffer is in the %current-fbo var.
;;
;; Given that, if we are inside a with-bind-fbo, we know what the the
;; fbo will be, then we can return the local var and not have to query
;; the special var which would have an overhead associated with it.
;; Q: is this moot because of having to set %current-fbo
;; A: I hope not, I think given correct settings the compiler may be able 
;;    to optimize away this let as nothing uses it.
(defmacro with-bind-fbo ((fbo target &optional (unbind t)) &body body)
  (labels ((inject-gmap-form (fbo-symbol)
             (subst fbo-symbol 'a
                    ``(%gmap a ,pipeline-func ,stream ,uniforms))))
    (let ((once-fbo (gensym "once-fbo")))
      `(macrolet ((gmap (pipeline-func stream &rest uniforms) 
                    ,(inject-gmap-form once-fbo)))
         (let* ((,once-fbo ,fbo)
                (%current-fbo ,once-fbo))
           (%bind-fbo ,once-fbo ,target)
           (prog1 (progn ,@body)
             ,(when unbind `(%unbind-fbo))))))))


;; EXAMPLES
;;
;; (gmap #'test a :tex tx)
;;
;; (macroexpand-dammit:macroexpand-dammit 
;;       '(with-bind-fbo (some-fbo :framebuffer)
;;         (let ((jam (gmap #'test a :tex tx)))
;;           (print jam))))


;; Deliberatly innefficient very of gmap that will create a temporary stream
;; if you give dont give it one. It will even create a temporary gpu-array
;; to hold the data, MADNESS!
;; (defmacro gmap~ (pipeline-func stream &rest uniforms)
;;   ())

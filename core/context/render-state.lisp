(in-package :cepl.render-state)

;; 2.5 Context State
;; Context state is state that belongs to the GL context as a whole, rather than
;; to instances of the different object types

;; There are two types of context state.
;; Server state resides in the GL server; the majority of GL state falls into
;; this category.
;; Client state resides in the GL client. Unless otherwise specified, all state
;; is server state;

;; printed out the state tables.. gotta go through them
;; Can at least start by finding the binding funcs.




;; GL Objects (not directly related)
;; ---------------------------------
;; - object/name creation/deletion
;; - shared object state (think shared contexts)
;; - buffer objects (done)
;; - shader objects (deleted so fast I dont really have to manage them)
;; - program objects (abstracted already..though would like lambda versions)
;; - program pipeline objects (not using these)
;; - texture objects (done)
;; - sampler objects (wip)
;; - renderbuffer objects (nope, and in no hurry to add these)
;; - fbos (of course :D)
;; - vaos (mostly done, would like to support the draw-base* commands)
;; - query objects (nothing yet)
;; - sync objects (nothing yet)

(defstruct context-state)

(defstruct render-state
  (context-state nil :type (or context-state null)))

(let ((render-state (make-render-state)))
  (defun get-render-state ()
    render-state))

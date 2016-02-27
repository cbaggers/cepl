;; During execution of pipelines we mtutate a lot of gl state and we would like
;; to set it back when we are done rendering. Unfortunately this can mean a lot
;; of uneccesary state changes which in turn hurts performance.

;; This can be remedied inside a pipeline by caching the state and the start
;; (using let and special vars) and then not setting any of the special vars
;; inside the block.

;; To aid this we need to make sure that internally we dont rely on the special
;; vars and make them more of a user construct.

;; The above is ok but we really want to minimize unncessary state changes
;; between different pipelines. This will be done using a shared gl-state-struct

;; The primitive with be created like this.

(with-shared-gl-state (x &optional state-object)
  ...)

;; Where is the name of the variable that which will be delared to hold the
;; state.
;; It will expand something like this:

(let ((x (or state-object (make-gl-state))))
  (prog1
      (progn
        ,@body)
    (restore-gl-state x)))

;; The struct will hold the values of the state at point of creation and at the
;; end of the block it will be restored.

;; with-shared-gl-state has a dynamically scoped effect. Within this block
;; gl-state is not kept clean and calls that bind objects are not obliged to
;; unbind them.
;; inside the scope the special vars are meaningless

;; The state object is then passed into pipelines via gmap. Normally pipelines
;; will use state objects internally to manage state but in this case they will
;; use the shared object passed in.
;; This explicit passing makes it possible to minimze the switching overhead
;; between calls but puts a task on the user

;; ANY change to gl state that is not recorded in the state object will not be
;; know to the pipelines and could cause issues. You must therefore ONLY use
;; cepl functions that support the gl-state-object as an optional parameter.

;; Maybe we can also use this primitive to make external gl code safer.

(with-saved-gl-state ()
  (calls to raw gl)
  (calls to other c libs that mess with gl))

;; To speed this up we can also provide details of what to save

(with-saved-gl-state (:textures :programs)
  (devil:load-image-to-texture ...))

;; here it wont try save and restore all the state it could.

;; All of this assumes you are doing gl with one context per thread
;; and not passing state objects between them! This would be madness

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; GL State is actually dynamically scoped...I'd never thought of this
;; but it means languages that have primitives and concepts for
;; dynamic scope can encapsulate this in a way that feel natural to the
;; language.
;;
;; Now we are not going to use the actual dynamic scope features as I want
;; to stack allocate these state objects but we will make the primitves
;; behave in a dynamic way, they will just explicitly pass and return state
;; objects.

;; Actually I may not bother returning the state object as we are just gonna
;; mutate the damn thing.

;; In this way we can statically type the args and use this basic stuff to
;; clean up the higher level abstractions in cepl.


;; Just want to test the sideffects here.

(defstruct gl-state-cache
  (some 0 :type fixnum)
  (vars 0 :type fixnum))

(declaim (ftype (function (gl-state-cache) null) restore-gl-state))
(defun restore-gl-state (state-cache)
  (declare (gl-state-cache state-cache))
  (format t "~%some = ~a vars = ~a~%" (gl-state-cache-some state-cache)
	  (gl-state-cache-vars state-cache))
  nil)

(defmacro with-shared-gl-state ((var-name &optional state-object) &body body)
  (let ((saved-sym (gensym "saved-state")))
    `(let* ((,var-name (let ((tmp ,state-object))
                         (if (and tmp (typep tmp 'gl-state-cache))
                             tmp
                             (make-gl-state-cache))))
            (,saved-sym (copy-gl-state-cache ,var-name)))
       (declare (gl-state-cache ,var-name ,saved-sym))
       (prog1
           (progn
             ,@body)
         (restore-gl-state ,saved-sym)))))


(defun test0 ()
  (with-shared-gl-state (x)
    (test1 x)
    (print (gl-state-cache-vars x))
    :done))


(defun test1 (state-cache)
  (setf (gl-state-cache-vars state-cache) 100))

;; Look at glspec 2.6.2

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; What if the default behaviour was never to clean up? never to unbind?
;; if we always assume the state dirty unless have the state-cache as a
;; guarentee?
;; Sounds good at first, makes more sense with foreign libs as who knows
;; what is happening. However it seems unneccesarily extreme. not unbinding
;; is for speed, and we wont be doing that when we use the state-cache
;; anyway. The advantages are we likely avoid odd side effects when doing shit
;; like making vaos (or even just bindings textures/samplers).

;; I do believe that the state shouldnt be trusted per se, but there isnt a
;; good reason not to make it harder for ourselves :)

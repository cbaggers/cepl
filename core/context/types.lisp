(in-package :cepl.context)

;;----------------------------------------------------------------------g

(defclass pending-surface ()
  ((title :initarg :title)
   (width :initarg :width)
   (height :initarg :height)
   (fullscreen :initarg :fullscreen)
   (resizable :initarg :resizable)
   (no-frame :initarg :no-frame)
   (hidden :initarg :hidden)
   (legacy-gl-version :initarg :legacy-gl-version)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-context-count+ 8))

(deftype context-id ()
  `(integer -1 ,+max-context-count+))

(defvar *free-context-ids*
  (loop :for i :below +max-context-count+ :collect i))

(defun get-free-context-id ()
  (or (pop *free-context-ids*)
      (error 'max-context-count-reached
             :max +max-context-count+)))

(defun discard-context-id (id)
  (push id *free-context-ids*))

(defstruct unbound-cepl-context
  (consumed nil :type boolean)
  (gl-context nil :type gl-context)
  (requested-gl-version nil :type t)
  (shared nil :type cepl-context)
  (surface nil :type t)
  (surfaces nil :type list))

(defstruct (cepl-context (:constructor %make-cepl-context)
                         (:conc-name %cepl-context-))
  (id (error "Context missing an ID") :type context-id)
  (gl-context nil :type (or null gl-context))
  (requested-gl-version nil :type t)
  (gl-version-float 0f0 :type single-float)
  (bound-thread nil :type (or null bt:thread))
  (uninitialized-resources nil :type list)
  (shared (error "Context must be initialized via #'make-context")
          :type (array cepl-context (*)))
  (surfaces
   (error "Context must be initialized via #'make-context")
   :type list)
  (current-program +unknown-gl-id+ :type gl-id)
  (current-tfs nil :type (or null transform-feedback-stream))
  (current-surface nil :type t)
  (vao-binding-id +unknown-gl-id+ :type vao-id)
  (current-viewport nil :type (or null viewport))
  (default-viewport nil :type (or null viewport))
  (current-scissor-viewports
   (make-array 32 :element-type '(or null viewport) :initial-element nil)
   :type (simple-array (or null viewport) (32)))
  (default-framebuffer nil :type (or null fbo))
  (read-fbo-binding nil :type (or null fbo))
  (draw-fbo-binding nil :type (or null fbo))
  (current-stencil-params-front nil :type (or null stencil-params))
  (current-stencil-params-back nil :type (or null stencil-params))
  (current-stencil-mask-front #xFF :type stencil-mask)
  (current-stencil-mask-back #xFF :type stencil-mask)
  (current-blend-params nil :type (or null blending-params))
  (fbos
   (make-array 0 :element-type 'fbo :initial-element +null-fbo+
               :adjustable t :fill-pointer 0)
   :type (array fbo (*)))
  (array-of-bound-gpu-buffers
   (make-array 12 :element-type '(or gpu-buffer null) :initial-element nil)
   :type (simple-array (or gpu-buffer null) (12)))
  (array-of-gpu-buffers
   (make-array 0 :element-type 'gpu-buffer :initial-element +null-gpu-buffer+
               :adjustable t :fill-pointer 0)
   :type (array gpu-buffer (*)))
  (array-of-ubo-bindings-buffer-ids
   (make-array 0 :element-type 'gl-id :initial-element +null-gl-id+
               :adjustable t :fill-pointer 0)
   :type (array gl-id (*)))
  (array-of-transform-feedback-bindings-buffer-ids
   (make-array 0 :element-type 'gl-id :initial-element +null-gl-id+
               :adjustable t :fill-pointer 0)
   :type (array gl-id (*)))

  (array-of-bound-samplers
   (make-array 0 :element-type '(or null sampler) :initial-element nil)
   :type (simple-array (or null sampler) (*)))

  (array-of-textures
   (make-array 0 :element-type 'texture :initial-element +null-texture+
               :adjustable t :fill-pointer 0)
   :type (array texture (*)))
  (depth-func :unknown :type (or symbol function))
  (depth-mask nil :type boolean)
  (color-masks (make-array 0 :element-type '(simple-array boolean (4)))
               :type (simple-array (simple-array boolean (4)) (*)))
  (depth-range (v! 0 1) :type vec2)
  (depth-clamp nil :type boolean)
  (cull-face :unknown :type (or symbol function))
  (front-face :unknown :type symbol)
  (clear-color (v! 0 0 0 0) :type vec4))

(defmethod print-object ((context cepl-context) stream)
  (format stream "#<CEPL-CONTEXT ~a>" (slot-value context 'bound-thread)))

(defmacro %with-cepl-context-slots (slots context &body body)
  (let ((context-slots
         '(id gl-context requested-gl-version bound-thread current-program
           uninitialized-resources shared surfaces current-surface
           current-tfs vao-binding-id current-viewport default-viewport
           current-scissor-viewports default-framebuffer read-fbo-binding
           draw-fbo-binding fbos array-of-bound-gpu-buffers
           array-of-gpu-buffers array-of-ubo-bindings-buffer-ids
           current-blend-params array-of-transform-feedback-bindings-buffer-ids
           array-of-bound-samplers array-of-textures
           depth-func color-masks depth-mask depth-range depth-clamp cull-face
           front-face current-stencil-params-front current-stencil-params-back
           current-stencil-mask-front current-stencil-mask-back
           clear-color gl-version-float)))
    (assert (every (lambda (x) (member x context-slots :test #'string=)) slots))
    (let ((slots (remove-duplicates slots))
          (accessors (loop :for slot :in slots :collect
                        (symb-package :cepl.context '%cepl-context- slot)))
          (ctx (gensym "CONTEXT")))
      `(let ((,ctx ,context))
         (declare (ignorable ,ctx)
                  (inline ,@accessors))
         (symbol-macrolet
             ,(loop :for slot :in slots :for acc :in accessors :collect
                 `(,slot (,acc ,ctx)))
           ,@body)))))


(defn-inline context-id ((context cepl-context)) context-id
  (%cepl-context-id context))

(defn surfaces ((cepl-context cepl-context)) list
  (%cepl-context-surfaces cepl-context))

(defn current-surface ((cepl-context cepl-context)) t
  (%cepl-context-current-surface cepl-context))

;;----------------------------------------------------------------------

#+sbcl
(declaim (sb-ext:freeze-type cepl-context))

;;----------------------------------------------------------------------

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

(defclass cepl-context-shared ()
  ((members :initform nil))
  ;; Things that arent shared between contexts
  ;;
  ;; FBOs
  ;; VAOs
  ;; Transform Feedback Objects
  ;; Program Pipeline Objects (from seperable stages gl>=4.5)
  )

(defstruct (cepl-context (:constructor %make-cepl-context)
                         (:conc-name %cepl-context-))
  (gl-context nil :type (or null gl-context))
  (gl-version nil :type t)
  (gl-thread nil :type (or null bt:thread))
  (uninitialized-resources nil :type list)
  (shared
   (error "Context must be initialized via #'make-context")
   :type cepl-context-shared)
  (surfaces
   (error "Context must be initialized via #'make-context")
   :type list)
  (current-surface nil :type t)
  (vao-binding-id +unknown-gl-id+ :type vao-id)
  (current-viewport nil :type (or null viewport))
  (default-viewport nil :type (or null viewport))
  (default-framebuffer nil :type (or null fbo))
  (read-fbo-binding-id +unknown-gl-id+ :type gl-id)
  (draw-fbo-binding-id +unknown-gl-id+ :type gl-id)
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
  (map-of-pipeline-names-to-gl-ids
   (make-hash-table :test #'eq)
   :type hash-table)
  (depth-func :unknown :type (or symbol function))
  (depth-mask :unknown :type (or symbol function))
  (depth-range (v! 0 1) :type vec2)
  (depth-clamp :unknown :type (or symbol function))
  (cull-face :unknown :type (or symbol function))
  (front-face :unknown :type (or symbol function))
  (clear-color (v! 0 0 0 0) :type vec4))

(defmacro %with-cepl-context-slots (slots context &body body)
  (let ((context-slots
         '(gl-context gl-version gl-thread uninitialized-resources shared
           surfaces current-surface vao-binding-id current-viewport
           default-viewport default-framebuffer read-fbo-binding-id
           draw-fbo-binding-id fbos array-of-gpu-buffers
           array-of-bound-gpu-buffers
           array-of-ubo-bindings-buffer-ids
           array-of-transform-feedback-bindings-buffer-ids
           array-of-bound-samplers array-of-textures
           map-of-pipeline-names-to-gl-ids depth-func
           depth-mask depth-range depth-clamp cull-face front-face
           clear-color)))
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


(defn surfaces ((cepl-context cepl-context)) list
  (%cepl-context-surfaces cepl-context))

(defn current-surface ((cepl-context cepl-context)) t
  (%cepl-context-current-surface cepl-context))

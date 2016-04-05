(defvar tex (load-texture "thing1.png"))
(defvar tex2 (load-texture "thing2.png"))

(defvar f (make-fbo :c :d))

(setf (attachment c 0) tex)
(setf (attachment c 0) nil)
(setf (attachment c 0) tex2)

(setf (attachment-blending c 0) (make-blending-params :wrap :clamp-to-edge))
(setf (attachment-blending c 0) nil)

:fbo
:fbo-p
:fbo-blending-params
:attachment
:attachment-p
:attachment-viewport
:attachment-gpu-array
:per-attachment-blending-available-p
:attachment
:make-fbo-from-id
:make-fbo
:check-framebuffer-status
:with-fbo-bound
:clear
:clear-fbo
:clear-attachment

(defstruct (fbo (:constructor %%make-fbo)
                (:conc-name %fbo-))
  (id -1 :type fixnum)
  ;;
  (color-arrays (error "") :type (array (or null gpu-array-t) *))
  (color-blending (error "") :type (array (or null blending-params) *))
  (depth-array nil :type (or null gpu-array-t))
  (depth-blending nil :type (or null blending-params))
  ;;
  (draw-buffer-map (error ""))
  (clear-mask (cffi:foreign-bitfield-value
               '%gl::ClearBufferMask '(:color-buffer-bit))
              :type fixnum)
  (is-default nil :type boolean)
  (blending-params (make-blending-params :mode-rgb :func-add
					 :mode-alpha :func-add
					 :source-rgb :one
					 :source-alpha :one
					 :destination-rgb :zero
					 :destination-alpha :zero)
		   :type blending-params))


;; OLD ;;
(defstruct (attachment (:constructor %make-attachment)
                       (:conc-name %attachment-))
  (fbo nil :type (or null fbo))
  (gpu-array nil :type (or null gpu-array-t))
  (owns-gpu-array nil :type boolean)
  (blending-enabled nil :type boolean)
  (override-blending nil :type boolean)
  (blending-params (cepl.blending:make-blending-params
		    :mode-rgb :func-add
		    :mode-alpha :func-add
		    :source-rgb :one
		    :source-alpha :one
		    :destination-rgb :zero
		    :destination-alpha :zero) :type blending-params))

;; need to delay:
;;
;; - gl id
;; - update-clear-mask
;; - update-draw-buffer-map
;; - make draw-buffer map

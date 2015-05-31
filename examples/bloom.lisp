(in-package :cepl)
(defvar cols nil)
(defvar *loop* 0)

(defun-g vert ((quad g-pt))
  (values (v! (pos quad) 1)
          (tex quad)))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun-g frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defpipeline blit () (g-> #'vert #'frag))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun-g qkern ((tc :vec2) &uniform (tex :sampler-2d) (offset :vec2))
  (+ (* (texture tex (- tc offset)) 0.3125)
     (* (texture tex tc) 0.375)
     (* (texture tex (+ tc offset)) 0.3125)))

(defpipeline qsmood () (g-> #'vert #'qkern))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun-g fourtex ((tc :vec2) &uniform (t0 :sampler-2d) (t1 :sampler-2d)
                  (t2 :sampler-2d) (t3 :sampler-2d) (scale-effect :float))
  (let ((tc (* tc (v! 1 -1))))
    (+ (* (texture t0 tc) 1)
       (* (texture t1 tc) scale-effect)
       (* (texture t2 tc) scale-effect)
       (* (texture t3 tc) scale-effect))))

(defpipeline combine () (g-> #'vert #'fourtex))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defpipeline bloom (stream &uniform tx)
    (g-> (c0 (blit stream :tex tx))
         (c1 (blit stream :tex tx))
         (c2 (blit stream :tex tx))
         (c3 (blit stream :tex tx))
         (h0 (qsmood stream :tex (attachment c0 0) :offset (v! (/ 1.2 512) 0)))
         (h1 (qsmood stream :tex (attachment c1 0) :offset (v! (/ 1.2 256) 0)))
         (h2 (qsmood stream :tex (attachment c2 0) :offset (v! (/ 1.2 128) 0)))
         (h3 (qsmood stream :tex (attachment c3 0) :offset (v! (/ 1.2 64) 0)))
         (c0 (qsmood stream :tex (attachment h0 0) :offset (v! 0 (/ 1.2 512))))
         (c1 (qsmood stream :tex (attachment h1 0) :offset (v! 0 (/ 1.2 256))))
         (c2 (qsmood stream :tex (attachment h2 0) :offset (v! 0 (/ 1.2 128))))
         (c3 (qsmood stream :tex (attachment h3 0) :offset (v! 0 (/ 1.2 64))))
         (nil (combine stream
                       :t0 (attachment c0 0) :t1 (attachment c1 0)
                       :t2 (attachment c2 0) :t3 (attachment c3 0))))
  :fbos
  (c0 '(:c :dimensions (512 512) :magnify-filter :linear))
  (c1 '(:c :dimensions (256 256) :magnify-filter :linear))
  (c2 '(:c :dimensions (128 128) :magnify-filter :linear))
  (c3 '(:c :dimensions (64 64) :magnify-filter :linear))
  (h0 '(:c :dimensions (512 512)))
  (h1 '(:c :dimensions (256 256)))
  (h2 '(:c :dimensions (128 128)))
  (h3 '(:c :dimensions (64 64))))

(defun step-demo ()
  (incf *loop* 0.008)
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'blit *quad-stream* :tex cols)
  ;;(map-g #'bloom *quad-stream* :tx cols :scale-effect (abs (sin *loop*)))
  (update-display))

;;-------------------------------------------------------
(defparameter *running* nil)

(defun run-loop ()
  (setf *running* t)
  (unless cols
    (setf cols (devil-helper:load-image-to-texture
                (merge-pathnames "ThickCloudsWater/front.png" *examples-dir*))))
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))

(evt:observe (evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))
;;-------------------------------------------------------

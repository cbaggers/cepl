(in-package :cepl.pipelines)

;; typedef  struct {
;;     uint  count;
;;     uint  instanceCount;
;;     uint  first;
;;     uint  baseInstance;
;; } DrawArraysIndirectCommand;

(defstruct-g indirect-command
  (count :uint)
  (instance-count :uint)
  (first :uint)
  (base-instance :uint))

;; (with-instances 1000
;;   )

#+nil
(batch-into
 some-draw-batch-object
 #'thepipelinefunc
 :uniforms 10
 :bar 20
 stream
 )

(defun-g foo-v ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) 0.5)
   (:flat my-id)))

(defun-g foo-f ((uv :vec2)
                &uniform (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g foo-p ()
  (foo-v :vec2)
  (foo-f :vec2))

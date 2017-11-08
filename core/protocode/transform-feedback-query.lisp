(in-package :cepl.examples)

(defvar *vertex-stream* nil)
(defvar *array* nil)
(defvar *feedback-array* nil)
(defvar *tfs* nil)
(defvar *query* nil)
(defvar *prim-count* nil)

(defun reset ()
  (unless *array*
    (setf *array* (make-gpu-array (list (v!  0.0   0.7  0.0  1.0)
                                        (v! -0.7  -0.7  0.0  1.0)
                                        (v!  0.7  -0.7  0.0  1.0))
                                  :element-type :vec4
                                  :dimensions 3)))
  (unless *feedback-array*
    (setf *feedback-array*
          (make-gpu-array nil :element-type :vec3 :dimensions 10)))
  (unless *tfs*
    (setf *tfs*
          (cepl.transform-feedback::make-transform-feedback-stream *feedback-array*)))
  (unless *vertex-stream*
    (setf *vertex-stream* (make-buffer-stream *array*)))

  (unless *query*
    (setf *query* (first (gl:gen-queries 1)))
    (cffi:with-foreign-object (cbits :int)
      (%gl:get-query-iv :transform-feedback-primitives-written
                        :query-counter-bits
                        cbits)
      (let ((bits (cffi:mem-aref cbits :int)))
        (print (list :bits bits))
        (assert (>= bits 1)))))
  nil)

(defun-g mtri-vert ((position :vec4))
  (values position
          (:feedback (v! 0.3 0 1))))

(defun-g mtri-frag ((col :vec3))
  (v! col 0))

(defpipeline-g prog-1 ()
  (mtri-vert :vec4)
  (mtri-frag :vec3))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (cepl.transform-feedback::with-transform-feedback (*tfs*)
    (gl:begin-query :transform-feedback-primitives-written *query*)
    (map-g #'prog-1 *vertex-stream*))
  (gl:end-query :transform-feedback-primitives-written)

  (cffi:with-foreign-object (c-prim-count '%gl:uint)
    (%gl:get-query-object-uiv *query* :query-result c-prim-count)
    (setf *prim-count* (cffi:mem-aref c-prim-count '%gl:uint)))

  (swap))

(let ((running nil))
  (defun run-loop ()
    (setf running t)
    (reset)
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable (step-demo))))
  (defun stop-loop () (setf running nil)))

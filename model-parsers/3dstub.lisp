(in-package :3dstub)

(defparameter *camera* nil)

(defun initialize ()
  (setf *camera* (make-instance 'camera :pos (v! 0 3 6)))
  (setf *frustrum-scale*
        (cepl-camera:calculate-frustrum-scale 45.0))
  (prog-1 nil :cam-to-clip (make-cam-clip-matrix *frustrum-scale*))
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 2.0)
  t)

;;-------TYPES-------;;

(defclass camera ()
  ((pos :initarg :pos :initform (v! 0 0 -10) :accessor pos)
   (look-direction :initform (v! 0 0 -1) 
                   :accessor look-direction)
   (up-direction :initform (v! 0 1 0) :accessor up-direction)))

(defclass stub-object ()
  ((gpu-arrays :initform nil :initarg :gpu-arrays
               :accessor gpu-arrays)
   (gstreams :initform nil :initarg :gstreams
             :accessor gstreams)
   (position :initform (v! 0 0 0)
             :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0)
             :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1)
          :initarg :scale :accessor scale)
   (pipeline :initform nil :initarg :pipeline
             :accessor pipeline)))

(cgl:defglstruct vc
  (pos :vec3 :accessor pos)
  (color :vec4 :accessor color))

(cgl:defglstruct vcn
  (pos :vec3 :accessor pos)
  (color :vec4 :accessor color)
  (normal :vec3 :accessor normal))

;;-------PARAMETERS-------;;

(defparameter *near* 1.0)
(defparameter *far* 1000.0)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *camera* nil)
(defparameter *light-direction* (v:normalize (v! 0.4 1 0 0)))
(defparameter *light-color* (v! 1.0 1.0 1.0 1.0))
(defparameter *ambient-intensity* 0.2)

;;-------SHADERS-------;;

(cgl:defpipeline prog-1
    ((data vcn) &uniform (dir-to-light :vec3)
     (light-intensity :vec4) (norm-model-to-cam :mat3)
     (cam-to-clip :mat4) (model-to-cam :mat4)
     (ambient-intensity :float))
  (:vertex 
   (setf gl-position (* cam-to-clip
                        (* model-to-cam 
                           (vec4 (pos data) 1.0))))
   (out (interp-color :smooth) 
        (+ (* light-intensity 
              (clamp (dot (normalize
                           (* norm-model-to-cam
                              (normal data)))
                          dir-to-light) 
                     0.0 1.0))
           (* (color data) ambient-intensity))))
  (:fragment (out output-color interp-color))
  (:post-compile (update-view 640 480 *near* *far*)))

;;-------CAMERA-------;;

(defun camera-position (&optional (camera *camera*))
  (pos camera))

(defun (setf camera-position) (vec3 &optional (camera *camera*))
  (setf (pos camera) vec3))

(defun point-camera-at (point &optional (camera *camera*))
  (setf (look-direction camera) (v:normalize (v3:v- point (pos camera))))
  camera)

(defun calculate-cam-look-at-w2c-matrix (&optional (camera *camera*))
  (let* ((look-dir (v3:normalize (look-direction camera)))
         (up-dir (v3:normalize (up-direction camera)))
         (right-dir (v3:normalize (v3:cross look-dir up-dir)))
         (perp-up-dir (v3:cross right-dir look-dir))
         (rot-matrix (m4:rotation-from-matrix3
                      (m3:make-from-rows right-dir
                                         perp-up-dir
                                         (v3:v-1 (v! 0 0 0)
                                                 look-dir))))
         (trans-matrix 
          (m4:translation (v3:v-1 (v! 0 0 0)
                                  (pos camera)))))
    (m4:m* rot-matrix trans-matrix)))

(defun make-cam-clip-matrix (frustrum-scale &optional (near 1.0) (far 45.0))
  (m4:make-matrix4 frustrum-scale 0.0 0.0 0.0
                   0.0 frustrum-scale 0.0 0.0
                   0.0 0.0 (/ (+ far near) (- near far))
                   (/ (* 2.0 far near) (- near far))
                   0.0 0.0 -1.0 0.0))

;;-------DRAWING-------;;

(defun update-view (width height &optional (near *near*) (far *far*))
  (prog-1 nil :cam-to-clip (make-cam-clip-matrix 
                            *frustrum-scale* near far))
  (gl:viewport 0 0 width height))

(defgeneric draw (ob &optional camera)
  (:documentation ""))

(defmethod draw ((ob stub-object) &optional (camera *camera*))
  (let* ((world-to-cam-matrix
          (calculate-cam-look-at-w2c-matrix camera))
         (model-to-cam-matrix 
          (m4:m* world-to-cam-matrix 
                 (reduce #'m4:m* 
                         (list (m4:translation (pos ob))
                               (m4:rotation-from-euler (rot ob))
                               (m4:scale (scale ob))))))
         (normal-to-cam-matrix 
          (m4:to-matrix3 model-to-cam-matrix))
         (cam-light-vec
          (m4:mcol*vec4 world-to-cam-matrix *light-direction*)))
    (let ((pipeline (pipeline ob)))
      (loop :for stream :in (gstreams ob) :do
         (funcall pipeline stream
                  :dir-to-light (v! (v-x cam-light-vec) 
                                    (v-y cam-light-vec)
                                    (v-z cam-light-vec))
                  :light-intensity *light-color*
                  :model-to-cam model-to-cam-matrix 
                  :norm-model-to-cam normal-to-cam-matrix
                  :ambient-intensity *ambient-intensity*))))
  ob)


;;-------MODELS-------

(defun primitive-aiscene (type &key (radius 10) (apex-offset (v! 0 1 0)) 
                                 (apex-radius radius))
  (classimp:import-from-string
   (case type
     (:sphere (format nil "s 0 0 0 ~a" radius))
     (:dodecahedron (format nil "dod 0 0 0 ~a" radius))
     (:hexahedron (format nil "hex 0 0 0 ~a" radius))
     (:octahedron (format nil "oct 0 0 0 ~a" radius))
     (:tetrahedron (format nil "tet 0 0 0 ~a" radius))
     (:cone (format nil "c~%0 0 0 ~a~%~{~a~^ ~} 0" 
                    radius 
                    (loop for i being the elements of apex-offset collect i)))
     (:cylinder (format nil "c~%0 0 0 ~a~%~{~a~^ ~} ~a" 
                        radius
                        (loop for i being the elements of apex-offset collect i)
                        apex-radius))
     (t (error "3DStub: primitive: Unrecognised primitive type"))) "nff"
     :processing-flags '(:ai-process-gen-normals)))

;; [TODO] gl-arrays need to be able to handle vectors as population targets
(defun make-primitive (type &key (radius 10) (apex-offset (v! 0 1 0)) 
                              (apex-radius radius) (color (v! 1 0 1 0)))
  (let* ((mesh (aref (ai:meshes (primitive-aiscene type :radius radius
                                                   :apex-offset apex-offset
                                                   :apex-radius apex-radius)) 0))
         (faces (loop for i being the elements of (ai:faces mesh)
                   append (loop for j being the elements of i collect j)))
         (data (loop :for v :in (coerce (ai:vertices mesh) 'list)
                  :for n :in (coerce (ai:normals mesh) 'list)
                  :for c :in (if (eql (length (ai:colors mesh)) 0)
                                 (loop :for i :below 
                                    (length (ai:vertices mesh))
                                    :collect color)
                                 (coerce (ai:colors mesh) 'list))
                  collect (list v c n))))
    (let* ((array (cgl:make-gpu-array data :element-type 'vcn
                                      :dimensions (length data)))
           (index (cgl:make-gpu-array 
                   faces :element-type :unsigned-short
                   :dimensions (length faces)))
           (stream (cgl:make-gpu-stream-from-gpu-arrays 
                    array :indicies-array index)))
      (make-instance 'stub-object :pipeline #'prog-1 
                     :gpu-arrays (list array index) 
                     :gstreams (list stream)))))


(defun load-model (filepath &optional (flags nil) (color (v! 1 0 1 0)))
  (let* ((mesh (aref (ai:meshes (ai:import-into-lisp 
                                 filepath :processing-flags flags)) 0))
         (faces (let ((facesp (> (length (ai:faces mesh)) 0)))
                  (when facesp
                    (let ((c-len (length (aref (ai:faces mesh) 0))))
                      (cond ((eql c-len 3) 
                             (loop for i being the elements of (ai:faces mesh)
                                :append (coerce i 'list))) 
                            ((eql c-len 4) 
                             (loop for i being the elements of (ai:faces mesh)
                                :append (list (aref i 0) (aref i 1) (aref i 2)
                                              (aref i 0) (aref i 2) (aref i 3))))
                            (t (error "3DStub: length of faces must be 3 or 4")))))))
         
         (data (loop :for v :in (coerce (ai:vertices mesh) 'list)
                  :for n :in (coerce (ai:normals mesh) 'list)
                  :for c :in (loop :for i :below (length (ai:vertices mesh))
                                :collect color)
                  collect (list v c n))))
    (let* ((array (cgl:make-gpu-array data :element-type 'vcn
                                      :dimensions (length data)))
           (index (when faces 
                    (cgl:make-gpu-array faces :element-type :unsigned-short
                                        :dimensions (length faces))))
           (stream (cgl:make-gpu-stream-from-gpu-arrays 
                    array :indicies-array index)))
      (make-instance 'stub-object :pipeline #'prog-1 
                     :gpu-arrays (list array index) 
                     :gstreams (list stream)))))

;; (defun test-load-thing (path)
;;   (let* ((data (utils:safe-read-from-string
;;                 (utils:file-to-string path)))
;;          (verts (loop for vert in (first data)
;;                    collect (list (v:* (v:swizzle (first vert))
;;                                       (v! 1 1 1)) 
;;                                  (v:swizzle (second vert))
;;                                  (v:swizzle (third vert))))))
;;     (let* ((garray (cgl:make-gpu-array 
;;                     verts :element-type 'vcn))
;;            (index-array (cgl:make-gpu-array 
;;                          (second data)
;;                          :element-type :unsigned-short
;;                          :index-array t))
;;            (stream (cgl:make-gpu-stream-from-gpu-arrays
;;                     garray :indicies-array index-array
;;                     :length (length (second data)))))
;;       (make-instance 'stub-object :pipeline #'prog-1 
;;                      :gpu-arrays (list garray index-array)
;;                      :gstreams (list stream)))))

(in-package :3dstub)

(defclass stub-object ()
  ((gpu-array :initform nil)
   (stream :initform nil)
   (position :initform (base-vectors:v! 0 0 0) :initarg :pos :accessor pos)
   (rotation :initform (base-vectors:v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (base-vectors:v! 1 1 1) :initarg :scale :accessor scale)))

(defun load-primitive (path)
  (ai:import-into-lisp path :processing-flags '(:ai-process-gen-normals)))

(defun primitive-scene (type &key (radius 10) (apex-offset (base-vectors:v! 0 1 0)) 
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

(defun primitive (type &key (radius 10) (apex-offset (base-vectors:v! 0 1 0)) 
                         (apex-radius radius))
  (let* ((mesh (aref (ai:meshes (primitive-scene type :radius radius
                                                 :apex-offset apex-offset
                                                 :apex-radius apex-radius)) 0)))
    (values (list (ai:vertices mesh) (ai:normals mesh) (ai:faces mesh))
            mesh)))


(in-package :meshes)

(defclass region () ())

(defclass linear-region (region) ((points :initarg :points :reader points)))
(defclass implicit-region (region) ((func :initarg :func :reader func)))

(defclass mesh () ((vertices :initarg :vertices :reader vertices)
                   (indicies :initarg :index :reader indicies)
                   (primitive-type :initarg :primitive-type :reader primitive-type)))

(defmethod vert-layout ((mesh mesh)) (element-type (vertices mesh)))

(defun transform-mesh (mesh &key (translation (v! 0 0 0))
                              (rotation (v! 0 0 0))
                              (linear-scale 1.0)
                              (rotation-type :euler))
  ;;{TODO} need to add cpu side caching to mesh, is it object that lives on gpu?
  (labels ((transform-normal (n)
             (cond
               ((eq rotation-type :euler)
                (let ((m (m3:rotation-from-euler rotation)))
                  (m3:m*vec m n)))
               ((or (eq rotation-type :mat3)
                    (eq rotation-type :matrix)
                    (eq rotation-type :matrix3))
                (m3:m*vec rotation n))
               ((or (eq rotation-type :quat)
                    (eq rotation-type :quaternion))
                (q:rotate n rotation)))))
    (let ((verts (pull-g (vertices mesh)))
          (has-normals (member (vert-layout mesh)
                               '(:g-pn :g-pnc :g-pnt :g-pntc))))
      (push-g
       (loop :for v :in verts :collect
         (if has-normals
             (append (list (v3:v* (v3:v+1 (first v) translation)
                                  linear-scale)
                           (transform-normal (second v)))
                     (subseq v 2))
             (cons (v3:v* (v3:v+1 (first v) translation) linear-scale)
                   (subseq v 1))))
       (vertices mesh))
      mesh)))

(defun transform-mesh-with-matrix (mesh matrix &optional normal-matrix)
  ;;{TODO} need to add cpu side caching to mesh, is it object that lives on gpu?
  (let ((verts (pull-g (vertices mesh)))
        (has-normals (member (vert-layout mesh)
                             '(:g-pn :g-pnc :g-pnt :g-pntc))))
      (push-g
       (loop :for v :in verts :collect
         (if has-normals
             (append (list (m3:m*vec matrix (first v))
                           (m3:m*vec (or normal-matrix matrix)
                                     (second v)))
                     (subseq v 2))
             (cons (m3:m*vec matrix (first v)) (subseq v 1))))
       verts)
      mesh))

(defmethod polygonize ((region linear-region) primitive-type
                       &key index-data normals texture-coords)
  (let* ((points (points region))
         (element-type (model-parsers:calc-type
                        points normals texture-coords)))
    (make-instance
     'mesh
     :vertices (make-gpu-array (apply #'map 'list #'list
                                      (remove nil (list points
                                                        normals
                                                        texture-coords)))
                               :element-type element-type)
     :vert-layout :element-type
     :index (when index-data
              (make-gpu-array (flatten-index index-data)
                              :element-type :ushort))
     :primitive-type primitive-type)))


(defun flatten-index (sequence)
  (if (typep (elt sequence 0) 'sequence)
      (make-array (* (length sequence) (length (elt sequence 0)))
                  :element-type 'fixnum
                  :initial-contents
                  (typecase sequence
                    (array (loop :for x :across sequence :append (coerce x 'list)))
                    (list (loop :for x :in sequence :append (coerce x 'list)))))
      (make-array (length sequence)
                  :element-type 'fixnum
                  :initial-contents sequence)))

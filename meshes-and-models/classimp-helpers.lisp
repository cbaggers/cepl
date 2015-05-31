(in-package :model-parsers)
(named-readtables:in-readtable fn_:fn_lambda)

(defun meshes->lists (scene)
  (map 'list #'mesh->lists (classimp:meshes scene)))

(defun mesh->lists (mesh)
  (let* ((v (classimp:vertices mesh))
         (n (classimp:normals mesh))
         (tcs (classimp:texture-coords mesh))
         (tc (when (> (length tcs) 0) (aref tcs 0)))
         (f (classimp:faces mesh))
         (n-len (length n))
         (tc-len (length tc))
         (set (remove nil `(,v ,@(when n-len (list n))
                               ,@(when tc-len (list tc))))))
    `((:data ,(calc-type v n tc)
             ,(apply #'map 'list #'list set))
      (:indices ,(loop :for % :across f :append (coerce % 'list))))))

(defun calc-type (v n tc)
  (apply #'utils:symb-package :cgl
         (remove nil (cons :g- (list (and v :p)
                                     (and (> (length n) 0) :n)
                                     (and (> (length tc) 0) :t))))))
(defun mesh-list->gpu (mesh-list)
  (list
   (destructuring-bind (_ type data) (assoc :data mesh-list)
     (declare (ignore _))
     (cgl:make-gpu-array data :element-type type))
   (destructuring-bind (_ data) (assoc :indices mesh-list)
     (declare (ignore _))
     (cgl:make-gpu-array data :element-type :ushort))))

(defun scene-meshes->gpu (scene)
  (mapcar #'mesh-list->gpu (meshes->lists scene)))

(defun mesh->gpu (mesh)
  (mesh-list->gpu (mesh->lists mesh)))

(defun load-file (file-path)
  (scene-meshes->gpu (classimp:import-into-lisp file-path)))

(in-package :cepl)

;; This generates a box of given dimensions, uploads the data to the 
;; gpu and returns a list with: 1- A gpu-stream which can be rendered
;;                              2- the gpu-array holding the vertices
;;                              3- the gpu-array holding the indicies

(defun make-box (width height depth)
  (destructuring-bind (vert-data index-data)
      (primitives:box-data width height depth)
    (let ((verts (make-gpu-array vert-data :element-type 'p3-n3-t2))
          (index (make-gpu-array index-data :element-type :ubyte)))
      (list (make-gpu-stream-from-gpu-arrays verts :index-array index)
            verts index))))

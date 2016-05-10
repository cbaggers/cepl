(in-package :cepl)


(c-v! ptr 0 1 2 3)
(c-m! ptr 0 1 2 3)

(v!ptr 0 1 2 3)

;;----------------------------------------------------------------------
;; the issue

(defun thing (size-x size-y)
  (let* ((quad-verts (list (v! -1.0 -1.0) (v! 1.0 -1.0)
                           (v! 1.0 1.0) (v! -1.0 1.0)))
         (data (make-array (* 4 size-x size-y)
                           :element-type 'rtg-math.types:vec4
                           :initial-element (v! 0 0 0 0)))
         (verts (loop :for y :below size-y :do
                   (loop :for x :below size-x :do
                      (loop :for qv :in quad-verts :for i :from 0 :do
                         (let ((index (+ i (* x 4) (* y 4 size-x))))
                           (setf (aref data index)
                                 (v! (v:x qv) (v:y qv) x y)))))
                   :finally (return (make-gpu-array data :element-type :vec4))))
         (indices (with-c-array
                      (x (make-c-array nil :dimensions (* 6 size-x size-y)
                                       :element-type :uint))
                    (let ((index #(3 0 1 3 1 2))
                          (offset 0))
                      (loop :for i :below (* size-x size-y 6) :by 6 :do
                         (loop :for e :across index :for j :from 0 :do
                            (setf (cffi:mem-aref (c-array-pointer x) :uint
                                                 (+ i j))
                                  (+ e offset)))
                         (incf offset 4)))
                    (make-gpu-array x))))
    (make-buffer-stream verts :index-array indices :retain-arrays t)))

;;----------------------------------------------------------------------
;; an approach

(defun map-c (function c-array)
  (let ((result (make-c-array nil :dimensions (c-array-rank c-array)
                              :element-type (c-array-element-type c-array))))
    (case= (c-array-rank c-array)
      (1 (%map-c-into-1d result function c-array))
      (2 (%map-c-into-2d result function c-array))
      (3 (%map-c-into-3d result function c-array))
      (otherwise (%map-c-into-nd result function c-array)))))

(defun map-c-into (destination-c-array function source-c-array)
  (case= (c-array-rank c-array)
    (1 (%map-c-into-1d destination-c-array function source-c-array))
    (2 (%map-c-into-2d destination-c-array function source-c-array))
    (3 (%map-c-into-3d destination-c-array function source-c-array))
    (otherwise (%map-c-into-nd destination-c-array function source-c-array))))

(defun %map-c-into-1d (dest func source)
  (loop :for i :below (first (c-array-))
     ))

(defun across-c (function c-array)
  "run function passing in the array and the current index/indices"
  (case= (c-array-rank c-array)
    (1 (%across-c-1d function c-array))
    (2 (%across-c-2d function c-array))
    (3 (%across-c-3d function c-array))
    (otherwise (%across-c-nd function c-array))))

(across-c λ(incf (aref _ _1 _2)) x)

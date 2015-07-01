(in-package :projection)

(defun perspective (frame-width frame-height near far fov)
  (let* ((aspect-ratio (/ frame-width frame-height))
         (near near )
         (far far )
         (fov fov )
         (range (tan (/ fov 2.0)))
         (left (- (* range aspect-ratio)))
         (right (* range aspect-ratio))
         (bottom (- range))
         (top range))
    (matrix4:make-matrix4
     (/ (* near 2) (- right left)) 0.0 0.0 0.0
     0.0 (/ (* near 2) (- top bottom)) 0.0 0.0
     0.0 0.0 (- (/ (+ far near) (- far near))) -1.0
     0.0 0.0 (/ (* 2.0 far near) (- near far)) 0.0)))

(defun orthographic (frame-width frame-height near far)
  (let ((left (- (/ frame-width 2.0)))
        (right (/ frame-width 2.0))
        (top (/ frame-height 2.0))
        (bottom (- (/ frame-height 2.0)))
        (near near )
        (far far ))
    (matrix4:make-matrix4
     (/ 2 (- right left)) 0.0 0.0 (- (/ (+ right left) (- left right)))
     0.0 (/ 2 (- top bottom)) 0.0 (- (/ (+ top bottom) (- bottom top)))
     0.0 0.0 (- (/ (- far near))) (- (/ (+ far near) (- far near)))
     0.0 0.0 0.0 1.0)))

(in-package :varjo)

(def-v-type-class v-space (v-type) nil)

(def-v-type-class pos4 (v-type) nil)



(def-v-type-class clip-space (v-space) nil)
(def-v-type-class pos4-clip-space (pos4) nil)
(v-defun p! (x y) "~a" (v-vec4 clip-space) pos4-clip-space)
(v-defun :%+ (a b) "(~a + ~a)" (pos4-clip-space pos4-clip-space) 0)

(def-v-type-class world-space (v-space) nil)
(def-v-type-class pos4-world-space (pos4) nil)
(v-defun p! (x y) "~a" (v-vec4 world-space) pos4-world-space)
(v-defun :%+ (a b) "(~a + ~a)" (pos4-world-space pos4-world-space) 0)

(v-defun make-clip-space () "<make clip space>" () clip-space)
(v-defun make-world-space () "<make world space>" () world-space)



(defshader test ()
  (let* ((clip-space (make-clip-space))
	 (world-space (make-world-space))
	 (b (v! 0 0 0 0))
	 (c (p! b clip-space))
	 (d (p! b world-space)))
    (+ c c)
    b))

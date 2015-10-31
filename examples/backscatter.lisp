(in-package :cepl)

;; 'Backscatter' is nice little raycasting demo made by a good friend.
;; It also happens to be very easy to read so I'm going to be copying
;; at least some of it here as a learning exercise.

(defstruct-g material ()
  (additive-color :vec3 :accessor additive-color)
  (diffuse-color :vec3 :accessor diffuse-color)
  (specular-color :vec3 :accessor specular-color)
  (specular-exponent :float :accessor specular-exponent)
  (background-ammount :float :accessor background-ammount))

(defstruct-g scene-result ()
  (d :float)
  (material material))

(defun-g mix-materials ((mat-a material) (mat-b material) (amount :float))
  (make-material
   (mix (additive-color mat-a) (additive-color mat-b) amount)
   (mix (diffuse-color mat-a) (diffuse-color mat-b) amount)
   (mix (specular-color mat-a) (specular-color mat-b) amount)
   (mix (specular-exponent mat-a) (specular-exponent mat-b) amount)
   (mix (background-ammount mat-a) (background-ammount mat-b) amount)))

(defun-g rotate-x ((vec :vec3) (angle :float))
  (let ((c (cos angle))
	(s (sin angle)))
    (v! (x vec)
	(- (* (y vec) c)
	   (* (z vec) s))
	(+ (* (z vec) c)
	   (* (y vec) s)))))

(defun-g rotate-y ((vec :vec3) (angle :float))
  (let ((c (cos angle))
	(s (sin angle)))
    (v! (- (* (x vec) c)
	   (* (z vec) s))
	(y vec)
	(+ (* (z vec) c)
	   (* (x vec) s)))))

(defun-g rotate-z ((vec :vec3) (angle :float))
  (let ((c (cos angle))
	(s (sin angle)))
    (v! (- (* (x vec) c)
	   (* (y vec) s))
	(+ (* (y vec) c)
	   (* (x vec) s))
	(z vec))))

(defun-g hash ((n :float))
  (fract (* (sin n) 753.5453123s0)))

(defun-g noise ((x :vec3))
  (let* ((p (floor x))
	 (f (fract x))
	 (f (* f f (- (v! 3 3 3) (* 2s0 f))))
	 (n (+ (x p)
	       (* (y p) 157s0)
	       (* 113s0 (z p)))))
    (mix (mix (mix (hash (+ n   0.0))  (hash (+ n   1.0)) (x f))
	      (mix (hash (+ n 157.0))  (hash (+ n 158.0)) (x f)) (y f))
	 (mix (mix (hash (+ n 113.0))  (hash (+ n 114.0)) (x f))
	      (mix (hash (+ n 270.0))  (hash (+ n 271.0)) (x f)) (y f)) (z f))))

(defun-g fbm ((x :vec3))
  (+ (noise x)
     (/ (noise (* x 2s0)) 2s0)
     (/ (noise (* x 4s0)) 4s0)
     (/ (noise (* x 8s0)) 8s0)
     (/ (noise (* x 16s0)) 16s0)))

(defun-g fbm-2 ((x :vec3))
  (+ (noise x)
     (/ (noise (* x 4s0)) 4s0)
     (/ (noise (* x 16s0)) 16s0)))

(defun-g fbm-3 ((x :vec3))
  (+ (noise x)
     (let ((x (rotate-y x 0.833)))
       (/ (noise (* x 4s0)) 4s0))))

(defmacro-g saturate (x)
  `(clamp ,x 0s0 1s0))

(defun-g sphere ((p :vec3) (pos :vec3) (radius :float))
  (- (length (- p pos)) radius))

;; (defun-g rep-cylinders ((p :vec3) (c :vec2))
;;   (let ((derp (mod (s~ p :xz) x))
;; 	)))

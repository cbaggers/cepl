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
  (d :float :accessor s-r-d)
  (material material :accessor s-r-material))

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

(defun-g rep-cylinders ((p :vec3) (c :vec2))
  (let* ((derp (mod (s~ p :xz) c))
	 (q (- derp (* c 0.5))))
    (- (length q) 0.34)))

(defun-g f ((p :vec3) (global-time :float))
  (let* ((diag (+ (x p) (y p) (z p)))
	 (time-vec3 (v! global-time global-time global-time))
	 (material (make-material
		    (v! 0 0 0)
		    (* (v! 0.08 0.08 0.08) (v! 1 0.9 0.8))
		    (v! 2 2 2)
		    36.0
		    0.42))
	 (s (+ (sphere p (v! 0 0 0) 5.0)
	       (noise (+ (* p 0.4) time-vec3))))
	 (c-noise (* (noise (+ (* p 0.1) time-vec3)) 8))
	 (cylinders (rep-cylinders (+ p (v! c-noise c-noise c-noise))
				   (v! 10 10)))
	 (ground (+ (y p)
		    12.0
		    (* (+ (sin (* (x p) .4))
			  (cos (* (z p) .4)))
		       .4))))
    (make-scene-result
     (min (min s cylinders) ground)
     material)))

(defun-g tf ((p :vec3) (global-time :float))
  (let* ((diag (* (+ (x p) (y p) (z p)) 2.2))
	 (material-mix (saturate (pow (* (+ (* (sin diag) .5)
					    .5)
					 1.2)
				      40.0)))
	 (material (make-material
		    (v! 0 0 0)
		    (v! 0.08 0.08 0.08)
		    (v! 1.01 1.01 1.01)
		    6.0
		    0.0))
	 (original (f p global-time)))
    (make-scene-result (+ (s-r-d original)
			  (* (+ (fbm (* p 3.0)) 1.4)
			     material-mix
			     .03))
		       (mix-materials (s-r-material original)
				      material
				      material-mix))))

(defun-g light-contribution ((eye-pos :vec3) (eye-dir :vec3) (normal :vec3)
			     (light-pos :vec3) (light-color :vec3)
			     (material material) (occlusion-term :float))
  (let* ((l (normalize (- light-pos eye-pos)))
	 (diffuse (max (- (* (diffuse-color material)
			     (max (dot normal l) 0.0))
			  (v! occlusion-term occlusion-term occlusion-term))
		       (v! 0 0 0)))
	 (ref-vec (reflect l normal))
	 (specular (* (specular-color material)
		      (pow (max (dot ref-vec eye-dir) 0.0)
			   (specular-exponent material)))))
    (* (+ diffuse specular) light-color)))

(defun-g outdoor-background ((eye-pos :vec3) (eye-dir :vec3)
			     (light-1-pos :vec3) (light-2-pos :vec3)
			     (light-1-color :vec3) (light-2-color :vec3)
			     (global-time :float))
  (let ((sky (+ (v! 0.08 0.08 0.08)
		(* (pow (max (dot eye-dir (normalize (- light-1-pos eye-pos)))
			     0.0)
			27.0)
		   light-1-color
		   .4)
		(* (pow (max (dot eye-dir (normalize (- light-2-pos eye-pos)))
			     0.0)
			27.0)
		   light-2-color
		   .4)
		(* (mix (v! .8 .9 1.0) (v! 1.0 .9 .8)
			(pow (- 1.0 (abs (y eye-dir))) 2.1))
		   .8)))
	(clouds-mix (saturate (pow (- (fbm (v! (* (s~ eye-dir :xz) 7.0)
					       (* global-time .1)))
				      .2)
				   6.0)))
	(clouds (mix (v! .8 .8 .8) (v! 1 1 1)
		     (fbm (* eye-dir 24.0)))))
    (mix sky clouds clouds-mix)))

(defun-g background ((eye-pos :vec3) (eye-dir :vec3)
		     (light-1-pos :vec3) (light-2-pos :vec3)
		     (light-1-color :vec3) (light-2-color :vec3)
		     (global-time :float))
  (outdoor-background eye-pos eye-dir light-1-pos light-2-pos
		      light-1-color light-2-color
		      global-time))

(defun-g main-image (&uniform (resolution :vec2) (global-time :float)
			      (light-1-pos :vec3) (light-2-pos :vec3)
			      (light-1-color :vec3) (light-2-color :vec3))
  (let* ((frag-coord (s~ gl-frag-coord :xy))
	 (pixel-pos (/ (- frag-coord (/ (s~ resolution :xy) 2))
		       (y resolution)))
	 (eye-dir (normalize (v! pixel-pos 1)))
	 (norm-mouse (v! 0 0)) ;; (- (/ mouse-xy resolution) .5)
	 (cam-rot-x (+ (sin (* global-time .1))
		       (* (y norm-mouse) 3.141592)))
	 (cam-rot-y (+ (* global-time .5)
		       (* (x norm-mouse) 2.0 3.141592)))
	 (cam-rot-z (* (sin (* global-time .083))
		       .4))
	 (eye-dir (rotate-y (rotate-x (rotate-z eye-dir cam-rot-z)
				      cam-rot-x)
			    cam-rot-y))
	 (eye-pos (rotate-y (rotate-x (v! 0 0 -14) cam-rot-x)
			    cam-rot-y))
	 (color (background eye-pos eye-dir
			    light-1-pos light-2-pos
			    light-1-color light-2-color
			    global-time))

	 (max-dis-travelled 40.0)
	 (dis-travelled 0.0)
	 (max-interations 160))
    (for (i 0) (< i max-interations) (++ i)
	 (let* ((d (s-r-d (f eye-pos global-time))))
	   (%if (<= d .0)
		(break))
	   (let ((d (max d .025)))
	     (incf eye-pos (* eye-dir d))
	     (incf dis-travelled d)
	     (%if (>= dis-travelled max-dis-travelled)
		  (break)))))
    (%if (< dis-travelled max-dis-travelled)
	 (let* ((td (tf eye-pos global-time))
		(normal (normalize
			 (v! (- (s-r-d (tf (+ eye-pos (v! .003 0 0))
					   global-time))
				(s-r-d td))
			     (- (s-r-d (tf (+ eye-pos (v! 0 .003 0))
					   global-time))
				(s-r-d td))
			     (- (s-r-d (tf (+ eye-pos (v! 0 0 .003))
					   global-time))
				(s-r-d td)))))
		(occlusion-term .0))
	   (for (i 0.0) (< i 4.0) (+ i 1.0)
		(incf occlusion-term
		      (/ (max (- (s-r-d (f (+ eye-pos (* normal i .1))
					   global-time)))
			      0.0)
			 (pow 2.0 i))))
	   (setf occlusion-term (* occlusion-term 2.0))
	   (let* ((surface-color
		   (+ (additive-color (s-r-material td))
		      (light-contribution eye-pos eye-dir normal
					  light-1-pos light-1-color
					  (s-r-material td) occlusion-term)
		      (light-contribution eye-pos eye-dir normal
					  light-2-pos light-2-color
					  (s-r-material td) occlusion-term)
		      (* (background eye-pos (reflect eye-dir normal)
				     light-1-pos light-2-pos
				     light-1-color light-2-color
				     global-time)
			 (background-ammount (s-r-material td))))))
	     (setf color (mix color surface-color
			      (- 1.0 (pow (/ dis-travelled
					     max-dis-travelled)
					  2.0)))))))
    (let* ((q (/ (s~ frag-coord :xy) resolution))
	   (color (pow (* color (v! 1 .99 1.06))
		       (v! 1.2 1.2 1.2)))
	   (color ;; Vignette (stolen from iq)
	    (* color
	       (+ 0.4 (* 0.6 (pow (* 16.0 (x q) (y q)
				     (- 1.0 (x q)) (- 1.0 (y q)))
				  0.1)))))
	   (color ;;Cheap "bloom emulation" to better match the actual intro :)
	    (+ color (* (pow (max (- color (v! .2 .2 .2)) 0)
			     (v! 1.4 1.4 1.4))
			.5))))
      (v! color 1))))

(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defvar +some-origin-time+ (get-internal-real-time))

(defun-g ray-vert ((position :vec4))
  (values position (swizzle position :xy)))

(defpipeline raymarcher () (g-> #'ray-vert #'main-image))

(let ((running nil))
  (defun run-loop ()
    (setf *gpu-array* (make-gpu-array (list (v! -1.0  -1.0  0.0  1.0)
                                            (v!  1.0  -1.0  0.0  1.0)
                                            (v!  1.0   1.0  0.0  1.0)
                                            (v!  1.0   1.0  0.0  1.0)
                                            (v! -1.0   1.0  0.0  1.0)
                                            (v! -1.0  -1.0  0.0  1.0))
                                      :element-type :vec4
                                      :dimensions 6))
    (setf *vertex-stream* (make-buffer-stream *gpu-array*))
    (setf running t)
    (loop :while running :do (continuable (draw-step))))
  (defun stop-loop () (setf running nil)))

(evt:def-named-event-node sys-listener (e evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))


(defun draw-step ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (let ((time (- (get-internal-real-time) +some-origin-time+)))
    (map-g #'raymarcher *vertex-stream*
	   :resolution (size (current-viewport))
	   :global-time time
	   :light-1-pos (v! (sin (* time 1.1)) (cos time) 20.0)
	   :light-2-pos (v! (cos (* time .84)) (cos (* time .45)) 20.0)
	   :light-1-color (v! 0.7 .85 1.0)
	   :light-2-color (v! 1.0 .85 .7)))

  (update-display))

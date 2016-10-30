(in-package :cepl.tests)

;;----------------------------------------------------------------------
;; vars

(defvar *loop* 0s0)

;;----------------------------------------------------------------------
;; types

(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defstruct-g test
  (scale :float :accessor scale))

;;----------------------------------------------------------------------
;; macros

(defmacro-g density-normal (object-call position-arg-num)
  (when (not (listp object-call)) (error "object-call form must a list"))
  (let ((p (1+ position-arg-num))
        (oc object-call))
    `(normalize
      (v! (- ,(cepl-utils:replace-nth oc p `(+ ,(nth p oc) (v! 0.01  0.0  0.0)))
             ,(cepl-utils:replace-nth oc p `(- ,(nth p oc) (v! 0.01  0.0  0.0))))
          (- ,(cepl-utils:replace-nth oc p `(+ ,(nth p oc) (v!  0.0 0.01  0.0)))
             ,(cepl-utils:replace-nth oc p `(- ,(nth p oc) (v!  0.0 0.01  0.0))))
          (- ,(cepl-utils:replace-nth oc p `(+ ,(nth p oc) (v!  0.0  0.0 0.01)))
             ,(cepl-utils:replace-nth oc p `(- ,(nth p oc) (v!  0.0  0.0 0.01))))))))

;;----------------------------------------------------------------------
;; funcs

(defun-g b3d-vert ((vert g-pc) &uniform (model->world :mat4) (world->cam :mat4)
		   (cam->clip :mat4))
  (values (* cam->clip
             (* world->cam
                (* model->world
                   (v! (pos vert) 1.0))))
          (:smooth (col vert))))

(defun-g b3d-frag ((interp-color :vec4))
  interp-color)

(defun-g box-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (norm vert)
          (tex vert)))

(defun-g box-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (v! (s~ (texture tex (* tc 1)) :xyz) fac))


(defun-g passthrough-vert ((quad g-pt))
  (values (v! (pos quad) 1) (tex quad)))

(defun-g passthrough-frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defun-g qkern ((tc :vec2) &uniform (tex :sampler-2d) (offset :vec2))
  (+ (* (texture tex (- tc offset)) 0.3125)
     (* (texture tex tc) 0.375)
     (* (texture tex (+ tc offset)) 0.3125)))

(defun-g fourtex ((tc :vec2) &uniform (t0 :sampler-2d) (t1 :sampler-2d)
                  (t2 :sampler-2d) (t3 :sampler-2d) (scale-effect :float))
  (let ((tc (* tc (v! 1 -1))))
    (+ (* (texture t0 tc) 1)
       (* (texture t1 tc) scale-effect)
       (* (texture t2 tc) scale-effect)
       (* (texture t3 tc) scale-effect))))

(defun-g cube-vert ((vert :vec3) &uniform (mod-clip :mat4))
  (values (* mod-clip (v! vert 1))
          vert))

(defun-g cube-frag ((tc :vec3) &uniform (tex :sampler-cube))
  (texture tex tc))

(defun-g pass-through-vert ((vert g-pt))
  (values (v! (pos vert) 1) (tex vert)))

(defun-g pass-through-frag ((tc :vec2) &uniform (board :sampler-2d))
  (let ((c (v:x (texture board tc))))
    (v! c 0 c 0)))

(defun-g the-meat! ((tc :vec2) &uniform (board :sampler-2d))
  (let* ((offset (/ 1.0 1024.0))
	 (score (v:x (+ (texture board (+ tc (v! (- offset) (- offset))))
			(texture board (+ tc (v! (- offset)  0)))
			(texture board (+ tc (v! (- offset)  offset)))
			(texture board (+ tc (v!  0 (- offset))))
			(texture board (+ tc (v!  0  offset)))
			(texture board (+ tc (v!  offset (- offset))))
			(texture board (+ tc (v!  offset  0)))
			(texture board (+ tc (v!  offset  offset))))))
         (current (texture board tc)))
    (cond
      ((or (< score 2) (> score 3)) (v! 0 0 0 0))
      ((and (= score 3) (= (v:x current) 0)) (v! 1 0 0 0))
      (t current))))

(defun-g simple-vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

(def-glsl-stage frag-glsl (("color_in" :vec4) &context :330 :fragment)
  "void main() {
       color_out = color_in;
   }"
  (("color_out" :vec4)))

(defun-g instance-vert ((data g-pnt) &uniform (model-to-cam :mat4)
                        (cam-to-clip :mat4) (offsets :sampler-buffer))
  (values (let ((tpos (texel-fetch offsets gl-instance-id)))
            (+ (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
               tpos
               (v! 0 0 -4 7)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g instance-frag ((model-space-pos :vec3) (vertex-normal :vec3)
			(diffuse-color :vec4) (tex-coord :vec2) &uniform
			(model-space-light-pos :vec3) (light-intensity :vec4)
			(ambient-intensity :vec4) (textur :sampler-2d)
			(norm-map :sampler-2d))
  (let* ((light-dir (normalize (- model-space-light-pos
                                  model-space-pos)))
         (t-norm (- (* (s~ (texture norm-map tex-coord) :xyz) 2)
                    (v! 1 1 1)))
         (cos-ang-incidence
          (clamp (dot (normalize (* (+ vertex-normal t-norm) 0.5)) light-dir)
                 0.0 1.0))
         (t-col (texture textur (* (v! 1 -1) tex-coord))))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(defun-g calc-pos ((v-pos :vec4) (id :int))
  (let ((pos (v! (* (s~ v-pos :xyz) 0.3) 1.0)))
    (+ pos (let ((i (/ (+ (float id)) 2)))
             (v! (sin (+ i *loop*))
                 (cos (* 3 (+ (tan i) *loop*)))
                 0.0 0.0)))))

(defun-g mtri-vert ((position :vec4) &uniform (i :int))
  (calc-pos position i))

(defun-g mtri-frag ()
  (v! (cos *loop*) (sin *loop*) 0.4 1.0))

(defun-g nm-vert ((data g-pnt) &uniform (model-to-cam :mat4) (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g nm-frag ((model-space-pos :vec3) (vertex-normal :vec3)
                  (diffuse-color :vec4) (tex-coord :vec2) &uniform
                  (model-space-light-pos :vec3) (light-intensity :vec4)
                  (ambient-intensity :vec4) (textur :sampler-2d)
                  (norm-map :sampler-2d))
  (let* ((light-dir (normalize (- model-space-light-pos
                                  model-space-pos)))
         (t-norm (- (* (s~ (texture norm-map tex-coord) :xyz) 2)
                    (v! 1 1 1)))
         (cos-ang-incidence
          (clamp (dot (normalize (* (+ vertex-normal t-norm) 0.5)) light-dir)
                 0.0 1.0))
         (t-col (texture textur tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(defun-g sphere ((p :vec3) (r :float))
  (- (length p) r))

(defun-g box ((p :vec3) (b :vec3))
  (let ((d (- (abs p) b)))
    (+ (min (max (x d) (max (y d) (z d))) 0.0)
       (length (max d 0.0)))))

(defun-g thing2 ((p :vec3) (r :float) (l :float))
  (+ (* 0.2 (+ (y p)
               (cos (+ (* (+ (cos l) 8.0)
                          (* 2 (x p))) l))
               (* 3 (sin (* 3 l)))))
     (- (length p) r)))

(defun-g ray-vert ((position :vec4))
  (values position
          (swizzle position :xy)))

(defun-g ray-frag ((posxy :vec2) &uniform (loop :float) (eye-pos :vec3))
  (let* ((eye-pos (v! 0.0 0.0 -5.0))
         (eye-dir (normalize (v! (x posxy) (y posxy) 1.0)))
         (e eye-pos)
         (output (v! 0.0 0.0 0.0)))
    (for (i 0) (< i 20) (++ i)
         (let ((d (thing2 e 1.4 loop)))
           (%if (<= d 0.0)
                (let ((norm (density-normal (thing2 e 1.4 loop) 0)))
                  (setf output (v! (+ 0.3 (* 0.5 (y norm)))
                                   0.0
                                   (+ 0.5 (* 0.2 (mix (y norm) (x norm)
                                                      (sin (* 1.0 loop))))) ))
                  (break)))
           (setf d (max d 0.01))
           (setf e (+ e (* eye-dir d)))))
    (v! output 1.0)))

(defun-g standard-vert ((data g-pnt) &uniform (model-to-cam :mat4)
                        (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g standard-frag
    ((model-space-pos :vec3) (vertex-normal :vec3) (diffuse-color :vec4)
     (tex-coord :vec2)
     &uniform (model-space-light-pos :vec3) (light-intensity :vec4)
     (ambient-intensity :vec4) (textur :sampler-2d))
  (let* ((light-dir (normalize (- model-space-light-pos
                                  model-space-pos)))
         (cos-ang-incidence
          (clamp (dot (normalize vertex-normal) light-dir) 0.0 1.0))
         (t-col (texture textur (v! (x tex-coord) (- (y tex-coord))))))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(defun-g refract-vert ((data g-pnt) &uniform (model-to-cam :mat4)
                       (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
          (tex data)))

(defun-g refract-frag ((tex-coord :vec2) &uniform (textur :sampler-2d)
                       (bird-tex :sampler-2d) (fbo-tex :sampler-2d)
                       (loop :float))
  (let* ((o (v! (mod (* loop 0.05) 1.0)
                (mod (* loop 0.05) 1.0)))
         (ot (* (s~ (texture textur (+ o tex-coord)) :xy) 0.1))
         (a (texture textur tex-coord))
         (b (+ (v! (* (x gl-frag-coord) (/ 1.0 640.0))
                   (* (y gl-frag-coord) (/ 1.0 480.0)))
               (* (s~ a :xy) 0.020)
               ot))
         (c (texture fbo-tex b))
         (r (* (texture bird-tex (* (v! 1 -1) tex-coord)) 0.1)))
    (+ r c)))

(defun-g sampling-vert ((vert g-pt))
  (values (v! (pos vert) 1.0) (tex vert)))

(defun-g sampling-frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defun-g tex-vert ((vert g-pt))
  (values (v! (pos vert) 1)
          (:smooth (tex vert))))

(defun-g tex-frag ((tex-coord :vec2) &uniform (texture :sampler-2d)
                   (count :float) (pos-offset :vec4))
  (let* ((rip-size 0.02) (centre (v! 0.0 0.0)) (damp 0.6)
         (peaks 9.0)
         (dif (- tex-coord centre))
         (dist (dot dif dif))
         (height (/ (+ (cos (+ count (* dist peaks)))
                       (sin (- count (* (y tex-coord) peaks))))
                    2.0))
         (rip-offset (* (* (normalize dif) rip-size) height damp)))
    (+ (texture texture (+ rip-offset tex-coord))
       (v! (* -0.2 height) (* -0.2 height) 0.0 0.0))))

(defun-g tri-vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

(defun-g tri-frag ((color :vec4))
  color)

(defun-g ubo-vert ((vert g-pc) &uniform (hmm test :ubo))
  (values (v! (* (pos vert) (scale hmm)) 1.0)
          (:smooth (col vert))))

(defun-g ubo-frag ((color :vec4))
  color)

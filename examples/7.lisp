;; In this file I'm looking at the main loop and thinking
;; about how I want to do event handling. I want to have the 
;; nuts and bolts of the main loop up front where I can see it
;; to allow quick changes and also just so you can really grasp
;; what is happening. It just feels like the main loop is too 
;; important to be left to magic!
;; The result is pretty damn ugly but it is the first step at 
;; an alternative method.
;; I'm also playing around with the 'diamond square' algorithm
;; for generating terrains.


(setf *random-state* (make-random-state t))

(defparameter *prog-1* nil)
(defparameter *frustrum-scale* nil)
(defparameter *cam-clip-matrix* nil)
(defparameter *shaders* nil)
(defparameter *entities* nil)
(defparameter *camera* nil)


;; Define data formats 
(cgl:defglstruct vert-data 
  (position :type :float :length 3)
  (colour :type :float :length 4))

;; The entities used in this demo
(defstruct entity 
  (stream nil)
  (position (v! 0.0 0.0 -20.0))
  (rotation (v! 0.0 0.0 0.0))
  (scale (v! 1.0 1.0 1.0))
  (left nil)
  (right nil)
  (forward nil)
  (backward nil))

(defstruct camera 
  (position (v! 0.0 0.0 0.0))
  (look-direction (v! 0.0 0.0 -1.0))
  (up-direction (v! 0.0 1.0 0.0)))

(defun point-camera-at (camera point)
  (setf (camera-look-direction camera)
	(v:normalize (v:- point (camera-position camera))))
  camera)

(defun calculate-cam-look-at-w2c-matrix (camera)
  (let* ((look-dir (v:normalize (camera-look-direction camera)))
	 (up-dir (v:normalize (camera-up-direction camera)))
	 (right-dir (v:normalize (v:cross look-dir up-dir)))
	 (perp-up-dir (v:cross right-dir look-dir))
	 (rot-matrix (m4:transpose
		      (m4::rotation-from-matrix3
		       (m3:make-from-rows right-dir
					  perp-up-dir
					  (v:1- (v! 0.0 0.0 0.0)
						look-dir)))))
	 (trans-matrix (m4:translation 
			(v:1- (v! 0.0 0.0 0.0)
			      (camera-position camera)))))
    (m4:m* rot-matrix trans-matrix)))

(defun resolve-cam-position (sphere-cam-rel-pos cam-target)
  (let* ((phi (* base-maths:+one-degree-in-radians+
		 (v-x sphere-cam-rel-pos)))
	 (theta (* base-maths:+one-degree-in-radians+
		   (+ 90.0 (v-y sphere-cam-rel-pos))))
	 (sin-theta (sin theta))
	 (con-theta (cos theta))
	 (sin-phi (sin phi))
	 (cos-phi (cos phi))
	 (dir-to-cam (v! (* sin-theta cos-phi)
			con-theta
			(* sin-theta sin-phi))))
    (v:+ cam-target (v:* dir-to-cam (v-z sphere-cam-rel-pos)))))



;----------------------------------------------

(defun gen-gs-terrain-model (&optional (depth 6)
			       (square-size 20.0))
  (destructuring-bind (verts indicies) 
      (get-terrain-verts-and-indices 
       (diamond-square :depth depth
		       :random-start 50.0
		       :random-decay 0.54
		       :corner-seed '(120.0 -130.0 0.0 50.0)) 
       square-size)
    
    (make-entity 
     :position (v! 0.0 0.0 -15.0)
     :rotation (v! 0.0 0.0 0.0)
     :stream (cgl:make-gpu-stream 
	      :vao (cgl:make-vao 
		    (cgl:gen-buffer
		     :initial-contents 
		     (cgl:destructuring-allocate 'vert-data 
						 verts))
		    :element-buffer 
		    (cgl:gen-buffer 
		     :initial-contents
		     (cgl:destructuring-allocate :unsigned-short
						 indicies)
		     :buffer-target :element-array-buffer))
	      :length (length indicies)
	      :index-type :unsigned-short))))


(defun rgb (r g b)
  `(,(/ r 255.0) ,(/ g 255.0) ,(/ b 255.0) 0.0))

;; (defun pick-color (height)
;;   (cond ((< height -4.0) (rgb 255.0 255.0 255.0))
;; 	((< height 20.0) (rgb 61.0 128.0 65.0))
;; 	(t (rgb 222.0 172.0 105.0))))

(defun pick-color (x)
  (declare (ignore x))
  (make-array 4 :element-type 'single-float
	      :initial-contents `(,(random 1.0) ,(random 1.0) 
				   ,(random 1.0) 0.0)))

(defun get-terrain-verts-and-indices (terrain square-size)
  (labels ((gen-vert (data x y)
	     (list 
	      (make-array 3 :element-type 'single-float
			  :initial-contents `(,(* square-size x) 
					       ,(aref data x y)
					       ,(* square-size y)))
	      (pick-color (aref data x y)))))
    (let* ((data terrain)
	   (data-dimen (array-dimensions data)))
      (list 
       (loop for y below (second data-dimen)
	  append (loop for x below (first data-dimen)
		    collect (gen-vert data x y)))
       (let ((size (first data-dimen)))
	 (loop for y below (1- size)
	    append
	      (loop for x below (1- size)
		 append `(,(+ x (* y size))
			   ,(+ x (* (1+ y) size))
			   ,(+ (1+ x) (* y size))
			   
			   ,(+ (1+ x) (* y size))
			   ,(+ x (* (1+ y) size))
			   ,(+ (1+ x) (* (1+ y) size))))))))))


(defun diamond-square (&key
                         (depth 2) 
                         (corner-seed '(0.0 0.0 0.0 0.0))
                         (random-start 1.0)
                         (random-decay 0.5))
  (let* ((size  (expt 2 depth))
         (terrain (make-array `(,(1+ size) ,(1+ size)))))
    ;;set corners
    (setf (aref terrain 0 0) (first corner-seed)
          (aref terrain size 0) (second corner-seed)
          (aref terrain 0 size) (third corner-seed)
          (aref terrain size size) (fourth corner-seed))
    
    (loop for i from depth downto 1
       for x from 1
       do 
         (let ((step-size (expt 2 i))) 
           (setf terrain
                 (square-step size 
                              step-size
                              (* random-start random-decay x)
                              (diamond-step size 
                                            step-size 
                                            (* random-start 
                                               random-decay 
                                               x)
                                            terrain)))
	   (setf random-start (* random-start random-decay))))
    terrain))


(defun diamond-step (size step random-range terrain)
  (loop for x from 0 below size by step
     do (loop for y from 0 below size by step
           do (setf (aref terrain 
                          (+ x (floor (/ step 2)))
                          (+ y (floor (/ step 2))))
                    (+ (- (random (* random-range 2.0)) 
                          random-range)
                       (/ (+ (aref terrain x y)
                             (aref terrain (+ x step) y)
                            (aref terrain x (+ y step))
                            (aref terrain (+ x step) (+ y step)))
                          4.0)))))
  terrain)


(defun square-step (size step random-range terrain)
  (let* ((hstep (floor (/ step 2)))
         (mod-val (+ size hstep)))
    (loop for i from hstep by step
       until (> (* hstep (floor (/ i mod-val))) size)
       do (let ((x (mod i mod-val))
                (y (* hstep (floor (/ i mod-val)))))
            (setf (aref terrain x y)
                  (+ (- (random (* random-range 2.0)) 
                        random-range)
                     (/ (+ (aref terrain (max 0 (- x hstep)) y)
                           (aref terrain x (max 0 (- y hstep)))
                           (aref terrain (min size (+ x hstep)) y)
                           (aref terrain x (min size (+ y hstep))))
                        4.0))))))
  terrain)


;----------------------------------------------

(defun init () 
  (setf *camera* (make-camera :position (v! 0.0 0.0 0.0)))
  (setf *shaders* (cgl:load-shaders "6.vert" "6.frag"))
  (setf *prog-1* (apply #'cgl:make-program *shaders*))
  (setf *frustrum-scale* 
	(cepl-camera:calculate-frustrum-scale 45.0))
  (setf *cam-clip-matrix* (cepl-camera:make-cam-clip-matrix 
			   *frustrum-scale*))
  (cgl:set-program-uniforms *prog-1* :cameratoclipmatrix *cam-clip-matrix*)

  ;;setup data 
  
  (setf *entities* (list (gen-gs-terrain-model)))
  
  ;;set options
  (cgl::clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp))  

(defun entity-matrix (entity)
  (reduce #'m4:m* (list
		   (m4:translation (entity-position entity))
		   (m4:rotation-from-euler (entity-rotation entity))
		   (m4:scale (entity-scale entity)))))


;----------------------------------------------

(defun draw ()
  (update-entity (first *entities*))
  (cgl::clear-depth 1.0)
  (cgl::clear :color-buffer-bit :depth-buffer-bit)

  (cgl:set-program-uniforms *prog-1* :worldtocameramatrix 
			    (calculate-cam-look-at-w2c-matrix
			     *camera*))
  
  (loop for entity in *entities*
       do (cgl::draw-streams *prog-1* (list (entity-stream entity)) 
  		   :modeltoworldmatrix (entity-matrix entity)))
  (gl:flush)
  (sdl:update-display))

(defun reshape (width height)  
  (setf (matrix4:melm *cam-clip-matrix* 0 0)
  	(* *frustrum-scale* (/ height width)))
  (setf (matrix4:melm *cam-clip-matrix* 1 1)
  	*frustrum-scale*)
  (cgl:set-program-uniforms *prog-1* 
			    :cameratoclipmatrix *cam-clip-matrix*)
  (cgl::viewport 0 0 width height))

(defun update-swank ()
  (let ((connection (or swank::*emacs-connection*
			(swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

(defun update-entity (entity)
  (when (entity-right entity)
    (setf (entity-rotation entity) 
	  (v:+ (entity-rotation entity)
	       (v! 0.00 -0.05 0.00))))
  (when (entity-left entity)
    (setf (entity-rotation entity) 
	  (v:+ (entity-rotation entity)
	       (v! 0.00 0.05 0.00))))
  (when (entity-forward entity)
    (setf (entity-position entity) 
	  (v:+ (entity-position entity)
	       (v! 0.00 0.00 -0.80))))
  (when (entity-backward entity)
    (setf (entity-position entity) 
	  (v:+ (entity-position entity)
	       (v! 0.00 0.00 0.80)))))

;----------------------------------------------

(defun key-name (sdl-event)
  (cffi:foreign-slot-value (cffi:foreign-slot-pointer 
			    sdl-event
			    'sdl-cffi::sdl-keyboard-event
			    'sdl-cffi::keysym)
                           'sdl-cffi::sdl-key-sym 'sdl-cffi::sym))

(defun forwardp (event)
  (and (eq (sdl:event-type event) :key-down-event)
       (eq (key-name event) :sdl-key-w)))

(defun backwardp (event)
  (and (eq (sdl:event-type event) :key-down-event)
       (eq (key-name event) :sdl-key-s)))

(defun rightp (event)
  (and (eq (sdl:event-type event) :key-down-event)
       (eq (key-name event) :sdl-key-a)))

(defun leftp (event)
  (and (eq (sdl:event-type event) :key-down-event)
       (eq (key-name event) :sdl-key-d)))

(defun forward-upp (event)
  (and (eq (sdl:event-type event) :key-up-event)
       (eq (key-name event) :sdl-key-w)))

(defun backward-upp (event)
  (and (eq (sdl:event-type event) :key-up-event)
       (eq (key-name event) :sdl-key-s)))

(defun right-upp (event)
  (and (eq (sdl:event-type event) :key-up-event)
       (eq (key-name event) :sdl-key-a)))

(defun left-upp (event)
  (and (eq (sdl:event-type event) :key-up-event)
       (eq (key-name event) :sdl-key-d)))

;; this wont do as when expired it stops working
;; (let ((event-cache nil)
;;       (target '(1 2 3 4)))
;;   (tlambda (make-itime-cache) (beforep !time 5000) (key)
;;     (if (eq key (car event-cache))
;; 	(progn 
;; 	  (setf event-cache (cdr event-cache))
;; 	  (when (null event-cache)
;; 	    (setf event-cache target)
;; 	    t))
;; 	(progn
;; 	  (setf event-cache target)
;; 	  nil))))

(defun make-timed-event-emmiter (event target-sequence time-limit)
  (let ((event-cache nil)
	(target target-sequence)
	(time-cache (make-itime-cache)))
    (lambda (key)
      (if (and (eq key (car event-cache))
	       (beforep time-cache time-limit))
	  (progn
	    (setf event-cache (cdr event-cache))
	    (when (eq event-cache nil)
	      event))
	  (progn
	    (setf event-cache target)
	    (funcall time-cache :reset)
	    nil)))))

(defun event-type-tester (type)
  (lambda (event) (eq (sdl:event-type event) type)))

(defun make-event-emitter (event-symb target-eventp)
  (lambda (event)
    (when (funcall target-eventp event)
      event-symb)))

(defun get-events (event-emitters)
  (loop for event in (collect-sdl-events)
     append (loop for emmiter in event-emitters
		 collect (funcall emmiter event))))
 

;----------------------------------------------


;; [TODO] Should look for quit event and just return that if found.
(defun collect-sdl-events ()
  (let ((x (sdl:new-event)))
    (loop until (= 0 (lispbuilder-sdl-cffi::sdl-poll-event x))
       collect x)))


;; currently anything changed in here is going to need a restart
;; this is obviously unacceptable and will be fixed when I can
;; extract the sdl event handling from their loop system.
(defun run-demo ()
  (setf (sdl:frame-rate) 0)
  (init)
  (reshape 640 480)
  ;; I've been tearing apart sdl's 'with-events' macro to see
  ;; what they include in the main loop. I'm trying to make 
  ;; as thin a layer between the user and the code as possible
  ;; do I feel that the 'with-events' macro has a little too
  ;; much magic.
  ;; Below I have ripped out the parts I need to make this 
  ;; function in the same way as 7.lisp.
  ;; I am currently experimenting with time in the protocode
  ;; folder, and as soon as I have nailed that down I will
  ;; and player controls to this (or prehaps another) example.
  (let ((draw-timer (make-time-buffer))
	(draw-stepper (make-stepper (/ 1000.0 60)))
	(running t)
	(event-emmiters `(,(make-event-emitter 
			    :quit
			    (event-type-tester :quit-event))
			   ,(make-event-emitter
			     :forward
			     #'forwardp)
			   ,(make-event-emitter
			     :backward
			     #'backwardp)
			   ,(make-event-emitter
			     :left
			     #'leftp)
			   ,(make-event-emitter
			     :right
			     #'rightp)
			   ,(make-event-emitter
			     :forward-up
			     #'forward-upp)
			   ,(make-event-emitter
			     :backward-up
			     #'backward-upp)
			   ,(make-event-emitter
			     :left-up
			     #'left-upp)
			   ,(make-event-emitter
			     :right-up
			     #'right-upp))))
    (do-until (not running)
      (dolist (event (get-events event-emmiters))
	(case event
	  (:quit (setf running nil))
	  (:forward (forward))
	  (:backward (backward))
	  (:left (left))
	  (:right (right))
	  (:forward-up (forward-up))
	  (:backward-up (backward-up))
	  (:left-up (left-up))
	  (:right-up (right-up))))
      (on-step-call (draw-stepper 
		     (funcall draw-timer))
	(continuable (update-swank))
	(continuable (draw)))
      (sdl::process-audio))))

(defun forward ()
  (let ((entity (first *entities*)))
    (setf (entity-forward entity) t)))

(defun backward ()
  (let ((entity (first *entities*)))
    (setf (entity-backward entity) t)))

(defun left ()
  (let ((entity (first *entities*)))
    (setf (entity-left entity) t)))

(defun right ()
  (let ((entity (first *entities*)))
    (setf (entity-right entity) t)))

(defun forward-up ()
  (let ((entity (first *entities*)))
    (setf (entity-forward entity) nil)))

(defun backward-up ()
  (let ((entity (first *entities*)))
    (setf (entity-backward entity) nil)))

(defun left-up ()
  (let ((entity (first *entities*)))
    (setf (entity-left entity) nil)))

(defun right-up ()
  (let ((entity (first *entities*)))
    (setf (entity-right entity) nil)))

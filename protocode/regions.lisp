;; Regions
;; -------

;; Regions are functions that return for a given point if you are inside or 
;; outside with some concept of being between states.
;; They are purely conceptual and can be used for anything where there are 
;; states and transitions. A blast radius, and sound propergation a distance
;; field for voxels or implicit surface rendering.

;; In combination with the temporal form of conditional function its should
;; allow for forms like:
(when (inside toxic-zone (pos player))
  (every 10 (incf (health player) -1)))

;; This will imediately respect if the toxic-zone changed shape or the time in
;; this region slowed down. 

;; In fact areas with time shifts can be represented by regions.

;; Regions only define the API not how to calculate how inside or outside you 
;; are.

;; This should allow hooking a region which is used to generate a distance field
;; to a audio signal to make rendering fft based visuals and doddle.

(defregion (x) (and (> x 10) (< x 40)))

(inside )

;; Wait are regions just lambdas?
;; -months pass-
;; i'm back!
;; the answer is: they can be

;; There is one generic predicate that matters
(defgeneric inside (region position))

;; this return a number between ? and ? to indicate if you are inside or not.
;; The variance is to handle transitions
(defconstant +reg-min+ 0.0) 
(defconstant +reg-max+ 1.0) 

;; a region can really be any object, just specialise the generic function on
;; the type.

;; The defintion for functions is already done for you, all it will do is return
;; the number returned by calling the function with the position. Obviously if
;; a non-number is returned, or a number outside the range then an error will be
;; thrown...if the arg spec is wrong that is you're fault.

(defmethod inside ((region function) (position array))
  (funcall region position))
(defmethod inside ((region function) (position list))
  (mapcar region position))

;; as you see if you pass a list of positions the logical things happens.
;; We can work from here
;; obviously a cube region would be simple
(defclass cube-region () (pos width))
(defmethod inside ((region cube-region) (position array))
  (with-slots (pos width) region
    (let ((xmin (- (x pos) (/ width 2))) (xmin (+ (x pos) (/ width 2)))
          (ymin (- (y pos) (/ width 2))) (ymin (+ (y pos) (/ width 2)))
          (zmin (- (z pos) (/ width 2))) (zmin (+ (z pos) (/ width 2)))))
    (if (and (> (v:x position) xmin) (< (v:x position) xmax)
             (> (v:y position) ymin) (< (v:y position) ymax)
             (> (v:z position) zmin) (< (v:z position) zmax))
        +reg-max+ +reg-min+)))

;; there is no transition as this is not a 'fuzzy' edge

;; Spatial scoping?

;; sref gets the value of a spatially scoped variable based on the 
;; var name 'a' and the position 'position'
(sref a position)

;; making this fast will be tricky...really should it allow function based
;; regions? Really if we re going to optimize this we need to know the current
;; maximum extent of the region. Then at least we can filter down the possible
;; candidates.

;; ok so regions need to have an extent slot...in some fashion
(defmethod extent ((region cube))
  (sqrt (* 3 (expt (/ width 2) 2))))
;; if that is true they need some given point to reference this from

;; ..this is starting to define a spec
(defclass region () (position extent))
;; and then implement inside

;; It is more flexible if the system leaves the style of the object somewhat up 
;; to the user.
;; So it could be that the region manager assumes a subclas or region object
;; but also could accept something else but also requires functions to get the 
;; equivilent data.

;; Also when a region is made it should have a key based on gensym so equivilent
;; regions can be tested for batching. So now it is:
(defclass region () (position extent key))
(defmethod inside ((region region) position) 1.0)

;; if a key returns nil then it is impossible to compare, anything else is 
;; compared using eq.




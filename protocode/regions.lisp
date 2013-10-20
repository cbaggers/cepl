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


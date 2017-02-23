(in-package :cepl)

(defun transform (to point &optional from)
  ;; if they are related use their relationship to calc matrix
  ;; if they are not just multiply.. I guess
  )

;; - it's impossible to compute where you are in a space
;; - you have to decide where you are..OR
;; - you have to say how the space you are in relates to another space
;; - that 'relates' bit is a transform..

;; so transforms are the verts in the space graph?
;; nah spaces are only defined it terms of another
;; except for one that we make up and say is root

;; ROOOT

;; ok so ultimately it's screenspace

(defspace screen-space
    :root t)

;; so lets define NDC space

(defspace ndc-space
    :parent screen-space
    :transform :-????-)

;; so what is the transform..it depends right?
;; it likely that in the cases that we transform from 'screen-space' to
;; ndc-space it will actually be from a framebuffer..
;;
;; actually it's viewport and depth-range we need to know

(defun ndc-space->screen-space (depth-range viewport point)
  (destructuring-bind (width height) (size viewport)
    (let ((vo (jungl::viewport-origin viewport))
          (near (v:x depth-range))
          (far (v:y depth-range)))
      (v! (+ (* (/ width 2) (v:x point)) (v:x vo) (/ width 2))
          (+ (* (/ height 2) (v:y point)) (v:y vo) (/ height 2))
          (+ (* (/ (- far near) 2) (v:z point)) (/ (+ far near) 2))))))

;; ok, but in cases like this we cant compute the inverse so we need both

(defspace ndc-space
    :parent screen-space
    :transform-func #'ndc-space->screen-space
    :inverse-func  #'screen-space->ndc-space)

;; would need to be a func that only takes the thing to be transformed

;; lets do this as a matrix

(defun matrix-for-ndc-space->screen-space (depth-range viewport)
  (destructuring-bind (width height) (viewport-dimensions viewport)
    (dvec* ((vo-x vo-y) (jungl::viewport-origin viewport)
            (near far) depth-range)
      (m! (/ width 2)  0.0           0.0                 (+ vo-x (/ width 2))
          0.0          (/ height 2)  0.0                 (+ vo-y (/ height 2))
          0.0          0.0           (/ (- far near) 2)  (/ (+ far near) 2)
          0.0          0.0           0.0                 1.0))))

;; both :transform-func & :inverse-func should really return a mat4


;; OK, finally got something!
;; - transforms are mat4s
;; - a space can be defined in terms of the transform from another space
;; - that 'another space' is KEY! spaces are relational by nature
;; - with a mat4 you dont ever know from to and to what you are transforming
;;   it is purely transformative
;; - so a space is a primitive that says the parent space and the transform to
;;   get from the parent space to the new one.
;; - a vec similaly is ignorant of space
;; - if you want to say (to-space clip-space (v! 1 2 3 0)) it can't as it doesnt
;;   know what space that vec is in
;; - so we add POSITION!
;; - position is a vec with a space
;; - grr position is taken, pos then :)

(defstruct (pos (:constructor %%-pos!))
  (space (error "positions can not be made without a space")
         :type spaces::space-event-node
         :read-only t)
  (point (v! 0 0 0) :type (or (simple-array single-float (2))
                              (simple-array single-float (3))
                              (simple-array single-float (4)))))
(defvar *ndc-space*)

(defun sv! (vec &optional (space *ndc-space*))
  (%%-pos! :space space :point vec))

;; now #'to-space can be defined to take a space and a pos
;; then it has all the data needed to calc and apply the transform

(defun to-space (destination-space pos)
  (m4:transform (get-transform (pos-space pos) destination-space)
                (pos-point pos)))

(defun get-transform (from-space to-space)
  ;; query space hierarchy for transform
  )

;; some spaces only make sense on the gpu. we will define them as such
;; they can still be attached to (for documentation purposes) but attempting
;; to walk to them will give and error (not applicable for cpu etc)
;; internally the resolve to the identity matrix

;; regions are defined to be in a space (only 1 space, though they can be
;; transformed to another) regions are defined by a predicate which says
;; in you are inside the extent of the region, or not.

(defmacro in-space (space &body body)
  `(let ((*current-space* ,space))
     ,@body))



;; every space-node (space-event-node..rename this) generates a graph ID when it
;; is created. when anything attaches to it the whole graph gets a new ID.
;; When a node is detached the node/subgraph that detached gets a new ID.

;; in this way all graphs are uniquely identifyable, so checking if a transform
;; exists between two spaces is an int comparison

;; ALL graphs are connected ALL of them. if no other space is specified then
;; parent space is ndc-space (discuss this).

;; when a space is attached to the graph it causes an event to flow from it's
;; parent down through the children looking for loops & updating the index array

;; Balls, if all graphs are connected then unique graph ID is pointless as they
;; would all have the same one. This means we need to focus on the index


;; other note, bounded spaces are spaces with an accosiated regions... od we
;; need this? why not just have the region as they are already in a space...hmm


;; ok so we need a hashing trie for storing our spaces. spaces still have
;; pointers to each other so communication jumps over the organisation mechanism
;; however we will be able to query the hashing trie in order to find common
;; ancestors. Wait how does this affect GC?

;; the answer is that it buggers it up :D

;; we are reading up on collections which by their natiure keep references. We
;; are making a collection that can abritrarily lose members, so using bit
;; partitioning is a folly. Also our distribution is funky so it doesnt lend
;; itself well to this.

;; instead lets start with something REALLY simple.
;; - a closure thunk that spits out fixnums incrementing from 0

;; when a space is added it creates itself an ID.
;; we can then walk up the tree comparing IDs for common ancestor.

;; this means a lot of jumping around memory potentially. As an optimization we
;; could have an index struct (with it's own pointer up) ands cons up a pool
;; of these at start an hand them out as needed. Should make it a bit more local
;; and also opens up possibilities of optimizing the pool.



;; random thoughts:
;; - if all spaces are connected isnt (eq unsubscribe destroy)?
;; - why do event nodes need a parent? maybe that is a space thing,
;;   and we make the index struct hold the parent pointer.

;; how would we remove parents (subscriptions) from event-node?
;; - remove it from #'subscribe (easy)
;; - remove it from #'unsubscribe (easy)
;; - remove it from #'unsubscribe-from-all (hard)
;; - remove it from #'%move-subscriptions (hard)

;; The second two are hard as they use the parent pointers to find all the
;; nodes to remove self from (etc).

;; We can replace this with an event that removes the node but this means
;; sending the event to all nodes, which in turn means knowing all the root
;; nodes

;; this is too much for now :D.. lets just use the event-nodes parent pointer.


;; ----------------------------------------------------------------------
;; when a new event-node is added a maintenence event should flow through the
;; system looking for cycles

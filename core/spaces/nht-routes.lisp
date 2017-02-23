(in-package :cepl.space.routes)
(in-readtable fn:fn-reader)

;; ideas for memory hungryness:
;; - dual approach? skip lists for short neighbour lists and full arrays
;;   for long?
;; - relationship based ids?
;; - can half the size by only storing a->b and not b->a. Any route to a
;;   lower id is just the reverse of the inverse route
;;   - FECK doesnt work unless always bidirectional
;;   - unless are always bidirectional BUT use ultra high score to deter
;;     using a particular path. Hmm..could work. need extra code in case
;;     the only route is a dumb one and it get's picked.
;; - multi chain system
;;   - first node gets a chain id, next node added joins that chain
;;   - any node which joins an end of the chain (or both ends) is part of
;;     the same chain
;;   - other nodes add (in middle of a chain) start a new chain and are a
;;     member of both chains.
;;   - (n2/2) maps are only for chain local relationships
;;   - then one map for between chains
;;   - optimal for many medium size pools
;;   - doesnt exclude one way relationships
;; - skip list is best if ids clumped based on relationship..which isnt the case
;;   - could add graph-defrag, but when to run it?
;;   - could be incremental, but as outside world only has pointers (fixnums)
;;   - we need a outer-id->inner-id cache.
;; -

;; other ideas
;; - store neighbour-index along with absolute-id to allow fast lookups
;;   into nht transforms when being used by spaces
;; -

;;----------------------------------------------------------------------
;; Types

(deftype subtable ()
  '(array fixnum (#.+subtable-length+ 2)))

(deftype maybe-subtable ()
  '(or null subtable))

(defstruct (route-table (:constructor %make-route-table))
  (sparse-part (error "subtable array must be provided")
               :type (array maybe-subtable (*))))

;;----------------------------------------------------------------------
;; IDs

(defvar %ids (loop :for i :below 64 :collect i))
(defvar %id-count 0)
(defvar %subtable-count 0)

(defun id! ()
  (incf %id-count)
  (let ((sc %subtable-count))
    (setf %subtable-count (ceiling %id-count +subtable-length+))
    (unless (= sc %subtable-count)
      (update-all-route-tables %subtable-count)))
  (extend-routes)
  (or (pop %ids) (error "no nht space ids left")))
(defun free-id (id)
  (loop :for i :below 64 :do
     (%set-route i id -1 -1)
     (%set-route id i -1 -1))
  (push id %ids)
  (decf %id-count)
  t)
(defun get-current-id-count ()
  %id-count)
(defun get-current-subtable-count ()
  (max %subtable-count +default-subtable-count+))
(defun reset-ids ()
  (setf %id-count 0)
  (setf %subtable-count 0)
  (setf %ids (loop :for i :below 64 :collect i))
  t)

;;----------------------------------------------------------------------
;; Routes

(defun %make-routes-array ()
  (make-array +default-subtable-count+ ;; using subtable as it's a good number
              :element-type 'route-table
              :adjustable t
              :fill-pointer +default-subtable-count+
              :initial-contents (loop :for i :below +default-subtable-count+
                                   :collect (make-route-table))))

(defun make-route-table ()
  (let ((len (get-current-subtable-count)))
    (%make-route-table
     :sparse-part (make-array len
                              :element-type 'maybe-subtable
                              :initial-element nil
                              :adjustable t
                              :fill-pointer len))))

(defun make-cache (len)
  (make-array (list len 2) :element-type 'fixnum
              :initial-element -1))

(defvar %cache (make-cache +subtable-length+))
(defvar %routes (%make-routes-array))

(defun extend-routes ()
  (when (> (get-current-id-count) (length %routes))
    (vector-push-extend (make-route-table) %routes +subtable-length+))
  (let ((cache-len (array-dimension %cache 0)))
    (when (> (get-current-id-count) cache-len)
      (setf %cache (make-cache (+ cache-len +subtable-length+)))))
  t)
(defun get-routes () %routes)
(defun get-route-cache () %cache)
(defun reset ()
  (reset-ids)
  (setf %routes (%make-routes-array))
  t)
(defun update-all-route-tables (new-subtable-count)
  (loop :for r :across %routes :do
     (let ((len (length (route-table-sparse-part r))))
       (cond
         ((> new-subtable-count len)
          (vector-push-extend nil (route-table-sparse-part r)))
         ((< new-subtable-count len)
          (setf (fill-pointer (route-table-sparse-part r)) len))))))


(defun on-route-p (from-id to-id id-that-might-be-on-route)
  (let ((current-id from-id))
    (if (or (= id-that-might-be-on-route current-id)
            (= id-that-might-be-on-route to-id))
        t
        (loop :for next-id = (%next-step current-id to-id)
           :if (= id-that-might-be-on-route current-id)
           :return t
           :else :do (setf current-id next-id)
           :until (= next-id to-id)
           :finally (return nil)))))

(defun get-route (from-id to-id)
  (let ((current-id from-id))
    (cons current-id
          (loop :for next-id = (%next-step current-id to-id)
             :collect next-id
             :do (setf current-id next-id)
             :until (= next-id to-id)))))

(defun map-route (from-id to-id function)
  (let ((current-id from-id))
    (loop :for next-id = (%next-step current-id to-id)
       :collect (funcall function current-id next-id)
       :do (setf current-id next-id)
       :until (= next-id to-id))))

(defun reduce-route (from-id to-id function &optional initial-value)
  ;; applying backwards because of matrix multiplication
  (let ((current-id to-id)
        (accum initial-value))
    (loop :for next-id = (%next-step current-id from-id)
       :do (setf accum (funcall function accum next-id current-id))
       :do (setf current-id next-id)
       :until (= next-id from-id))
    accum))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
;; Subtables

(defun make-route-subtable ()
  (make-array '(#.+subtable-length+ 2) :element-type 'fixnum :initial-element -1))

(defmacro with-sparse-elem ((&key next len create-if-missing)
                               (route-table index) &body body)
  (let* ((subtable (gensym "subtable"))
         (sparse-table (gensym "sparse-table"))
         (subtable-index (gensym "subtable-index"))
         (elem-index (gensym "elem-index"))
         (next-var next)
         (len-var len)
         (there `(symbol-macrolet
                     (,@(when next-var `((,next-var (aref ,subtable ,elem-index 0))))
                      ,@(when len-var `((,len-var (aref ,subtable ,elem-index 1)))))
                   ,@body))
         (missing `(symbol-macrolet (,@(when next-var `((,next-var -1)))
                                     ,@(when len-var `((,len-var -1))))
                     ,@body)))
    `(let ((,sparse-table (route-table-sparse-part ,route-table)))
       (multiple-value-bind (,subtable-index ,elem-index)
           (floor ,index +subtable-length+)
         (let ((,subtable (or (aref ,sparse-table ,subtable-index)
                              ,(when create-if-missing
                                     `(setf (aref ,sparse-table ,subtable-index)
                                            (make-route-subtable))))))
           ,(if create-if-missing
                there
                `(if ,subtable ,there ,missing)))))))

(defun rt-elem (route-table x)
  (with-sparse-elem (:next next :len len) (route-table x)
    (cons next len)))

(defun rt-elem-step (route-table x)
  (with-sparse-elem (:next next) (route-table x)
    next))

(defun rt-elem-len (route-table x)
  (with-sparse-elem (:len len) (route-table x)
    len))

(defun set-rt-elem (route-table x step len)
  (with-sparse-elem
      (:next s :len l :create-if-missing t) (route-table x)
    (setf s step l len)
    route-table))

;;-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

(defun %set-route (from-id to-id step length)
  (set-rt-elem (aref (get-routes) from-id) to-id step length))

(defun %route-len (from-id to-id)
  (rt-elem-len (aref (get-routes) from-id) to-id))

(defun %next-step (from-id to-id)
  (rt-elem-step (aref (get-routes) from-id) to-id))

(defun %has-route (from-id to-id)
  (> (rt-elem-step (aref (get-routes) from-id) to-id) -1))

;;----------------------------------------------------------------------
;; Adding

(defun add-id (new-id connect-to-ids)
  (case= (length connect-to-ids)
    (0 nil)
    (1 (connect-to-1 new-id (first connect-to-ids)))
    (otherwise (connect-to-many new-id connect-to-ids)))
  new-id)

(defun id-neighbours (id)
  (loop :for i :below (get-current-id-count)
     :if (%has-route id i)
     :collect i))

(defun connect-to-0 (id)
  (%set-route id id -1 0))

(defun connect-to-1 (my-id new-neighbour-id)
  ;;
  (loop :for i :below (get-current-id-count) :do
     (when (and (not (= i my-id)) (%has-route new-neighbour-id i))
       ;; copy the neighbour's route adding the neighbour to all
       ;; the routes
       (%set-route my-id i new-neighbour-id (1+ (%route-len new-neighbour-id i)))
       ;; walk the routes leaving the route to here)
       (%walk-leaving-trail my-id i)))
  ;;
  (%set-route my-id new-neighbour-id new-neighbour-id 1)
  (%set-route new-neighbour-id my-id my-id 1)
  ;;
  my-id)

(defun %walk-leaving-trail (my-id to-id)
  (let ((current-id my-id))
    (loop :for next-id := (%next-step current-id to-id)
       :for l :from 1 :do
       (%set-route next-id my-id current-id l)
       (setf current-id next-id)
       :until (= current-id to-id))))

;;    cache layout is (next-step length-to-destination original-route-owner)
(defun connect-to-many (my-id new-neighbour-ids)
  (let ((neighbour-ids new-neighbour-ids)
        (cache (get-route-cache))
        (clear (* 2 (get-current-id-count)))) ;; *2 so it is way out of bounds
    (loop :for i :below (get-current-id-count) :do
       ;; we use the len field as an indicator of route presence
       ;; so we 'zero' it out with a large val
       (setf (aref cache i 0) clear)
       ;; for each neighbour
       (loop :for n :in neighbour-ids :do
          (when (and (not (= n i)) (%has-route n i))
            ;; if the neighbour's route to destination 'i is shorter than
            ;; the route in the cache
            (let ((len (%route-len n i)))
              (when (< len (aref cache i 0))
                ;; then cache this route:
                ;; we store the usual stuff (next step & length)
                ;; but also the owner of that short route
                (setf (aref cache i 0) len
                      (aref cache i 1) n))))))
    ;; then for all the cache short routes
    (loop :for i :below (get-current-id-count) :do
       (when (< (aref cache i 0) clear)
         ;; store the route from me to the destination
         (let ((dest i)
               (len (1+ (aref cache i 0)))
               (next-step (aref cache i 1)))
           (%set-route my-id dest next-step len)
           (%walk-leaving-trail my-id dest))))
    ;; add routes between new neighbours
    (loop :for n :in neighbour-ids :do
       (%set-route my-id n n 1)
       (%set-route n my-id my-id 1))
    ;; and propagate the changes
    (propagate-routes (mapcar λ(cons _ my-id) neighbour-ids)
                      (list my-id))
    my-id))

(defun propagate-routes (todo seen)
  (let ((propagate-further nil)
        (cache (get-route-cache))
        (clear (* 2 (get-current-id-count))))
    (destructuring-bind (to-id . from-id) (pop todo)
      (loop :for i :below (get-current-id-count) :do
         (let ((len (aref cache i 0)))
           (when (and (< len clear)
                      (< (1+ len) (%route-len to-id i)))
             (%set-route to-id i from-id (1+ len))
             (setf propagate-further t))))

      (when propagate-further
        (propagate-routes
         ;; visit all nodes except the one we've just come from
         (append todo (mapcar λ(cons _ to-id)
                              (set-difference (id-neighbours to-id) seen)))
         (adjoin from-id seen :test #'=))))))

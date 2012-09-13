
;; standard lisp struct
(defstruct ship
   (x-position 0.0 :type short-float)
   (y-position 0.0 :type short-float)
   (x-velocity 0.0 :type short-float)
   (y-velocity 0.0 :type short-float))

;; desired look
(defglstruct vert-data
    (position :type 'vector3)
    (color :type 'vector4))

;; location is the optional buffer to use. if left blank this 
;; will create a new buffer.
;; the initial contents part is tricky. Do we use raw data?
;; could do but would have to make it a c array first.
(make-gpu-array :length 10 :element-type 'vert-data 
              :location my-buffer :initial-contents gldata)

(make-gpu-stream :arrays ((my-glarray-1 :offset 10) 
                          (my-gl-array-2 :enabled nil))
             :length 3
             :indicies-gpu-array my-indicies-gpu-array)

(funcall pipeline1 my-gpu-stream)


;; Ok so the million pound gorilla-question: how do we hanlde 
;; foriegn arrays & structs. Also what do we name them?
;; Woo GPU ARRAYS...much better, it designates the location and 
;; makes it clear why we need to move data around.

;; Ok so next, how do we get from this:
(cgl:define-interleaved-attribute-format vert-data
  (:type :float :components (x y z w))
  (:type :float :components (r g b a)))

;; to this?:
(defglstruct vert-data
    (position :type 'vector3)
    (color :type 'vector4))

;; the macro can do the type conversion where we need to.
;; part of it is how we then edit things using aref.
;; we are going to need some hefty changes to glarrays.
;; The bright side is that it will make everything much tidier
;; and more lispy.

;; ah hold on!...here is the dissonance I've been feeling
;; current we have to do this:
(setf (aref my-array 5 'x)
      (v:make-vector 0.0 0.0 0.0))
;; instead of this (which is more lispy)
(setf (vert-data-position (aref my-array 5)) 
      (v:make-vector 0.0 0.0 0.0))

;; here lies the issue with the opengl abstractions and why
;; we feel slightly uneasy about them. Ok so we need defglstruct
;; to generate the appropriate accessor functions for both foreign
;; c-arrays and also for buffers. So this will probably have to be
;; generic functions.

;; If it works then we have a fully cohesive way of handling the 
;; vertex data regardless of where it is stored.

;; There will then be:
;; glstructs - used for c and gpu arrays
;; glarrays - used for 'local' data storage
;; gpuarrays - abstractions of the memory pools (buffers)
;; gpustreams - multiple arrays pulled together for a single 
;;              render job.

;; could this be better?
(make-gpu-stream :arrays ((my-glarray-1 :offset 10 :enabled t) 
                          (my-gl-array-2 :enabled nil))
                 :length 3
                 :indicies-gpu-array my-indicies-gpu-array)

;; it would be nice if (my-glarray-1 :offset 10) was replaced with
;; (glsubseq my-glarray-1 10 20)

;; this would make the above
(make-gpu-stream :arrays (((glsubseq my-glarray-1 10) :enabled t) 
                          (my-glarray-1 :enabled nil))
                 :length 3
                 :indicies-gpu-array my-indicies-gpu-array)
;; we can then use glsubseq elsewhere as if it was a lazy subseq
;; for gl data.

;; Wonderful, I'm pretty happy with this. Now lets look at how we 
;; build this out of buffers, vaos and foreign data types.
;; We should also make sure that it is still perfectly possible to 
;; use buffers and vaos on their own if you want to manage things 
;; yourself.

;; buffers are blocks of memory
;; arrays are windows into that data, they have an offset, length
;;  and type.
;; streams are vaos with a length, they must also store the arrays
;;  they rely on so we can pull them out and modify the contents.

;; buffers will need to be aware of their own data layout to avoid
;; collision during array creatiion and data assignment. This means
;; that maybe arrays should be just two numbers: 
;; (buffer_id, format_index)
;; what about subsets of that data, you may have one array with the 
;; vert data for 10 different enemies in there. Ah well that is for
;; streams to handle using the glsubseq function.

;; if we can get this working it will be a beautiful thing.
(setf (glsubseq my-gpuarray-1 10 22) my-glarray-1)

;; right so I think the current definition of glbuffer is ok
;; is the name though? Its already in the cgl package. it should
;; just be buffer. gen-buffer should just be a synonym for for 
;; make-buffer

;; prehaps this:
(defglstruct vert-data
    (position :type 'vector3)
    (color :type 'vector4))

;; could be:
(defglstruct vert-data
    (position :type 'float :length 3)
    (color :type 'float :length 4))
;; this means we dont need different notation for vectors and arrays
;; of data. BUT you cant have arrays as elements in foreign 
;; structs so we wil limit to vectors


;; defglstruct needs to produce:
;; - foreign c struct
;; - lispy struct style accessors for glarrays
;; - lispy struct style accessors for buffers
(defmacro defglstruct (name &body slot-descriptions)
  `())



(defstruct gpu-array 
  (buffer_id)
  (format_index))

;; we need to implement buffer-sub-data using our buffers.
;; we can then use this as the basis for our data pushing.


;; this is from cl-opengl this needs extending for extras we have
;; like :initial-contents
(defmacro with-gl-array ((var type &key count) &body forms)
  "Allocates a fresh GL-ARRAY of type TYPE and COUNT elements.
   The array will be bound to VAR and is freed when execution moves
   outside WITH-GL-ARRAY."
  (with-unique-names (ptr)
    `(with-foreign-object (,ptr ,type ,count)
       (let ((,var (make-gl-array-from-pointer ,ptr ,type ,count)))
         (declare (dynamic-extent ,var))
         ,@forms))))

;;expand for many arrays

;;proof to myself of how setf and foreign data works
(defcstruct tester
  (a :float) (b :float))

(defvar testarray (foreign-alloc 'tester :count 5))

(defun aref-test (array index)
  (mem-aref array 
	    'tester 
	    index))

(defun (setf tester-a) (value instance)
  (setf (foreign-slot-value instance 'tester 'a) value))

(defun tester-a (instance)
  (foreign-slot-value instance 
		      'tester
		      'a))

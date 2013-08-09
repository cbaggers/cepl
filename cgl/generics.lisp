(in-package :cgl)

(defgeneric gl-push (object destination))
(defgeneric gl-pull (object))
(defgeneric gl-pull-1 (object))

(defgeneric gl-subseq (array start &optional end)
  (:documentation
   "This function returns a gpu-array or c-array which contains
   a subset of the array passed into this function.
   Right this will make more sense with a use case:

   Imagine we have one gpu-array with the vertex data for 10
   different monsters inside it and each monster is made of 100
   vertices. The first mosters vertex data will be in the 
   sub-array (gpu-sub-array bigarray 0 1000) and the vertex 
   data for the second monster would be at 
   (gpu-sub-array bigarray 1000 2000)

   This *view* (for lack of a better term) into our array can
   be really damn handy. Prehaps, for example, we want to 
   replace the vertex data of monster 2 with the data in my
   c-array newmonster. We can simply do the following:
   (gl-push (gpu-sub-array bigarray 1000 2000) newmonster)

   Obviously be aware that any changes you make to the parent
   array affect the child sub-array. This can really bite you
   in the backside if you change how the data in the array is 
   laid out."))

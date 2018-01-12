(in-package :%cepl.types)

;;------------------------------------------------------------

(deftype vec2 ()
  '(simple-array single-float (2)))
(deftype vec3 ()
  '(simple-array single-float (3)))
(deftype vec4 ()
  '(simple-array single-float (4)))

(deftype ivec2 ()
  '(simple-array (signed-byte 32) (2)))
(deftype ivec3 ()
  '(simple-array (signed-byte 32) (3)))
(deftype ivec4 ()
  '(simple-array (signed-byte 32) (4)))

(deftype uvec2 ()
  '(simple-array (unsigned-byte 32) (2)))
(deftype uvec3 ()
  '(simple-array (unsigned-byte 32) (3)))
(deftype uvec4 ()
  '(simple-array (unsigned-byte 32) (4)))

(deftype mat3 ()
  '(simple-array single-float (9)))
(deftype mat4 ()
  '(simple-array single-float (16)))

;;------------------------------------------------------------

(defn-inline vec2 ((x single-float) (y single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 2 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y)
    vec))

(defn-inline vec3 ((x single-float) (y single-float) (z single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 3 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y
          (aref vec 2) z)
    vec))

(defn-inline vec4 ((x single-float) (y single-float)
                   (z single-float) (w single-float))
    vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 4 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y
          (aref vec 2) z
          (aref vec 3) w)
    vec))

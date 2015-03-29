(in-package :cgl)

(defstruct-g g-pn ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm))

(defstruct-g g-pc ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defstruct-g g-pt ()
  (position :vec3 :accessor pos)
  (texture :vec2 :accessor tex))

(defstruct-g g-pnc ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (color :vec4 :accessor col))

(defstruct-g g-pnt ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (texture :vec2 :accessor tex))

(defstruct-g g-pntc ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (texture :vec2 :accessor tex)
  (color :vec4 :accessor col))

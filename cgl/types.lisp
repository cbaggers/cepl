(in-package :cgl)

(defglstruct g-pn ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm))

(defglstruct g-pc ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defglstruct g-pt ()
  (position :vec3 :accessor pos)
  (texture :vec2 :accessor tex))

(defglstruct g-pnc ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (color :vec4 :accessor col))

(defglstruct g-pnt ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (texture :vec2 :accessor tex))

(defglstruct g-pntc ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (texture :vec2 :accessor tex)
  (color :vec4 :accessor col))



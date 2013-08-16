(in-package :cgl)

(defglstruct p-n-t
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (texture :vec2 :accessor tex))

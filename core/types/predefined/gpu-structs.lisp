(in-package :cepl.types.predefined)

(defgeneric pos (object))
(defgeneric col (object))
(defgeneric norm (object))
(defgeneric tangent (object))
(defgeneric bi-tangent (object))
(defgeneric tex (object))

(defgeneric (setf pos) (val obj))
(defgeneric (setf col) (val obj))
(defgeneric (setf norm) (val obj))
(defgeneric (setf tex) (val obj))
(defgeneric (setf tangent) (val object))
(defgeneric (setf bi-tangent) (val object))

(defstruct-g g-pn
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm))

(defstruct-g g-pc
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defstruct-g g-pt
  (position :vec3 :accessor pos)
  (texture :vec2 :accessor tex))

(defstruct-g g-pnc
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (color :vec4 :accessor col))

(defstruct-g g-pnt
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (texture :vec2 :accessor tex))

(defstruct-g g-pntc
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (texture :vec2 :accessor tex)
  (color :vec4 :accessor col))

(defstruct-g g-pnb
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (tangent :vec3 :accessor tangent)
  (bi-tangent :vec3 :accessor bi-tangent))

(defstruct-g g-pncb
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (tangent :vec3 :accessor tangent)
  (bi-tangent :vec3 :accessor bi-tangent)
  (color :vec4 :accessor col))

(defstruct-g g-pntb
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (tangent :vec3 :accessor tangent)
  (bi-tangent :vec3 :accessor bi-tangent)
  (texture :vec2 :accessor tex))

(defstruct-g g-pntcb
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor norm)
  (tangent :vec3 :accessor tangent)
  (bi-tangent :vec3 :accessor bi-tangent)
  (texture :vec2 :accessor tex)
  (color :vec4 :accessor col))

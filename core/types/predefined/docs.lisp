(in-package :cepl.types.predefined)

(docs:define-docs
  (defstruct g-pn
    "
g-pn is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm")

  (defstruct g-pc
    "
g-pc is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  color: of type vec4 with an accessor method: col")

  (defstruct g-pt
    "
g-pt is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  texture: of type vec2 with an accessor method: tex")

  (defstruct g-pnc
    "
g-pnc is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  color: of type vec4 with an accessor method: col")

  (defstruct g-pnt
    "
g-pnt is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  texture: of type vec2 with an accessor method: tex")

  (defstruct g-pntc
    "
g-pntc is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  texture: of type vec2 with an accessor method: tex
  color: of type vec4 with an accessor method: col")

  (defstruct g-pnb
    "
g-pnb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent")

  (defstruct g-pncb
    "
g-pncb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent
  color: of type vec4 with an accessor method: col")

  (defstruct g-pntb
    "
g-pntb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent
  texture: of type vec2 with an accessor method: tex")

  (defstruct g-pntcb
    "
g-pntcb is a CEPL gpu struct with the following slots:

  position: of type vec3 with an accessor method: pos
  normal: of type vec3 with an accessor method: norm
  tangent: of type vec3 with an accessor method: tangent
  bi-tangent: of type vec3 with an accessor method: bi-tangent
  texture: of type vec2 with an accessor method: tex
  color: of type vec4 with an accessor method: col")

  (defun MAKE-G-PC
      "
This function returns a new instance of the gpu struct of type G-PC
")
  (defun MAKE-G-PN
      "
This function returns a new instance of the gpu struct of type G-PN
")
  (defun MAKE-G-PNC
      "
This function returns a new instance of the gpu struct of type G-PNC
")
  (defun MAKE-G-PNT
      "
This function returns a new instance of the gpu struct of type G-PNT
")
  (defun MAKE-G-PNTC
      "
This function returns a new instance of the gpu struct of type G-PNTC
")
  (defun MAKE-G-PT
      "
This function returns a new instance of the gpu struct of type G-PT
")

  (defun pos
      "
This function, returns a :vec3 which is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun norm
      "
This function, returns a :vec3 which is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun tangent
      "
This function, returns a :vec3 which is the tangent of the instance.

You can alternatively use the generic function #'tangent to get this value.
")

  (defun bi-tangent
      "
This function, returns a :vec3 which is the bi-tangent of the instance.

You can alternatively use the generic function #'bi-tangent to get this value.
")

  (defun tex
      "
This function, returns a :vec2 which is the texture of the instance.

You can alternatively use the generic function #'tex to get this value.
")

  (defun col
      "
This function, returns a :vec4 which is the color of the instance.

You can alternatively use the generic function #'col to get this value.
")

  (defun g-pn-position
      "
This function, when given an instance of g-pn returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pn-normal
      "
This function, when given an instance of g-pn returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pc-position
      "
This function, when given an instance of g-pc returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pc-color
      "
This function, when given an instance of g-pc returns a :vec4 which
is the color of the instance.

You can alternatively use the generic function #'col to get this value.
")

  (defun g-pt-position
      "
This function, when given an instance of g-pt returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pt-texture
      "
This function, when given an instance of g-pt returns a :vec2 which
is the texture of the instance.

You can alternatively use the generic function #'tex to get this value.
")

  (defun g-pnc-position
      "
This function, when given an instance of g-pnc returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pnc-normal
      "
This function, when given an instance of g-pnc returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pnc-color
      "
This function, when given an instance of g-pnc returns a :vec4 which
is the color of the instance.

You can alternatively use the generic function #'col to get this value.
")

  (defun g-pnt-position
      "
This function, when given an instance of g-pnt returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pnt-normal
      "
This function, when given an instance of g-pnt returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pnt-texture
      "
This function, when given an instance of g-pnt returns a :vec2 which
is the texture of the instance.

You can alternatively use the generic function #'tex to get this value.
")

  (defun g-pntc-position
      "
This function, when given an instance of g-pntc returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pntc-normal
      "
This function, when given an instance of g-pntc returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pntc-texture
      "
This function, when given an instance of g-pntc returns a :vec2 which
is the texture of the instance.

You can alternatively use the generic function #'tex to get this value.
")

  (defun g-pntc-color
      "
This function, when given an instance of g-pntc returns a :vec4 which
is the color of the instance.

You can alternatively use the generic function #'col to get this value.
")

  (defun g-pnb-position
      "
This function, when given an instance of g-pnb returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pnb-normal
      "
This function, when given an instance of g-pnb returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pnb-tangent
      "
This function, when given an instance of g-pnb returns a :vec3 which
is the tangent of the instance.

You can alternatively use the generic function #'tangent to get this value.
")

  (defun g-pnb-bi-tangent
      "
This function, when given an instance of g-pnb returns a :vec3 which
is the bi-tangent of the instance.

You can alternatively use the generic function #'bi-tangent to get this value.
")

  (defun g-pncb-position
      "
This function, when given an instance of g-pncb returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pncb-normal
      "
This function, when given an instance of g-pncb returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pncb-tangent
      "
This function, when given an instance of g-pncb returns a :vec3 which
is the tangent of the instance.

You can alternatively use the generic function #'tangent to get this value.
")

  (defun g-pncb-bi-tangent
      "
This function, when given an instance of g-pncb returns a :vec3 which
is the bi-tangent of the instance.

You can alternatively use the generic function #'bi-tangent to get this value.
")

  (defun g-pncb-color
      "
This function, when given an instance of g-pncb returns a :vec4 which
is the color of the instance.

You can alternatively use the generic function #'col to get this value.
")

  (defun g-pntb-position
      "
This function, when given an instance of g-pntb returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pntb-normal
      "
This function, when given an instance of g-pntb returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pntb-tangent
      "
This function, when given an instance of g-pntb returns a :vec3 which
is the tangent of the instance.

You can alternatively use the generic function #'tangent to get this value.
")

  (defun g-pntb-bi-tangent
      "
This function, when given an instance of g-pntb returns a :vec3 which
is the bi-tangent of the instance.

You can alternatively use the generic function #'bi-tangent to get this value.
")

  (defun g-pntb-texture
      "
This function, when given an instance of g-pntb returns a :vec2 which
is the texture of the instance.

You can alternatively use the generic function #'tex to get this value.
")

  (defun g-pntcb-position
      "
This function, when given an instance of g-pntcb returns a :vec3 which
is the position of the instance.

You can alternatively use the generic function #'pos to get this value.
")

  (defun g-pntcb-normal
      "
This function, when given an instance of g-pntcb returns a :vec3 which
is the normal of the instance.

You can alternatively use the generic function #'norm to get this value.
")

  (defun g-pntcb-tangent
      "
This function, when given an instance of g-pntcb returns a :vec3 which
is the tangent of the instance.

You can alternatively use the generic function #'tangent to get this value.
")

  (defun g-pntcb-bi-tangent
      "
This function, when given an instance of g-pntcb returns a :vec3 which
is the bi-tangent of the instance.

You can alternatively use the generic function #'bi-tangent to get this value.
")

  (defun g-pntcb-texture
      "
This function, when given an instance of g-pntcb returns a :vec2 which
is the texture of the instance.

You can alternatively use the generic function #'tex to get this value.
")

  (defun g-pntcb-color
      "
This function, when given an instance of g-pntcb returns a :vec4 which
is the color of the instance.

You can alternatively use the generic function #'col to get this value.
"))

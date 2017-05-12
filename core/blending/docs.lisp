(in-package :cepl.blending)

(docs:define-docs
  (defstruct blending-params
    "
Blending Parameters dictate how a color is written into an FBO's attachment if
there is already a color there.

Blending Parameters (or blending-params) can be applied in CEPL in 3 places:

- On an FBO:
  This sets the rules for all attachments in the FBO

- On an FBO Attachment:
  This overrides the rules set on the FBO but only for the one attachment.
  This is only supported on some versions of OpenGL.

- On a blending-params object:
  This object can then be applied using with-blending
  (see with-blending's docstring for more info)


The interaction between the options in the blending-params is fairly complex
so whilst we seek to give an introduction here, some cases will be best covered
in other documentation.


-- The Parameters --

There are 3 pairs of parameters that work together to tell GL how to compute
the final color.

In all cases below 'source' means the color coming from the pipeline that needs
to be written into the FBO, and 'destination' means the color already in the FBO

Conceptually what is happening is that source-rgb, source-alpha, destination-rgb
and destination-alpha all name functions that will be called on their respective
values. This will result in new source-rgb, source-alpha, destinations-rgb and
destination-alpha values.

Those new values are then given to the mode-rgb and mode-alpha functions which
compute the final color.

So in pseudo-code it would look this this:

    (defun compute-final-color (blend-params src-rgb src-alpha
                                dest-rgb dest-alpha)
      (let ((new-src-rgb
             (funcall (blend-params-source-rgb blend-params)
                      src-rgb
                      dest-rgb))
            (new-dest-rgb
             (funcall (blend-params-destination-rgb blend-params)
                      src-rgb
                      dest-rgb))
            (new-src-alpha
             (funcall (blend-params-source-alpha blend-params)
                      src-alpha
                      dest-alpha))
            (new-dest-alpha
             (funcall (blend-params-destination-alpha blend-params)
                      src-alpha
                      dest-alpha)))
        ;; compute final colors
        (values (funcall (blend-params-mode-rgb blend-params)
                         new-src-rgb
                         new-dest-rgb)
                (funcall (blend-params-mode-alpha blend-params)
                         new-src-alpha
                         new-dest-alpha))))


-- :source-rgb, :source-alpha, :destination-rgb & :destination-alpha --

This is the list of operations and what they do to the *-rgb or *alpha values
given.

Parameter                 | RGB Factor                      | Alpha Factor
--------------------------------------------------------------------------
:zero                     | (v! 0 0 0)                      | 0
:one                      | (v! 1 1 1)                      | 1
:src-color                | (v! rs0 gs0 bs0)                | as0
:one-minus-src-color      | (- (v! 1 1 1) (v! rs0 gs0 bs0)) | 1 - as0
:dst-color                | (v! rd gd bd)                   | ad
:one-minus-dst-color      | (- (v! 1 1 1) (v! rd gd bd))    | 1 - ad
:src-alpha                | (v! as0 as0 as0)                | as0
:one-minus-src-alpha      | (- (v! 1 1 1) (v! as0 as0 as0)) | 1 - as0
:dst-alpha                | (v! ad ad ad)                   | ad
:one-minus-dst-alpha      | (- (v! 1 1 1) (v! ad ad ad))    | ad
:constant-color           | (v! rc gc bc)                   | ac
:one-minus-constant-color | (- (v! 1 1 1) (v! rc gc bc))    | 1 - ac
:constant-alpha           | (v! ac ac ac)                   | ac
:one-minus-constant-alpha | (- (v! 1 1 1) (v! ac ac ac))    | 1 - ac
:src-alpha-saturate       | (v! i i i)                      | 1
:src1-color               | (v! rs1 gs1 bs1)                | as1
:one-minus-src-color      | (- (v! 1 1 1) (v! rs1 gs1 bs1)) | 1 - as1
:src1-alpha               | (v! as1 as1 as1)                | as1
:one-minus-src-alpha      | (- (v! 1 1 1) (v! As1 As1 As1)) | 1 - As1


-- :mode-rgb & :mode-alpha --

As mentioned above, to compute the final color two equations are used:
one for the RGB portion of the color, and one for the alpha of the color.
This is useful if you want treat rgb and alpha differently when producing the
end result.

The equations available are:

:func-add - The source and destination colors are added to each other.
            O = sS + dD. The s and d are blending parameters that are
            multiplied into each of S and D before the addition.

:func-subtract - Subtracts the destination from the source. O = sS - dD.
                 The source and dest are again multiplied by blending
                 parameters.

:func-reverse-subtract - Subtracts the source from the destination.
                         O = sD - dS. The source and dest are multiplied by
                         blending parameters.

:min - The output color is the component-wise minimum value of the source
       and dest colors. So performing :min in the RGB equation means that
       Or = min(Sr, Dr), Og = min(Sg, Dg), and so forth.
       The parameters s and d are ignored for this equation.

:max - The output color is the component-wise maximum value of the source and
       dest colors. The parameters s and d are ignored for this equation.


-- Precision --

Despite the apparent precision of the above equations, blending arithmetic is
not exactly specified, because blending operates with imprecise integer color
values.
However, a blend factor that should be equal to 1 is guaranteed not to modify
its multiplicand, and a blend factor equal to 0 reduces its multiplicand to 0.
For example, when srcRGB is GL_SRC_ALPHA, dstRGB is GL_ONE_MINUS_SRC_ALPHA,
and As0 is equal to 1, the equations reduce to simple replacement:
")

  (defun blending-params
      "
This function, when passed an fbo or attachment will return the blending-params
for that object.

For details on what blending-params are, see the docstring for the
blending-params struct
")

  (defun blending-params-p
      "
This function returns t when the given value is a blending-params object,
otherwise it returns nil
")

  (defun make-blending-params
      "
This function makes a new blending-params object.

The valid values for source-rgb, source-alpha, destination-rgb
and destination-alpha are:

 :zero
 :one
 :src-color
 :one-minus-src-color
 :dst-color
 :one-minus-dst-color
 :src-alpha
 :one-minus-src-alpha
 :dst-alpha
 :one-minus-dst-alpha
 :constant-color
 :one-minus-constant-color
 :constant-alpha
 :one-minus-constant-alpha
 :src-alpha-saturate
 :src1-color
 :one-minus-src-color
 :src1-alpha
 :one-minus-src-alpha

The valid values for mode-rgb and mode-alpha are:

 :func-add
 :func-subtract
 :func-reverse-subtract
 :min
 :max

For details on their behaviour on the blending-params object please see the
docstring for blending-params.
")

  (defun copy-blending-params
      "
This function, when passed a blending-params object, will create a new
blending-params object with the same settings as the one passed.
")

  (defun mode-rgb
      "
This function, when passed a blending-params object, will return the name of the
equation that will be used to compute the final color value from the processed
source-rgb and destination-rgb.

The result will be one of the following:

 :func-add
 :func-subtract
 :func-reverse-subtract
 :min
 :max

To see more info on this subject please see the doc-string for the
blending-params struct.
")

  (defun mode-alpha
      "
This function, when passed a blending-params object, will return the name of the
equation that will be used to compute the final alpha value from the processed
source-alpha and destination-alpha.

The result will be one of the following:

 :func-add
 :func-subtract
 :func-reverse-subtract
 :min
 :max

To see more info on this subject please see the doc-string for the
blending-params struct.
")

  (defun source-rgb
      "
This function, when passed a blending-params object, will return the name of the
function that will be applied to the color value that is coming from the
pipeline and is to be combined with the value already in the fbo.

The result will be one of the following:

 :zero
 :one
 :src-color
 :one-minus-src-color
 :dst-color
 :one-minus-dst-color
 :src-alpha
 :one-minus-src-alpha
 :dst-alpha
 :one-minus-dst-alpha
 :constant-color
 :one-minus-constant-color
 :constant-alpha
 :one-minus-constant-alpha
 :src-alpha-saturate
 :src1-color
 :one-minus-src-color
 :src1-alpha
 :one-minus-src-alpha

To see more info on this subject please see the doc-string for the
blending-params struct.
")

  (defun source-alpha
      "
This function, when passed a blending-params object, will return the name of the
function that will be applied to the alpha value that is coming from the
pipeline and is to be combined with the value already in the fbo.

The result will be one of the following:

 :zero
 :one
 :src-color
 :one-minus-src-color
 :dst-color
 :one-minus-dst-color
 :src-alpha
 :one-minus-src-alpha
 :dst-alpha
 :one-minus-dst-alpha
 :constant-color
 :one-minus-constant-color
 :constant-alpha
 :one-minus-constant-alpha
 :src-alpha-saturate
 :src1-color
 :one-minus-src-color
 :src1-alpha
 :one-minus-src-alpha

To see more info on this subject please see the doc-string for the
blending-params struct.
")

  (defun destination-rgb
      "
This function, when passed a blending-params object, will return the name of the
function that will be applied to the color value that is currently in the fbo
and is about to be combined with the value coming from the pipeline.

The result will be one of the following:

 :zero
 :one
 :src-color
 :one-minus-src-color
 :dst-color
 :one-minus-dst-color
 :src-alpha
 :one-minus-src-alpha
 :dst-alpha
 :one-minus-dst-alpha
 :constant-color
 :one-minus-constant-color
 :constant-alpha
 :one-minus-constant-alpha
 :src-alpha-saturate
 :src1-color
 :one-minus-src-color
 :src1-alpha
 :one-minus-src-alpha

To see more info on this subject please see the doc-string for the
blending-params struct.
")

  (defun destination-alpha
      "
This function, when passed a blending-params object, will return the name of the
function that will be applied to the alpha value that is currently in the fbo
and is about to be combined with the value coming from the pipeline.

The result will be one of the following:

 :zero
 :one
 :src-color
 :one-minus-src-color
 :dst-color
 :one-minus-dst-color
 :src-alpha
 :one-minus-src-alpha
 :dst-alpha
 :one-minus-dst-alpha
 :constant-color
 :one-minus-constant-color
 :constant-alpha
 :one-minus-constant-alpha
 :src-alpha-saturate
 :src1-color
 :one-minus-src-color
 :src1-alpha
 :one-minus-src-alpha

To see more info on this subject please see the doc-string for the
blending-params struct.
")

  (defun with-blending
      "
This macro will set the default blending parameters for the scope.

These values will be used unless overriden by blend settings in an fbo.

CEPL ensures the blending settings are undone at the end of the scope.
"))

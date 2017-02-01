(in-package :cepl.context)

(docs:define-docs
  (defun depth-test-function
      "
This function returns the function used to used to compare each incoming pixel
depth value with the depth value present in the depth buffer.

By setf'ing this to nil you disable depth testing

By setting this to one of the following functions you enable depth testing and
control whether a pixel will be written into the output attachment:

 #'never - Never passes.

 #'<     - Passes if the incoming depth value is less than the stored depth
           value.

 #'=     - Passes if the incoming depth value is equal to the stored depth value

 #'<=    - Passes if the incoming depth value is less than or equal to the
           stored depth value.

 #'>     - Passes if the incoming depth value is greater than the stored depth
           value.

 #'/=    - Passes if the incoming depth value is not equal to the stored depth
           value.

 #'>=    - Passes if the incoming depth value is greater than or equal to the
            stored depth value.

 #'aways - Always passes

Initially, it is set to #'<")

  (defun never
      "
One of the depth test functions (see the documentation for #'depth-test-function
for more details.

Given an incoming and stored depths it will always return nil.")

  (defun always
      "
One of the depth test functions (see the documentation for #'depth-test-function
for more details.

Given an incoming and stored depths it will always return T.")

  (defun depth-mask
      "
This function specifies whether the depth buffer is enabled for writing.
If setf'ed to nil, depth buffer writing is disabled. Otherwise, it is enabled.

Initially, it is set to T (enabled)")

(defun depth-clamp
      "
This function specifies whether depth clamping is enabled.

If setf'ed to nil, the -wc ≤ zc ≤ wc plane equation is ignored
    by view volume clipping (effectively, there is no near or far
    plane clipping). See glDepthRange.
If setf'ed to nil, depth buffer writing is disabled. Otherwise, it is enabled.

Initially, it is set to T (enabled)")

  (defun depth-range-vec2
      "
This function returns the depth range as a vec2, with the X component of the
vector being the distance to the near plane and the Y component being the
distance to the far plane.

See https://www.khronos.org/opengl/wiki/GLAPI/glDepthRange for a deeper
description.
")
  )

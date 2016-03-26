(in-package :cepl.image-formats)

(docs:define-docs
  (defun :image-formatp
      "
This function returns t if the value provided is a keyword that can be found in
*image-formats*
")

  (defun :valid-image-format-for-buffer-backed-texturep
      "
This function returns t if the value provided is a keyword that can be found in
*valid-image-formats-for-buffer-backed-texture*
")

  (defun :color-renderable-formatp
      "
This function returns t if the value provided is a keyword that can be found in
*color-renderable-formats*
")

  (defun :depth-formatp
      "
This function returns t if the value provided is a keyword that can be found in
*depth-formats*
")

  (defun :stencil-formatp
      "
This function returns t if the value provided is a keyword that can be found in
*stencil-formats*
")

  (defun :depth-stencil-formatp
      "
This function returns t if the value provided is a keyword that can be found in
*depth-stencil-formats*
")

  (defvar :*unsigned-normalized-integer-formats*
    "
A list of all of OpenGL's unsigned normalized integer formats
")

  (defvar :*signed-normalized-integer-formats*
    "
A list of all of OpenGL's signed normalized integer formats
")

  (defvar :*signed-integral-formats*
    "
A list of all of OpenGL's signed integral formats
")

  (defvar :*unsigned-integral-formats*
    "
A list of all of OpenGL's unsigned integral formats
")

  (defvar :*floating-point-formats*
    "
A list of all of OpenGL's floating point formats
")

  (defvar :*regular-color-formats*
    "
A list of all of OpenGL's regular color formats
")

  (defvar :*special-color-formats*
    "
A list of all of OpenGL's special color formats
")

  (defvar :*srgb-color-formats*
    "
A list of all of OpenGL's srgb color formats
")

  (defvar :*red/green-compressed-formats*
    "
A list of all of OpenGL's red/green compressed formats
")

  (defvar :*bptc-compressed-formats*
    "
A list of all of OpenGL's bptc compressed formats
")

  (defvar :*s3tc/dxt-compessed-formats*
    "
A list of all of OpenGL's s3tc/dxt compessed formats
")

  (defvar :*depth-formats*
    "
A list of all of OpenGL's depth formats
")

  (defvar :*stencil-formats*
    "
A list of all of OpenGL's stencil formats
")

  (defvar :*depth-stencil-formats*
    "
A list of all of OpenGL's depth stencil formats
")

  (defvar :*color-renderable-formats*
    "
A list of all of OpenGL's color renderable formats
")

  (defvar :*valid-image-formats-for-buffer-backed-texture*
    "
A list of all of OpenGL's valid image formats for buffer backed texture
")

  (defvar :*image-formats*
    "
A list of all of OpenGL's image formats
"))

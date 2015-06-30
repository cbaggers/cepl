(asdf:defsystem #:cepl-default
  :serial t
  :depends-on (#:cepl
               #:cepl-backend-sdl
               #:cepl-image-helper
               #:cepl-model-helper))

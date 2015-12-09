(in-package :spaces)

(cgl::deferror not-ancestor () (start-space ancestor-space)
    "spaces:collect-inverse-to - ~s is not an ancestor of ~s"
  ancestor-space start-space)

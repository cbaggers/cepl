(in-package :cepl.docs)

(defun gen-docs ()
  (let ((org.tymoonnext.staple::*extension-file* #p"docs/api/staple.ext.lisp"))
    (labels ((@ (x) (asdf:system-relative-pathname :cepl x)))
      (staple:generate
       :cepl :packages '(:cepl.c-arrays
                         :cepl.gpu-arrays
                         :cepl.streams
                         :cepl.viewports
                         :cepl.textures
                         :cepl.samplers
                         :cepl.memory
                         :cepl.measurements
                         :cepl.fbos
                         :cepl.blending
                         :cepl.pipelines
                         :cepl.image-formats
                         :cepl.pixel-formats
                         :cepl.ubos
                         :cepl.gpu-buffers
                         :cepl.vaos
                         :cepl.types
                         :cepl.types.predefined)
       :template (@ #p"docs/api/cepl-template.ctml")
       :out (@ "docs/api/api.html")
       :if-exists :supersede)
      (staple:generate
       :cepl :packages '(:cepl.host)
       :template (@ "docs/api/host-template.ctml")
       :out (@ "docs/api/host-api.html")
       :if-exists :supersede)))
  :booya)

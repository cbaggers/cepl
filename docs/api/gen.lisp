(in-package :cepl.docs)

(ql:quickload :staple)

(defvar *template-dir*
  (asdf:system-relative-pathname :cepl "docs/api/cepl-template.ctml"))

(defvar *out-dir*
  (asdf:system-relative-pathname :cepl "docs/api/api.html"))

(defun gen-docs ()
  (staple:generate
   :cepl :packages '(:cepl.context
		     :cepl.generics
		     :cepl.blending
		     :cepl.c-arrays
		     :cepl.fbos
		     :cepl.generics
		     :cepl.gpu-arrays.buffer-backed
		     :cepl.gpu-buffers
		     :cepl.image-formats
		     :cepl.pipelines
		     :cepl.pixel-formats
		     :cepl.render-state
		     :cepl.samplers
		     :cepl.space
		     :cepl.streams
		     :cepl.textures
		     :cepl.types
		     :cepl.ubos
		     :cepl.viewports
		     :cepl.host
		     :cepl)
   :template *template-dir*
   :out *out-dir*
   :if-exists :supersede))

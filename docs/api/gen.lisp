(in-package :cepl.docs)

(ql:quickload :staple)

(defparameter *template-dir*
  (asdf:system-relative-pathname :cepl "docs/api/cepl-template.ctml"))

(defparameter *out-dir*
  (asdf:system-relative-pathname :cepl "docs/api/api.html"))

(defparameter *host-template*
  (asdf:system-relative-pathname :cepl "docs/api/host-template.ctml"))

(defparameter *host-out-dir*
  (asdf:system-relative-pathname :cepl "docs/api/host-api.html"))

(defun gen-docs ()
  (staple:generate
   :cepl :packages '(:cepl
		     :cepl.c-arrays
		     :cepl.gpu-arrays.buffer-backed
		     :cepl.streams
		     :cepl.viewports
		     :cepl.textures
		     :cepl.samplers
		     :cepl.fbos
		     :cepl.blending
		     :cepl.pipelines
		     :cepl.generics
		     :cepl.gpu-buffers
		     :cepl.image-formats
		     :cepl.pixel-formats
		     :cepl.ubos
		     :cepl.render-state
		     :cepl.space
		     :cepl.types

		     :cepl.context)
   :template *template-dir*
   :out *out-dir*
   :if-exists :supersede)
  (staple:generate
   :cepl :packages '(:cepl.host)
   :template *host-template*
   :out *host-out-dir*
   :if-exists :supersede)
  :booya)

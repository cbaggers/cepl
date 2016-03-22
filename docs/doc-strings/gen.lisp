(in-package :cepl.docs)

(ql:quickload :staple)

(defvar *template-dir*
  (asdf:system-relative-pathname :cepl "docs/api/cepl-template.ctml"))

(defvar *out-dir*
  (asdf:system-relative-pathname :cepl "docs/api/api.html"))

(defun gen-docs ()
  (staple:generate
   :cepl :packages '(:cepl.host
		     :cepl.generics
		     :cepl.context
		     :cepl.types
		     :cepl.c-arrays
		     :cepl.gpu-buffers
		     :jungl
		     :cepl.space
		     :cepl)
   :template *template-dir*
   :out *out-dir*
   :if-exists :supersede))

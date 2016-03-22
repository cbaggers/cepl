(in-package :cepl.docs)

(ql:quickload :staple)

(defvar *template-dir*
  (asdf:system-relative-pathname :cepl "docs/api/cepl-template.ctml"))

(defun gen-docs ()
  (staple:generate
   :cepl :packages '(:cepl.host :cepl.generics :jungl :jungl.space :cepl)
   :template *template-dir*
   :if-exists :supersede))

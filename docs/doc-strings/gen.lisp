(in-package :cepl.docs)

(ql:quickload :staple)

(defun gen-docs ()
  (staple:generate
   :cepl :packages '(:cepl.host :cepl-generics :jungl :jungl.space :cepl)
   :if-exists :supersede))

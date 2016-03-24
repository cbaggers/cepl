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
   :cepl :packages '(:cepl.c-arrays
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

(org.tymoonnext.staple::define-tag-processor do-external-symbols (node)
  (let ((package (plump:attribute node "package"))
        (sort (or (plump:attribute node "sort") "#'symb-type<"))
        (exclude (cl-ppcre:split "\\s+" (plump:attribute node "exclude"))))
    (plump:remove-attribute node "package")
    (plump:remove-attribute node "sort")
    (plump:remove-attribute node "exclude")
    (org.tymoonnext.staple::process-attributes node)
    (let ((package (org.tymoonnext.staple::resolve-value (read-from-string package))))
      (org.tymoonnext.staple::process-attribute
       node "iterate"
       (sort
        (remove-if #'(lambda (symb) (org.tymoonnext.staple::%is-excluded symb exclude))
                   (package-symbol-objects2 package))
        (org.tymoonnext.staple::resolve-value (read-from-string sort)))))))

(defun package-symbol-objects2 (package)
  "Gathers all possible symbol-objects of the given package."
  (org.tymoonnext.staple:symbol-objects (package-symbols2 package)))

(defun package-symbols2 (package)
  "Gets all symbols within a package."
  (let ((lst ())
        (package (find-package package)))
    (do-external-symbols (s package)
      (push s lst))
    lst))

	(mapcar #'cgl:make-shader `(,(merge-pathnames "1.vert" *examples-path*) 
				    ,(merge-pathnames "1.frag" *examples-path*))))

  (defvar *examples-path* (merge-pathnames "examples" (asdf:system-source-directory :cepl)))

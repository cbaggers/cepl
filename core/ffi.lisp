(in-package :cepl)

#+darwin
(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((to-path (ns)
             (uiop:ensure-directory-pathname
              (uiop:parse-unix-namestring ns)))
           (brew-prefix ()
             "Returns brew's prefix path or nil"
             (multiple-value-bind (res _ err)
                 (uiop:run-program "brew --prefix" :output :string
                                   :ignore-error-status t)
               (declare (ignore _))
               (when (= err 0)
                 (to-path (string-trim '(#\newline) res))))))
    (let ((ports-paths (mapcar #'to-path '("/opt/local/lib/" "/opt/local/"))))
      (loop :for path :in (cons (brew-prefix) ports-paths) :do
         (when (and path (uiop:directory-exists-p path))
           (pushnew path cffi:*foreign-library-directories* :test #'equal))))))

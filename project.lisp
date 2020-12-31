(in-package :cepl)

(deferror make-project-needs-quickproject () ()
    "make-project: Can't find quickproject

cepl uses the excellent quickproject to make its projects, please load
quickproject and then run this again.")

(deferror make-project-no-such-dir () (dir)
    "make-project: Directory '~a' does not exist"
  dir)

(defvar *template-dir*
  (asdf:system-relative-pathname :cepl "project-template/"))

(defun process-mp-path (pathname name)
  (let ((pathname (if (symbolp pathname)
                      (string-downcase (symbol-name pathname))
                      pathname))
        (name (if (and name (symbolp name))
                  (string-downcase (symbol-name name))
                  name)))
    (if name
        (let ((path (uiop:directory-exists-p pathname)))
          (if path
              (list (uiop:ensure-directory-pathname
                     (uiop:merge-pathnames* name path))
                    name)
              (error 'make-project-no-such-dir :dir pathname)))
        (let ((full (uiop:ensure-directory-pathname
                     (if (uiop:absolute-pathname-p pathname)
                         pathname
                         (uiop:merge-pathnames* pathname (uiop:getcwd))))))
          (list
           full
           (car (last (pathname-directory full))))))))

(defun make-project (pathname &key name (host :cepl.sdl2) (repl :slime)
                                (depends-on '(:skitter :dirt)))
  ;; this has a bunch of little hacks to make the experience of making
  ;; project's better, we can add lots of little helpers here when they
  ;; pick the only valid option. See the skitter.sdl2 example for an
  ;; example
  (let ((qp (find-package :quickproject))
        (depends-on (cepl-utils:listify depends-on)))
    (assert qp () 'make-project-needs-quickproject)
    (destructuring-bind (pathname name) (process-mp-path pathname name)
      (let* (;; if you are using skitter and also cepl.sdl2 then
             ;; you actually will want skitter.sdl2
             (depends-on (if (and (member :skitter depends-on)
                                  (eq host :cepl.sdl2))
                             (cons :cepl.skitter.sdl2 (remove :skitter depends-on))
                             depends-on))
             ;; with skitter.sdl2 there are two input packages that are
             ;; good to have :use'd by default so we add them
             (skitter-sdl-p (member :cepl.skitter.sdl2 depends-on))
             (swank-p (or (eq repl :swank) (eq repl :slime)))
             (slynk-p (or (eq repl :sly) (eq repl :slynk)))
             (livesupport-p (or swank-p slynk-p)))
        (cepl-utils:ni-call
         :quickproject :make-project
         pathname
         :depends-on `(:cepl
                       :rtg-math.vari
                       ,host
                       ,@(when swank-p `(:swank))
                       ,@(when slynk-p `(:slynk))
                       ,@(when livesupport-p `(:livesupport))
                       ,@depends-on)
         :name name
         :template-directory *template-dir*
         :template-parameters (list :skitter-sdl-p skitter-sdl-p
                                    :livesupport-p livesupport-p))
        name))))

(docs:define-docs
  (defun make-project
      "
This function is a simple way to make a lisp project with all the
supporting libraries to get up and running with cepl quickly.

It uses the excellent quickproject project, so before starting be sure
to run the following in your repl:

    (ql:quickload :quickproject)

By default it assumes you want to use sdl2, skitter and dirt, and that
you will be using slime as the communication layer between lisp and your editor.

Valid values for the :repl argument are currently :slime or :slynk.
"))

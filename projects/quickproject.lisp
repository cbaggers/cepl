(in-package :cepl-project)

(defvar *depends* '(#:cepl))

(let ((dir nil))
  (defun get-template-directory ()
    (unless dir
      (setf dir (asdf:system-relative-pathname :cepl #p"projects/templates")))
    dir))

(defun template-name-to-path (name)
  (assert (keywordp name))
  (merge-pathnames-as-directory
   (get-template-directory) #p"templates/"
   (format nil "~a/" (symbol-name name))))

(defun template-exists-p (name)
  (not (null (directory-exists-p (template-name-to-path name)))))

(defun process-name (name)
  (typecase name
    (string (string-downcase name))
    (keyword (process-name (symbol-name name)))
    (symbol (process-name (symbol-name name)))
    (t (error "Invalid type for use as name"))))

(defun make-project (name &key (template :basic))
  (assert (template-exists-p template))
  (let* ((processed-name (process-name name))
         (template-path (template-name-to-path template))
         (quickproject:*template-directory* template-path)
         (quickproject::*name* processed-name))
    (quickproject:make-project
     processed-name
     :depends-on *depends*
     :template-directory quickproject::*template-directory*)
    (rename-file (merge-pathnames-as-file
                  (quickproject::pathname-as-directory processed-name)
                  "base.lisp")
                 (format nil "~a.lisp" processed-name))))

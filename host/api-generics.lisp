(in-package :cepl.host)

(defgeneric %init (host args))
(defgeneric %make-gl-context (host &key &allow-other-keys))
(defgeneric %make-surface (host &rest args &key &allow-other-keys))
(defgeneric %supports-multiple-surfaces-p (host &key &allow-other-keys))
(defgeneric %supports-multiple-contexts-p (host &key &allow-other-keys))

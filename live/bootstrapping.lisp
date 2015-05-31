(in-package :live)

(defun peek (x) (swank:inspect-in-emacs x))

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
   working while cepl runs"
  (continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

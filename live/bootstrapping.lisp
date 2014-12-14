(in-package :live)

(defmacro defdemo (name &key init step (swank-update-sec 0.3) subsystems)
  "Sets up a simple main loop, which does nothing except updates swank and call
   the step function. It will run when run-* is called and will end when stop-* 
   is called.
   An init function can be specialize which will be called the first time run-*
   is called. If you need to run the init function again call the reset-* 
   function and then call run-*"
  (let ((func-name (gensym (symbol-name name)))
        (run-name (symb 'run- name))
        (stop-name (symb 'stop- name))
        (reset-name (symb 'reset- name))
        (running-name (symb name '-runningp))
        (init-func (or (second init) 'progn))
        (step-func (or (second step) (error "step fuction must be specified"))))
    `(let ((running nil)
           (initd nil)
           (last-swank 0))
       (defun ,func-name ()         
         (loop :while running :do
            (,step-func)
            (setq last-swank (- (get-internal-real-time) last-swank))
            (when (> last-swank ,swank-update-sec)
              (setq last-swank (get-internal-real-time))
              (update-swank))))
       (defun ,run-name ()
         (setf running t)
         (unless initd
           (print '(ensure-cepl-subsystems ',subsystems))
           (,init-func))
         (,func-name))
       (defun ,stop-name ()
         (setf running nil))
       (defun ,running-name ()
         running)
       (defun ,reset-name ()
         (setf running nil
               initd nil)))))

(defun peek (x) (swank:inspect-in-emacs x))

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
   working while cepl runs"
  (base-macros:continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

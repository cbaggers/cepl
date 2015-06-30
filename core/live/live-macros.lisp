(in-package :live)

(defmacro main-loop (&key init step (swank-update-sec 0.3)
                          subsystems own-pump)
  "Sets up a simple main loop, which does nothing except updates swank and call
   the step function. It will run when run-* is called and will end when stop-*
   is called.
   An init function can be specialize which will be called the first time run-*
   is called. If you need to run the init function again call the reset-*
   function and then call run-*"

  (declare (ignore subsystems swank-update-sec))
  (let ((run-symb (symb :run-loop))
        (stop-symb (symb :stop-loop)))
    `(progn
       (let ((running nil))
         (defun ,run-symb ()
           ,(when init `(,init))
           (setf running t)
           (format t "-starting-")
           (loop :while running :do
              (live:continuable
                ,@(when (not own-pump)
                        '((update-swank)
                          (evt:pump-events)))
                ,(when step `(,step))))
           (print "-shutting down-")
           nil)
         (defun ,stop-symb () (setf running nil)))
       (evt:def-event-listener (event :sys)
           (when (typep ,(symb 'event) 'evt:will-quit)
             (,stop-symb))))))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "CEPL Continue")))

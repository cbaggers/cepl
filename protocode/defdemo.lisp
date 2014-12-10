
(defdemo test :step #'step-demo :swank-update-sec 0.3)

(defmacro defdemo (name &key init step (swank-update-sec 0.3) subsystems)
  (let ((func-name (gensym (symbol-name name)))
        (start-name (symb 'start- name))
        (stop-name (symb 'stop- name))
        (reset-name (symb 'reset- name))
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
       (defun ,start-name ()
         (setf running t)
         (unless initd
           (ensure-cepl-subsystems ',subsystems)
           (,init-func))
         (,func-name))
       (defun ,stop-name ()
         (setf running nil))
       (defun ,reset-name ()
         (setf running nil
               initd nil)))))


(defun symb (&rest args) (values (intern (format nil "~{~a~}" args))))

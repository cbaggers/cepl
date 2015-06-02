
(defmacro defunf (name args &body body)
  `(progn 
     (when (fboundp ',name)
       (setf (symbol-function ',(symb '*old- name)) 
             (symbol-function ',name)))
     (defun ,name ,args 
       (handler-case (progn ,@body)
         (error () 
           (if (fboundp ',(symb '*old- name))
               (progn (print ,(concatenate 'string (symbol-name name) " fucked up, reverting to old version of the function"))
                      (setf (symbol-function ',name) 
                            (symbol-function ',(symb '*old- name))))
               (error "you fucked it up on the first go genius!")))))))

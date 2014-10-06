
(defmacro deftvar (name form)
  `(progn 
     (let ((worker (tlambda () ,form))) 
       (ttm:add worker))))


(deftvar *time* |current-time|)

(deftvar *sin-time* (sin |current-time|))




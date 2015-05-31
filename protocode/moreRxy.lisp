
(defclass enode () 
  ((func :initarg :func)
   (children :initarg :children :initform nil)))

(defmacro e! (etype &rest args)
  `(prog1 (make-instance ',etype) ;; :func ,func :children (list ,@children)
     (e-init args)))

(defmethod e-cons-end (node-a node-b)
  (setf (slot-value node-a 'children) node-b))

(defun e-list (&rest nodes)
  (let ((last (first nodes)))
    (loop :for n :in (rest nodes) :do 
       (with-slots (children) n
         (if (slot-value last 'children)
             (error "Cannot append to node with children: ~a ~a" n children)
             (setf (slot-value last 'children) (list n
                                                     last) n)))))
  (first nodes))

;;------------------------------------------------

(defclass emap (enode) ())
(defclass eid (enode) ())
(defclass esplit (enode) ())

(defmethod e-push ((node eid) evt)
  (with-slots (children) node
    (if children
        (e-push (first children) evt)
        evt)))

(defmethod e-push ((node emap) evt)
  (with-slots (func children) node
    (if children
        (e-push (first children) (funcall func evt))
        (funcall func evt))))

(defmethod e-push ((node esplit) evt)
  (with-slots (func children) node
    (e-push (nth (funcall func evt) children) evt)))

(defmethod e-replace (old-node new-node))

(setf a (e-list (e! eid)
                (e! eid)
                (e! esplit (lambda (x) (random 3))
                    (e! emap λ(print (list "1" _))) 
                    (e! emap λ(print (list "2" _)))
                    (e! emap λ(print (list "3" _))))))

(defun elog (x) (print (list "1" x)))

(e-> eid eid (esplit (lambda (x) (random 3))
                     (e-> eid (emap elog))
                     (emap elog)
                     (emap elog)))

;; what if #' in a node position meant emap

(setf a (e-> eid eid (esplit (lambda (x) (random 3))
                             (e-> eid #'elog)
                             #'elog
                             #'elog)))

;; That get's kinda tidy, especially compared to the e-list version
(e-> #'rand-vert #'make-snow )

;; symbol -> make a type of this node
;; #' -> this is an emap node

(fn+ #'id #'id (ev-split (lambda (x) (random 3))
                         (fn+ #'id #'elog)
                         #'elog))

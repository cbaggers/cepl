(in-package #:cepl.events)

(defparameter *global-node-index* (make-hash-table :test 'eq))

(defclass event-node ()
  ((body :initarg :body :initform #'identity)
   (subscribing-nodes :initarg :nodes :initform nil)
   (subscribing-funcs :initarg :funcs :initform nil)
   (dispose :initarg :dispose :initform nil)))

(defclass expand (event-node) ())
(defclass pump-func (event-node) ())
(defclass filter (event-node) ())
(defclass terminal (event-node) ())

(defmethod + ((source event-node) (subscriber event-node))
  (cl:push subscriber (slot-value source 'subscribing-nodes))
  (values subscriber
          (lambda ()
            (delete subscriber (slot-value source 'subscribing-nodes)))))

(defmethod + ((source event-node) (subscriber function))
  (cl:push subscriber (slot-value source 'subscribing-funcs))
  (values subscriber
          (lambda ()
            (delete subscriber (slot-value source 'subscribing-funcs)))))

(defmethod - ((source event-node) (subscriber event-node))
  (with-slots (subscribing-nodes) source
    (setf subscribing-nodes (cl:remove subscriber subscribing-nodes)))
  subscriber)

(defmethod - ((source event-node) (subscriber function))
  (with-slots (subscribing-funcs) source
    (setf subscribing-funcs (cl:remove subscriber subscribing-funcs)))  
  subscriber)

(defun %clone-node-tree (node)
  (with-slots (body subscribing-funcs subscribing-nodes) node
    (let ((new-node (make-instance (type-of node) :body body
                                   :funcs subscribing-funcs)))
      (mapcar (lambda (x) (+ new-node (%clone-node-tree x)))
              subscribing-nodes)
      new-node)))

(defmethod swap ((old-node event-node) (new-node event-node))
  (with-slots ((new-nodes subscribing-nodes) (new-funcs subscribing-funcs))
      new-node
    (with-slots ((old-nodes subscribing-nodes) (old-funcs subscribing-funcs))
        old-node
      (setf new-funcs (concatenate 'list new-funcs old-funcs)
            new-nodes (concatenate 'list new-node
                                   (mapcar #'%clone-node-tree old-nodes)))
      new-node)))

(defmethod push (event (node event-node))  
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (let ((result (funcall body event)))
      (loop :for node :in nodes :do (push result node))
      (loop :for func :in funcs :do (funcall func result)))))

(defmethod push (event (node filter))  
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (when (funcall body event)
      (loop :for node :in nodes :do (push event node))
      (loop :for func :in funcs :do (funcall func event)))))

(defmethod push (event (node expand))
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (let ((results (funcall body event)))
      (loop :for result :in results :do
         (loop :for node :in nodes :do (push result node))
         (loop :for func :in funcs :do (funcall func result))))))

(defmethod push (event (node pump-func))
  (declare (ignore event))
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (loop :for result = (funcall body) :while result :do 
       (loop :for node :in nodes :do (push result node))
       (loop :for func :in funcs :do (funcall func result)))))

(defmethod push (event (node terminal))
  (funcall (slot-value node 'body) event))


(defmacro defnode (name (&key source var (kind 'event-node))
                     &body body)
  `(progn (defparameter ,name
            (let ((node (make-instance ',kind :body (lambda (,var) ,@body)))
                  (global-source-node (gethash ',name *global-node-index*)))
              (when global-source-node
                (destructuring-bind (old-source old-node) global-source-node
                  (cepl.events:- old-source old-node)))
              ,(when source
                     `(progn (setf (slot-value node 'dispose) 
                                   (second (multiple-value-list
                                            (cepl.events:+ ,source node))))
                             (setf (gethash ',name *global-node-index*) 
                                   (list ,source node))))
              node))))



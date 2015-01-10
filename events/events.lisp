(in-package #:cepl.events)

(defclass event-node ()
  ((body :initarg :body :initform #'identity)
   (subscribing-nodes :initarg :nodes :initform nil)
   (subscribing-funcs :initarg :funcs :initform nil)
   (dispose :initarg :dispose :initform nil)))

(defclass expand (event-node) ())
(defclass pump-func (event-node) ())
(defclass filter (event-node) ())

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

(defmethod push (event (node event-node))  
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (let ((result (funcall body event)))
      (loop :for node :in nodes :do (push result node))
      (loop :for func :in funcs :do (funcall func result)))))

(defmethod push (event (node event-node))  
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


(defmacro defnode (name (&key source var (kind 'event-node))
                     &body body)
  `(progn (defparameter ,name
            (let ((node (make-instance
                         ',kind :body (lambda (,var) ,@body))))
              ,(when source
                     `(setf (slot-value node 'dispose)
                            (second (multiple-value-list (+ ,source node)))))
              node))))



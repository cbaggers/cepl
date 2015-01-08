(in-package #:cepl.events)

(defparameter *inputs* (make-hash-table))

;; really basic event system

(defclass event-node ()
  ((body :initarg :body :initform #'identity)
   (subscribing-nodes :initarg :nodes :initform nil)
   (subscribing-funcs :initarg :funcs :initform nil)))

(defclass event-expand-node (event-node) ())
(defclass event-pump-func-node (event-node) ())
(defclass event-filter-node (event-node) ())

(defmethod subscribe ((subscriber event-node) (subscribe-to event-node))
  (push subscriber (slot-value subscribe-to 'subscribing-nodes))
  (lambda () (delete subscriber (slot-value subscribe-to 'subscribing-nodes))))

(defmethod subscribe ((subscriber function) (subscribe-to event-node))
  (push subscriber (slot-value subscribe-to 'subscribing-funcs))
  (lambda () (delete subscriber (slot-value subscribe-to 'subscribing-funcs))))

(defmethod event-push (event (node event-node))  
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (let ((result (funcall body event)))
      (loop :for node :in nodes :do (event-push result node))
      (loop :for func :in funcs :do (funcall func result)))))

(defmethod event-push (event (node event-node))  
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (when (funcall body event)
      (loop :for node :in nodes :do (event-push event node))
      (loop :for func :in funcs :do (funcall func event)))))

(defmethod event-push (event (node event-expand-node))
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (let ((results (funcall body event)))
      (loop :for result :in results :do
         (loop :for node :in nodes :do (event-push result node))
         (loop :for func :in funcs :do (funcall func result))))))

(defmethod event-push (event (node event-pump-func-node))
  (declare (ignore event))
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (loop :for result = (funcall body) :while result :do 
       (loop :for node :in nodes :do (event-push result node))
       (loop :for func :in funcs :do (funcall func result)))))

(in-package #:cepl.events)

;;----------------------------------------------------------------------
;; base event

(defstruct+methods cpl-event
  (source-node (error "source-node is mandatory")
          :type cepl-event-node
          :read-only t)
  (timestamp (get-internal-real-time)
             :type fixnum
             :read-only t
             :reader timestamp))

;;----------------------------------------------------------------------
;; meta events

(defstruct
    (new-event-node-event
      (:include cpl-event
                (source-node event-system-meta-node)))
  (new-node (error "new-node must be provided")
              :type cepl-event-node
              :read-only t))

;;----------------------------------------------------------------------
;; event nodes

(defconstant +default-node-name+ :no-human-name)

(defstruct (cepl-event-node (:constructor %make-cepl-event-node)
                              (:conc-name ces-))
  (uid (gensym "EVENT-NODE-")
       :type symbol)
  (name +default-node-name+
        :type symbol)
  (tags nil
        :type list
        :read-only t)
  (subscribers (make-event-node-subscribers) ;; weak refs to consumers
               :type event-node-subscribers
               :read-only t)
  (subscriptions (make-event-node-subscriptions) ;; strong refs to nodes
                 :type event-node-subscriptions
                 :read-only t)
  (filter #'event-no-filter
          :type function
          :read-only t)
  (body #'event-no-body
          :type function
          :read-only t))

(defmethod print-object ((object cepl-event-node) stream)
  (format stream "#<CEPL-EVENT-NODE ~@[:NAME ~S ~]:UID ~s>"
          (ces-name object)
          (ces-uid object)))

(defun event-node-eq (node-a node-b)
  (eq (ces-uid node-a) (ces-uid node-b)))

(defun event-no-filter (e)
  (declare (ignore e) (cpl-event e))
  t)

(defun event-no-body (e)
  (declare (ignore e) (cpl-event e))
  nil)

(defstruct (event-node-subscribers (:conc-name cess-))
  (subscribers nil :type list))

(defstruct (event-node-subscriptions (:conc-name cesp-))
  (subscriptions nil :type list))

(defun make-event-node (&key name tags (filter #'event-no-filter)
                          (body #'event-no-body)
                          subscribe-to)
  (assert (typep filter 'function))
  (assert (typep body 'function))
  (assert (or (null subscribe-to) (typep subscribe-to 'cepl-event-node)))
  (let ((new-node (%make-cepl-event-node
                     :name (or name +default-node-name+)
                     :tags (if (listp tags) tags (list tags))
                     :filter filter
                     :body body)))
    (when (boundp 'event-system-meta-node)
      (push-event-to-event-node (symbol-value 'event-system-meta-node)
                                (make-new-event-node-event :new-node new-node)))
    (when subscribe-to (subscribe new-node subscribe-to))
    new-node))


(defun subscribe (node source-node)
  (assert (typep node 'cepl-event-node))
  (assert (typep source-node 'cepl-event-node))
  (unless (event-node-already-subscribed node source-node)
    (push (trivial-garbage:make-weak-pointer node)
          (cess-subscribers (ces-subscribers source-node)))
    (push source-node (cesp-subscriptions (ces-subscriptions node))))
  source-node)

(defun unsubscribe (node source-node)
  (assert (typep node 'cepl-event-node))
  (assert (typep source-node 'cepl-event-node))
  (let ((subscribers (ces-subscribers source-node)))
    (setf (cess-subscribers subscribers)
          (delete node (cess-subscribers subscribers)
                  :key #'trivial-garbage:weak-pointer-value)))
  (let ((subscriptions (ces-subscriptions source-node)))
    (setf (cesp-subscriptions subscriptions)
          (delete node (cesp-subscriptions subscriptions))))
  source-node)

(defun event-node-already-subscribed (node source-node)
  (member node (cess-subscribers (ces-subscribers source-node))
          :test (lambda (x y)
                  (event-node-eq x
                                 (trivial-garbage:weak-pointer-value y)))))

(defun %move-subscriptions (from to)
  (let ((old-subscribers
             (when from
               (remove nil (mapcar #'trivial-garbage:weak-pointer-value
                                   (cess-subscribers
                                    (ces-subscribers from))))))
        (old-subscriptions
         (when from (cesp-subscriptions (ces-subscriptions from)))))
    ;; recreate all the old subscriptions to and from new node
    (map nil (lambda (x) (subscribe x to)) old-subscribers)
    (map nil (lambda (x) (subscribe to x)) old-subscriptions)
    ;; remove subscriptions from old node
    (map nil (lambda (x) (unsubscribe x from)) old-subscribers)
    (map nil (lambda (x) (unsubscribe from x)) old-subscriptions))
  to)

;;----------------------------------------------------------------------
;; pushing events to nodes

(defun push-event-to-event-node (node event)
  (when (funcall (ces-filter node) event)
    (funcall (ces-body node) event)
    (push-event-to-subscribers node event)))

(defun push-event-to-subscribers (node event)
  (labels ((push-to-subscriber (subscriber)
             (let ((subscribed-node (trivial-garbage:weak-pointer-value subscriber)))
               (when subscribed-node
                   (push-event-to-event-node subscribed-node event)
                 subscriber))))
    (let ((subscribers (cess-subscribers (ces-subscribers node))))
      (mapcar #'push-to-subscriber subscribers))))

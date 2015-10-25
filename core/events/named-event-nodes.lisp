(in-package #:cepl.events)

;;----------------------------------------------------------------------
;; named event nodes

(defmacro def-named-event-node (name (var parent &key filter tags) &body body)
  `(defparameter ,name
     (let* ((old-node (when (boundp ',name) (symbol-value ',name)))
            (result (make-event-node
                     :name ',name
                     :tags ',(cepl-utils:listify tags)
                     :filter ,(or filter '(function event-no-filter))
                     :body (lambda (,var) ,@body))))
       (when old-node (%move-subscriptions old-node result))
       (subscribe result ,parent)
       result)))

;; - example -
;; (def-named-event-node some-events (e cepl-mouse-node :tags :testing)
;;   (print e))

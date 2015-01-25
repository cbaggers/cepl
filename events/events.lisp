(in-package #:cepl.events)


(defun map-evt (function event-source)
  (make-instance 'event-cell 
                 :event (cells:c? (funcall function (event event-source)))))

(defun filter-evt (predicate event-source)
  (make-instance 'event-cell 
                 :event (cells:c? (when (funcall predicate (event event-source))
                              (event event-source)))))

(defun merge-evt (evt-source-a evt-source-b)
  (make-instance 'event-cell 
                 :event (cells:c? (or (event evt-source-a) (event evt-source-b)))))

;;--------------------------------------------
;; root nodes

(def-event-node all-events () :in)



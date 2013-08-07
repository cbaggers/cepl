(in-package :cgl)
;;------------------------------------------------------------

(defun slot-name (slot) (first slot))
(defun slot-type (slot) (second slot))
(defun slot-normalisedp (slot) (third slot))

(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     ,@(loop for slot in slots :collect 
            (let ((ptype (varjo:type-principle (slot-type slot))))
              (list (slot-name slot)
                    (if (varjo:type-aggregate-p ptype)
                        (varjo:type-principle (slot-type slot))
                        ptype)
                    :count (if (varjo:type-arrayp (slot-type slot))
                               (varjo:type-array-length (slot-type slot))
                               1))))))
(defun make-translators (name type-name)
  `((defmethod translate-from-foreign (ptr (type ,type-name))
      (make-instance 'gl-value :element-type ',name :pointer ptr))
    (defmethod translate-into-foreign-memory
        (value (type ,type-name) pointer)
      (print "NO EFFECT: Setting of c structs not yet implemented") 
      nil)))

(defun make-getters-and-setters ())

;; [TODO] generate a dpop1 for this type
(defmacro defglstruct (name &body slot-descriptions)
  (let ((slots (loop for slot in slot-descriptions collect
                    (destructuring-bind 
                          (slot-name slot-type &key (normalised nil) 
                                     (accessor nil) &allow-other-keys)
                        slot
                      (list slot-name (varjo:flesh-out-type slot-type) 
                            normalised accessor))))
        (struct-name (utils:symb name '-struct))
        (type-name (utils:symb name '-type)))
    `(progn
       (varjo:vdefstruct ,name
         ,@(loop for slot in slots
              collect (append (subseq slot 0 2) 
                              (list nil nil)
                              (last slot))))
       ,(make-cstruct-def struct-name slots)
       (define-foreign-type ,type-name () 
         ()
         (:actual-type :struct ,struct-name)
         (:simple-parser ,name))
       ,@(make-translators name type-name)
       ,@(make-getters-and-setters)
       ',name)))

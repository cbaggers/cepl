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

;; [TODO] remove all ignores
(defun make-getters-and-setters (name value-name slots)
  (loop for slot-definition in slots collecting
       (destructuring-bind (slot-name vslot-type normalised accessor)
           slot-definition
         (declare (ignore vslot-type normalised))
         `(defmethod ,(or accessor (utils:symb name '- slot-name )) 
              ((gl-object ,value-name))
            nil))))

(defun make-dpop ()
  ;; (defmethod dpop1 ((type t) gl-object pos data)
  ;;   (setf (aref-gl* gl-object pos) data))
  )

;; [TODO] generate a dpop1 for this type
(defmacro defglstruct (name &body slot-descriptions)
  (when (keywordp name) (error "Keyword name are now allowed for glstructs"))
  (let ((slots (loop for slot in slot-descriptions collect
                    (destructuring-bind 
                          (slot-name slot-type &key (normalised nil) 
                                     (accessor nil) &allow-other-keys)
                        slot
                      (list slot-name (varjo:flesh-out-type slot-type) 
                            normalised accessor))))
        (struct-name (utils:symb name '-struct))
        (type-name (utils:symb name '-type))
        (value-name (utils:symb name '-value)))
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
       (defclass ,value-name (gl-value) ())
       ,@(make-translators name type-name)
       ,@(make-getters-and-setters name value-name slots)
       ,(make-dpop)
       ',name)))

(in-package :cepl.types)

;;------------------------------------------------------------

(defmacro define-gstruct-lisp-equiv ((name &key static) &body slots)
  (let ((pkg (symbol-package name))
        (constructor-name (symb-package pkg :make- name))
        (copy-name (symb-package pkg :copy- name))
        (predicate-name (symb-package pkg name :-p))))
  (if static
      `(defstruct ,name
         ,@(loop :for (a-name a-type) :in slots :collect
              `(,a-name (error) :type a-type)))
      `(progn
         (defclass ,name () (a b))

         (defn ,constructor-name (&key a b) ,name
           (assert (and a b))
           (let ((res (make-instance ',name)))
             (setf (,name-a res) a
                   (,name-b res) b)
             res))

         (defn-inline ,copy-name ((instance ,name)) ,name
           (let ((res (make-instance ',name)))
             (setf (,name-a res) (,name-a instance)
                   (,name-b res) (,name-b instance))
             res))

         (defn-inline ,predicate-name (x) boolean
           (typep x ',name))

         ,@(loop :for (a-name a-type) :in slots
              :for accessor-name := (symb-package pkg a-name :- name)
              :collect
              `((defn-inline ,accessor-name ((x ,name)) t
                  (slot-value x ',a-name))
                (defn-inline (setf ,accessor-name) ((val t) (x ,name)) t
                  (setf (slot-value x ',a-name) val)))))))

(defstruct-g ,name
  (a :vec2)
  (b :vec4))



;;------------------------------------------------------------

(defstruct-g foooo
  (a :int)
  (b :vec4)
  (c (:vec2 10)))

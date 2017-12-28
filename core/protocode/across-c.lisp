(in-package :cepl.c-arrays)
(in-readtable :fn.reader)

(defmacro through-c ((element-type &rest var/slot-list) c-array &body body)
  (let* ((last-2 (last var/slot-list 2))
         (explicit-index-var (eq (first last-2) :index))
         (index-var (if explicit-index-var
                        (let ((name (second last-2)))
                          (assert (and name (symbolp name)) ()
                                  "~s cannot be index var name" name)
                          name)
                        (gensym "INDEX")))
         (slot-list (if explicit-index-var
                        (butlast var/slot-list 2)
                        var/slot-list))
         (known-struct (cepl.types::g-struct-slots element-type)))
    (if known-struct
        (gen-through-c-struct slot-list index-var element-type c-array body)
        (gen-through-c-val (first var/slot-list) index-var element-type c-array
                          body))))

(defun gen-through-c-val (var-name index elem-type c-array body)
  (assert (symbolp var-name) () "through-c: ~a is not a valid variable name"
          var-name)
  (let ((type (cepl.types::lisp-equivalent-of-keyword-cffi-type elem-type)))
    (with-gensyms (carr)
      `(let ((,carr ,c-array))
         (loop :for ,index :below (c-array-total-size ,carr) :do
            (let ((,var-name (aref-c ,carr, index)))
              ,@(when type `((declare (type ,type ,var-name))))
              ,@body))))))


(defun gen-through-c-struct (slot-list index elem-type c-array body)
  (let* ((slot-list (mapcar #'listify slot-list))
         (slot-names (mapcar #'first slot-list))
         (local-slot-names (mapcar λ(or (second _0) _1) slot-list slot-names))
         (accessors (mapcar λ(gensym (symbol-name _)) slot-names))
         (slot-defs (cepl.types::g-struct-slots elem-type)))
    (labels ((def (x)
               (or (find x slot-defs :key #'cepl.types::s-name)
                   (error "through-c: could not find slot ~a in struct ~a ~a"
                          x elem-type slot-defs)))
             (getter (acc slot-name)
               `(,acc (ptr)
                      #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                      ,(cepl.types::slot-getter 'ptr (def slot-name))))
             (setter (acc slot-name)
               `((setf ,acc) (value ptr)
                      #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                 ,(cepl.types::slot-setter 'ptr 'value (def slot-name)))))
      (with-gensyms (carr ptr)
        `(let ((,carr ,c-array))
           (declare (optimize (speed 3) (safety 0) (debug 0)
                              (compilation-speed 0))
                    (type c-array ,carr))
           (flet (,@(mapcar #'getter accessors slot-names)
                  ,@(mapcar #'setter accessors slot-names))
             (declare (ignorable ,@(mapcar λ`(function ,_) accessors))
                      (inline ,@accessors
                              ,@(mapcar λ`(setf ,_) accessors)))
             (loop :for ,index :below (c-array-total-size ,carr) :do
                (let* ((,ptr (ptr-index-1d ,carr ,index)))
                  (declare (optimize (speed 1) (safety 1) (debug 1)
                                     (compilation-speed 1))
                           (ignorable ,ptr))
                  (symbol-macrolet ,(loop :for acc :in accessors
                                  :for l-name :in local-slot-names
                                       :collect `(,l-name (,acc ,ptr)))
                    ,@body)))))))))


#+nil
(defun test-0 (some-c-array)
  (through-c (:vec3 pos) some-c-array
    (print (list :> pos))))

#+nil
(defun test-1 (some-c-array)
  (through-c (cepl:g-pn position) some-c-array
    (print (list :> position))))

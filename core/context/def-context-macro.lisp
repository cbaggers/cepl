(in-package :cepl.context)

;; ---- TODO ----
;;
;; - Add field for getting cache-id from object
;;
;; --------------

(defmacro def-cepl-context (&body slots)
  (let* ((regular-slots (remove :cached slots :key #'first :test #'equal))
         (cached-slots (mapcar #'parse-slot (remove :cached slots :key #'first
                                                    :test-not #'equal))))
    `(progn
       (defclass cepl-context ()
         ((gl-context :initform nil)
          ,@regular-slots
          ,@(mapcat #'gen-cached-context-slots cached-slots)))
       ,@(mapcat #'gen-cached-context-funcs cached-slots))))

(defclass cached-slot-def ()
  ((gl-type :initarg :gl-type)
   (index-size :initarg :index-size)
   (null-obj :initarg :null-obj)
   (id-func :initarg :id-func)
   (target-arg-name :initarg :target-arg-name)
   (target->index-func :initarg :target->index-func)
   (gl-context-accessor :initarg :gl-context-accessor)))

(defun parse-slot (slot)
  (assert (eq (first slot) :cached))
  (dbind (gl-type &key index-size null-obj id-func gl-context-accessor
                  target-arg-name target->index-func)
      (rest slot)
    (assert (and null-obj id-func gl-context-accessor))
    (assert (not (equal index-size t)) ()
            "you didnt implement variable index size yet")
    (make-instance 'cached-slot-def
                   :gl-type gl-type
                   :index-size index-size
                   :null-obj null-obj
                   :id-func id-func
                   :gl-context-accessor gl-context-accessor
                   :target->index-func (if target->index-func
                                           (if index-size
                                               target->index-func
                                               (error ":target->index-func is not valid unless the slot is indexed")))
                   :target-arg-name (if target-arg-name
                                        (if index-size
                                            target-arg-name
                                            (error ":target-arg-name is not valid unless the slot is indexed"))
                                        (when index-size 'target)))))

(defun gen-cached-context-slots (slot)
  (with-slots (index-size) slot
    (if (or (null index-size) (eql index-size 1))
        (gen-uni-cache-slots slot)
        (gen-index-cache-slots slot))))

(defun gen-uni-cache-slots (slot)
  (with-slots (gl-type null-obj) slot
    (let ((bound-id-name (symb :bound- gl-type :-id))
          (array-name (symb :array-of- gl-type :s)))
      `((,bound-id-name
         :initform +null-gl-id+) ;; we use null as a with-*-bound block will
        (,array-name             ;; need to unbind to something sensible
         :initform (make-array 0 :element-type ',gl-type
                               :initial-element ,null-obj
                               :adjustable t
                               :fill-pointer 0))))))

(defun gen-index-cache-slots (slot)
  (with-slots (gl-type index-size null-obj) slot
    (let ((bound-ids-name (symb :array-of-bound- gl-type :-ids))
          (array-name (symb :array-of- gl-type :s)))
      `((,bound-ids-name
         :initform
         (make-array ,(if (numberp index-size)
                          index-size
                          0)
                     :element-type 'gl-id
                     ;; we use null as a with-*-bound block will need to unbind
                     ;; to something sensible
                     :initial-element +null-gl-id+
                     ,@(when (not (numberp index-size))
                             `(:adjustable t))))
        (,array-name
         :initform (make-array 0 :element-type ',gl-type
                               :initial-element ,null-obj
                               :adjustable t
                               :fill-pointer 0))))))

(defun gen-cached-context-funcs (slot)
  (with-slots (index-size) slot
    (if (or (null index-size) (eql index-size 1))
        (gen-uni-cache-funcs slot)
        (gen-index-cache-funcs slot))))

(defun gen-uni-cache-funcs (slot)
  (with-slots (gl-type null-obj gl-context-accessor id-func) slot
    (let ((set-id-name (symb :set- gl-type :-bound-id))
          (get-bound (symb gl-type :-bound))
          (bound-id-name (symb :bound- gl-type :-id))
          (array-name (symb :array-of- gl-type :s)))
      `((defun ,set-id-name (ctx id) ;; Create id caches at max size at GL init
          (with-slots (,bound-id-name gl-context) ctx
            (let ((current ,bound-id-name)
                  (bind-id (if (unknown-gl-id-p id) 0 id)))
              (unless (= id current)
                (setf (,gl-context-accessor gl-context) bind-id)
                (setf ,bound-id-name id))
              id)))

        (defun ,get-bound (ctx)
          (with-slots (,bound-id-name ,array-name gl-context) ctx
            (let* ((id ,bound-id-name)
                   (id (if (unknown-gl-id-p id)
                           (,set-id-name ctx (,gl-context-accessor gl-context))
                           id)))
              ;; in this case we don't check for unknown as foreign-query-*
              ;; can't return that
              (when (and (>= id 0) (< (length ,array-name)))
                (aref ,array-name id)))))

        (defun (setf ,get-bound) (val ctx)
          (,set-id-name ctx (,id-func val)))))))

(defun gen-index-cache-funcs (slot)
  (with-slots (gl-type index-size null-obj gl-context-accessor id-func
                       target-arg-name target->index-func) slot
    (let ((bound-id-name (symb gl-type :-bound-id))
          (set-id-name (symb :set- gl-type :-bound-id))
          (register-name (symb :register- gl-type))
          (get-bound (symb gl-type :-bound))
          (bound-ids-name (symb :array-of-bound- gl-type :-ids))
          (array-name (symb :array-of- gl-type :s))
          (target->index (if target->index-func
                             `(,target->index-func ,target-arg-name)
                             target-arg-name)))
      `((declaim (inline ,bound-id-name))
        (defun ,bound-id-name (ctx index) ;; Create id caches at max size at GL init
          (with-slots (,bound-ids-name) ctx
            (aref ,bound-ids-name index)))

        (defun ,set-id-name (ctx index id) ;; Create id caches at max size at GL init
          (with-slots (,bound-ids-name gl-context) ctx
            (let ((current (,bound-id-name ctx index))
                  (bind-id (if (unknown-gl-id-p id) 0 id)))
              (unless (= id current)
                (setf (,gl-context-accessor gl-context index) bind-id)
                (setf (aref ,bound-ids-name index) id))
              id)))

        (defun ,get-bound (ctx ,target-arg-name)
          (let ((index ,target->index))
            (with-slots (,array-name gl-context) ctx
              (let* ((id (,bound-id-name ctx index))
                     (id (if (unknown-gl-id-p id)
                             (,set-id-name
                              ctx index (,gl-context-accessor gl-context index))
                             id)))
                ;; in this case we don't check for unknown as foreign-query-*
                ;; can't return that
                (when (and (>= id 0) (< (length ,array-name)))
                  (aref ,array-name id))))))

        (defun (setf ,get-bound) (val ctx ,target-arg-name)
          (let ((index ,target->index))
            (,set-id-name ctx index (,id-func val))))

        (defun ,register-name (cepl-context ,gl-type)
          (with-slots (,array-name) cepl-context
            (let ((id (,id-func ,gl-type)))
              (assert (> id 0) (id) "Attempted to register ~s before id fully initialized"
                      ',gl-type)
              (ensure-vec-index ,array-name id ,null-obj)
              (setf (aref ,array-name id) ,gl-type))))))))

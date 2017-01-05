
;;------------------------------------------------------------

(defstruct jam
  (id +null-gl-id+ :type gl-id))

(defvar +null-jam+ (make-jam))

(defun foreign-query-jam (id)
  (declare (ignore id))
  0)
(defun foreign-bind-jam (id)
  id)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defstruct ham
  (id +null-gl-id+ :type gl-id))

(defvar +null-ham+ (make-ham))

(defun foreign-query-ham (index id)
  (declare (ignore index id))
  0)
(defun foreign-bind-ham (index id)
  (declare (ignore index))
  id)

;;------------------------------------------------------------

(defclass thing-context ()
  ((bound-jam-id
    :initform +unknown-gl-id+)
   (array-of-jams
    :initform (make-array 0 :element-type 'jam
                          :initial-element +null-jam+
                          :adjustable t
                          :fill-pointer 0))
   (array-of-bound-ham-ids
    :initform (make-array 11 :element-type 'gl-id
                          :initial-element +unknown-gl-id+))
   (array-of-hams
    :initform (make-array 0 :element-type 'ham
                          :initial-element +null-ham+
                          :adjustable t
                          :fill-pointer 0))))

;;------------------------------------------------------------

(defun set-jam-bound-id (ctx id) ;; Create id caches at max size at GL init
  (with-slots (bound-jam-id) ctx
    (let ((current bound-jam-id)
          (bind-id (if (unknown-gl-id-p id) 0 id)))
      (unless (= id current)
        (foreign-bind-jam bind-id)
        (setf bound-jam-id id))
      id)))

(defun jam-bound (ctx)
  (with-slots (bound-jam-id array-of-jams) ctx
    (let* ((id bound-jam-id)
           (id (if (unknown-gl-id-p id)
                   (set-jam-bound-id ctx (foreign-query-jam id))
                   id)))
      ;; in this case we don't check for unknown as foreign-query-jam can't
      ;; return that
      (when (and (>= id 0) (< (length array-of-jams)))
        (aref array-of-jams id)))))

(defun (setf jam-bound) (val ctx)
  (set-jam-bound-id ctx (jam-id val)))


;;------------------------------------------------------------

(declaim (inline ham-bound-id))
(defun ham-bound-id (ctx index) ;; Create id caches at max size at GL init
  (with-slots (array-of-bound-ham-ids) ctx
    (aref array-of-bound-ham-ids index)))

(defun set-ham-bound-id (ctx index id) ;; Create id caches at max size at GL init
  (with-slots (array-of-bound-ham-ids) ctx
    (let ((current (ham-bound-id ctx index))
          (bind-id (if (unknown-gl-id-p id) 0 id)))
      (unless (= id current)
        (foreign-bind-ham bind-id index)
        (setf (aref array-of-bound-ham-ids index) id))
      id)))

(defun ham-bound (ctx index)
  (let* ((id (ham-bound-id ctx index))
         (id (if (unknown-gl-id-p id)
                 (set-ham-bound-id ctx index (foreign-query-ham index id))
                 id)))
    ;; in this case we don't check for unknown as foreign-query-ham can't
    ;; return that
    (with-slots (array-of-hams) ctx
      (when (and (>= id 0) (< (length array-of-hams)))
        (aref array-of-hams id)))))

(defun (setf ham-bound) (val ctx index)
  (set-ham-bound-id ctx index (ham-id val)))


;; Seems like core of issues is nature of `with-___`. For example with-fbo

#+nil
(with-fbo (fbo)
  (map-g blah 1 2)
  (with-fbo (fbo2)
    (map-g blah 1 2))
  (map-g blah 1 2))

;; however, with the scheme above, we can just use ids for the 'with-*'
;; which will avoid the object lookup cost

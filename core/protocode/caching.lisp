
(defconstant +id-bit-size+ 16)

(deftype thing-id ()
  '(unsigned-byte #.+id-bit-size+))

(declaim (type thing-id +unknown-thing-id+))
(defconstant +unknown-thing-id+ #.(1- (expt 2 +id-bit-size+)))
(defconstant +null-thing-id+ 0)

(declaim (inline unknown-thing-id-p)
         (ftype (function (thing-id) boolean) unknown-thing-id-p))
(defun unknown-thing-id-p (id)
  (declare (thing-id id))
  (= id +unknown-thing-id+))

;;------------------------------------------------------------

(defstruct jam
  (id +null-thing-id+ :type thing-id))

(defvar +null-jam+ (make-jam))

(defvar bound-jam-id +unknown-thing-id+)
(defvar array-of-jams
  (make-array 0 :element-type 'jam
              :initial-element +null-jam+
              :adjustable t
              :fill-pointer 0))

(defun foreign-query-jam (id)
  (declare (ignore id))
  0)
(defun foreign-bind-jam (id)
  id)

(defun jam-bound-id (ctx)
  (declare (ignore ctx))
  bound-jam-id)

(defun set-jam-bound-id (ctx id) ;; Create id caches at max size at GL init
  (let ((current (jam-bound-id ctx))
        (bind-id (if (unknown-thing-id-p id) 0 id)))
    (unless (= id current)
      (foreign-bind-jam bind-id)
      (setf bound-jam-id id))
    id))

(defun jam-bound (ctx)
  (let* ((id (jam-bound-id ctx))
         (id (if (unknown-thing-id-p id)
                 (set-jam-bound-id ctx (foreign-query-jam id))
                 id)))
    ;; in this case we don't check for unknown as foreign-query-jam can't
    ;; return that
    (when (and (>= id 0) (< (length array-of-jams)))
      (aref array-of-jams id))))

(defun (setf jam-bound) (val ctx)
  (set-jam-bound-id ctx (jam-id val)))


;;------------------------------------------------------------

(defstruct ham
  (id +null-thing-id+ :type thing-id))

(defvar +null-ham+ (make-ham))

(defvar array-of-bound-ham-ids
  (make-array 11 :element-type 'thing-id
              :initial-element +unknown-thing-id+))

(defvar array-of-hams
  (make-array 0 :element-type 'ham
              :initial-element +null-ham+
              :adjustable t
              :fill-pointer 0))

(defun foreign-query-ham (index id)
  (declare (ignore index id))
  0)
(defun foreign-bind-ham (index id)
  (declare (ignore index))
  id)

(defun ham-bound-id (ctx index) ;; Create id caches at max size at GL init
  (declare (ignore ctx))
  (aref array-of-bound-ham-ids index))

(defun set-ham-bound-id (ctx index id) ;; Create id caches at max size at GL init
  (let ((current (ham-bound-id ctx index))
        (bind-id (if (unknown-thing-id-p id) 0 id)))
    (unless (= id current)
      (foreign-bind-ham bind-id index)
      (setf (aref array-of-bound-ham-ids index) id))
    id))

(defun ham-bound (ctx index)
  (let* ((id (ham-bound-id ctx index))
         (id (if (unknown-thing-id-p id)
                 (set-ham-bound-id ctx index (foreign-query-ham index id))
                 id)))
    ;; in this case we don't check for unknown as foreign-query-ham can't
    ;; return that
    (when (and (>= id 0) (< (length array-of-hams)))
      (aref array-of-hams id))))

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

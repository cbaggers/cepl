(in-package :cepl.queries)

;;------------------------------------------------------------
;; Constructors

;; These were so simple we left them in the %cepl.types struct
;; definitions (using no-arg boa-constructors)

;;------------------------------------------------------------

;; :samples-passed
;; :any-samples-passed
;; :any-samples-passed-conservative
;; :primitives-generated
;; :transform-feedback-primitives-written
;; :time-elapsed

;;------------------------------------------------------------

(defn begin-scoped-gpu-query ((query scoped-gpu-query)) scoped-gpu-query
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert (not (scoped-gpu-query-active-p query)) ()
          'query-is-already-active
          :query query)
  (with-cepl-context (ctx)
    (multiple-value-bind (success bound) (can-bind-query-p ctx query)
      (cond
        (success (gl:begin-query (gpu-query-enum query)
                                 (gpu-query-id query)))
        ((eq query bound) (error 'query-is-active-bug :query query))
        (t (error 'another-query-is-active :query query :current bound)))
      (force-bind-query ctx query))))

(defn end-scoped-gpu-query ((query scoped-gpu-query)) scoped-gpu-query
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert (scoped-gpu-query-active-p query) ()
          'query-not-active
          :query query)
  (with-cepl-context (ctx)
    (gl:end-query (gpu-query-enum query))
    (force-unbind-query ctx query)))

(defmacro with-gpu-query-bound ((query) &body body)
  (let ((tmp (gensym "query")))
    `(let ((,tmp ,query))
       (begin-scoped-gpu-query ,tmp)
       (unwind-protect (progn ,@body)
         (end-scoped-gpu-query ,tmp)))))

;;------------------------------------------------------------

(defn gpu-query-result-available-p ((query scoped-gpu-query)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-foreign-object (cbool '%gl:uint)
    (%gl:get-query-object-uiv (gpu-query-id query)
                              :query-result-available
                              cbool)
    (/= (mem-aref cbool '%gl:uint) 0)))

(defn calc-query-get-func-for-gpu-array ((arr gpu-array-bb))
    function
  (let ((elem-type (gpu-array-bb-element-type arr)))
    (case elem-type
      (:int #'%gl:get-query-object-iv)
      (:uint #'%gl:get-query-object-uiv)
      (t (error 'cannot-write-query-results-to-this-gpu-array
                :array arr :elem-type elem-type)))))

(defn push-gpu-query-result-to-gpu-array
    ((query scoped-gpu-query)
     (destination-gpu-array gpu-array-bb)
     &optional
     (destination-array-index array-index 0))
    scoped-gpu-query
  (with-cepl-context (ctx)
    (let* ((arr destination-gpu-array)
           (byte-index (+ (gpu-array-bb-offset-in-bytes-into-buffer arr)
                          (* (gpu-array-bb-element-byte-size arr)
                             destination-array-index)))
           (get-func (calc-query-get-func-for-gpu-array arr)))
      (setf (cepl.context:gpu-buffer-bound ctx :query-buffer)
            (gpu-array-buffer destination-gpu-array))
      ;; The idea of pushing to a gpu-array but not unless it can be done
      ;; instantly seems wrong, so here we hardcode the pnam to :query-result
      ;; this can be reviewed in future if this is too restrictive.
      (funcall get-func (gpu-query-id query) :query-result byte-index)))
  query)

(defn pull-gpu-query-result ((query gpu-query)
                             &optional (wait boolean t))
    (values (unsigned-byte 32) boolean)
  (with-cepl-context (ctx)
    (%with-cepl-context-slots (gl-version-float) ctx
      (let ((can-use-no-wait (>= gl-version-float 4.4)))
        (with-foreign-object (c-count :uint32)
          (setf (mem-aref c-count :uint32) 0)
          (cond
            ;; block until we have the value
            (wait (%gl:get-query-object-uiv (gpu-query-id query)
                                            :query-result
                                            c-count)
                  (values (mem-aref c-count :uint32) t))
            ;; we have gl>=4.4 so use the combined fetch
            (can-use-no-wait
             (%gl:get-query-object-uiv (gpu-query-id query)
                                       :query-result-no-wait
                                       c-count)
             (let ((val (mem-aref c-count :uint32)))
               (values val (/= val 0))))
            ;; we fall back to querying if it's available
            ((gpu-query-result-available-p query)
             (%gl:get-query-object-uiv (gpu-query-id query)
                                       :query-result
                                       c-count)
             (values (mem-aref c-count :uint32) t))
            ;; not available, let the people know :)
            (t (values 0 nil))))))))

;;------------------------------------------------------------

;; when all previously issued commands will have completed (gpu is done)
(defn query-all-gpu-commands-completed-time ((timestamp-query timestamp-query))
    timestamp-query
  (%gl:query-counter (gpu-query-id timestamp-query) :timestamp)
  timestamp-query)

;; when all previously given commands have issued
(defn pull-all-gpu-commands-issued-time () (signed-byte 64)
  (with-foreign-object (c-time '%gl:int64)
    (%gl:get-integer-64-v :timestamp c-time)
    (mem-aref c-time '%gl:int64)))

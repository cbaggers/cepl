(in-package :cepl.perf)

(defvar *perf-time-function* nil)
(defvar *perf-second-as-units-function* nil)
(defvar *tags* nil)

(defun assert-host-provider-combo (cepl-host perf-provider)
  (assert (eq perf-provider :sdl2) ()
          "Currently only :sdl2 is supported as a perf-provider")
  (assert (eq cepl-host :cepl.sdl2) ()
          "Currently only the :cepl.sdl2 host is compatible with the :sdl2 perf-provider"))

(defun too-late-p ()
  (find-package :cepl))

(defun load-with-instrumentation
    (&optional (perf-provider :sdl2) (cepl-host :cepl.sdl2) (tags t))
  (assert (not (too-late-p)) ()
          "Sorry, CEPL has already been loaded so it is too late to instrument the code")
  (assert-host-provider-combo cepl-host perf-provider)
  (print "-- Loading perf-provider --")
  (asdf:load-system perf-provider)
  (setf *tags* tags)
  (set-perf-options perf-provider)
  (print "-- Loading CEPL Host --")
  (asdf:load-system :cepl :force t)
  (asdf:load-system cepl-host)
  t)

(defgeneric set-perf-options (perf-provider)
  (:method ((perf-provider (eql :cl)))
    (setf *perf-time-function*
          'get-internal-real-time)
    (setf *perf-second-as-units-function*
          'cl-second-as-units))
  (:method ((perf-provider (eql :sdl2)))
    (setf *perf-time-function*
          (intern "GET-PERFORMANCE-COUNTER" :sdl2))
    (setf *perf-second-as-units-function*
          (intern "SDL-GET-PERFORMANCE-FREQUENCY" :SDL2-FFI.FUNCTIONS))))

(defun cl-second-as-units ()
  internal-time-units-per-second)

;;------------------------------------------------------------

(defconstant +chanl-size+ 60)
(defconstant +buffer-size+ 524288)
(defconstant +max-elems+
  (floor (/ (/ (- +buffer-size+ 1) (cffi:foreign-type-size :uint64)) 2)))

(defvar *func-map* (make-hash-table))
(defvar *last-id* 0)

(defun func-id (name)
  (or (gethash name *func-map*)
      (setf (gethash name *func-map*) (incf *last-id*))))

(defstruct perf-session
  (size-limit-in-mb 0 :type (unsigned-byte 16))
  (chanl-to-output-thread
   (make-instance 'chanl:bounded-channel :size +chanl-size+)
   :type chanl:bounded-channel)
  (chanl-to-cepl-thread
   (make-instance 'chanl:bounded-channel :size +chanl-size+)
   :type chanl:bounded-channel)
  (current-buffer nil :type (or null cffi:foreign-pointer))
  (current-buffer-pos 1 :type (unsigned-byte 32)))

(declaim (type (or null perf-session) *current-profiling-session*))
(defvar *current-profiling-session* nil)

(defun output-loop (session stream)
  (format stream "~%-- perf thread starting up ---")
  (with-open-file (s "/tmp/perf-map" :direction :output
                     :if-exists :rename-and-delete
                     :if-does-not-exist :create)
    (format s "~s" (alexandria:hash-table-alist *func-map*)))
  (with-open-file (o "/tmp/perf" :direction :output
                     :if-exists :rename-and-delete
                     :if-does-not-exist :create)
    (let ((fd (sb-posix:file-descriptor o))
          (from-cepl (perf-session-chanl-to-output-thread session))
          (to-cepl (perf-session-chanl-to-cepl-thread session))
          (size-limit-in-bytes (* (perf-session-size-limit-in-mb session)
                                  1000000))
          ;; Provide a bunch of
          (buffers (loop :for i :below (floor (* 0.75 +chanl-size+)) :collect
                      (cffi:foreign-alloc :uint8 :count +buffer-size+)))
          (bytes-total 0)
          (running t))
      (loop :for buffer :in buffers :do (chanl:send to-cepl buffer))
      (labels ((err (message)
                 (format stream "~%Shutting down prematurely due to this message: ~a"
                         message)))
        (format stream "~%-- perf thread started ---")
        (loop :while running :do
           (let ((message (chanl:recv from-cepl :blockp nil)))
             (etypecase message
               (null nil)
               (cffi:foreign-pointer
                (if (or (cffi:null-pointer-p message)
                        (and (> size-limit-in-bytes 0)
                             (> bytes-total size-limit-in-bytes)))
                    (setf running nil)
                    (let ((data (cffi:inc-pointer
                                 message (cffi:foreign-type-size :uint64)))
                          (len (* (cffi:mem-aref message :uint64 0)
                                  (cffi:foreign-type-size :uint64))))
                      (sb-posix:write fd data len)
                      (incf bytes-total len)
                      (chanl:send to-cepl message))))
               (t (err message))))))
      (chanl:send to-cepl (lambda () (map nil #'cffi:foreign-free buffers)))
      (format stream "~%-- perf thread shut down ---"))))

(defun start-profiling (&optional (size-limit-in-mb 400))
  (assert (>= size-limit-in-mb 0))
  (if *current-profiling-session*
      (format t "~%profile session already in progress")
      (let* ((session (make-perf-session
                       :size-limit-in-mb size-limit-in-mb))
             (stream *standard-output*))
        (print "; -- starting profiling thread --")
        (bt:make-thread (lambda () (output-loop session stream))
                        :name "cepl-perf-session")
        (setf *current-profiling-session* session)
        (print "; -- profiling session live --"))))

(defun stop-profiling ()
  (if *current-profiling-session*
      (let* ((session *current-profiling-session*)
             (chanl-to-output (perf-session-chanl-to-output-thread session)))
        (setf *current-profiling-session* nil)
        (print "; -- sending stop profiling --")
        (when (> (perf-session-current-buffer-pos session) 1)
          (finalize-and-send session))
        (chanl:send chanl-to-output (cffi:null-pointer) :blockp t)
        (print "searching for destructor")
        (let ((trying t))
          (loop :while trying :for i :from 0 :do
             (let ((message
                    (find-if #'functionp
                             (loop :for i :below +chanl-size+ :collect
                                (chanl:recv
                                 (perf-session-chanl-to-cepl-thread session)
                                 :blockp nil)))))
               (cond
                 (message
                  (print "-- destructor found --")
                  (funcall message)
                  (setf trying nil))
                 ((> i 20)
                  (print "-- giving up on destructor --")
                  (setf trying nil))
                 (t (format t "~%attempt ~a" i)
                    (sleep 1))))))
        (print "; -- profiling stopped --")
        (values))
      (format t "~%No profile session in progress")))

(defun profiling-terminated (destructor)
  (print "-- destructor found --")
  (setf *current-profiling-session* nil)
  (funcall destructor)
  (print "; -- profiling stopped --"))

(defun finalize-and-send (session)
  (let ((ptr (perf-session-current-buffer session)))
    (setf (cffi:mem-aref ptr :uint64 0)
          (- (perf-session-current-buffer-pos session) 1))
    (setf (perf-session-current-buffer session) nil)
    (chanl:send (perf-session-chanl-to-output-thread session) ptr)
    (values)))

(defun %log-profile-event (id time)
  (let ((session *current-profiling-session*))
    (when session
      (let ((buffer (perf-session-current-buffer session))
            (pos (perf-session-current-buffer-pos session)))
        (unless buffer
          (let ((recvd (chanl:recv (perf-session-chanl-to-cepl-thread session)
                                   :blockp t)))
            (if (functionp recvd)
                (progn
                  (setf buffer nil)
                  (profiling-terminated recvd))
                (progn
                  (setf buffer recvd)
                  (setf pos 1)))))
        (when buffer
          ;; write data into buffer
          (setf (cffi:mem-aref buffer :int64 pos) id
                (cffi:mem-aref buffer :uint64 (+ 1 pos)) time)
          (incf pos 2)
          ;; if buffer full, send it
          (if (>= pos +max-elems+)
              (progn
                (setf (perf-session-current-buffer-pos session) pos)
                (finalize-and-send session))
              (setf (perf-session-current-buffer session) buffer))
          ;; set the new pos
          (setf (perf-session-current-buffer-pos session) pos))))))

(define-defn-declaration profile (&rest tags)
  (when (or (eq *tags* t) (intersection tags *tags*))
    (assert *perf-time-function*)
    (let* ((id (func-id %func-name)))
      (values `(%log-profile-event ,id (,*perf-time-function*))
              `(%log-profile-event ,(- id) (,*perf-time-function*))))))

(defmacro profile-block (name &body body)
  (let* ((name (if (listp name)
                   (cons :block name)
                   (list :block name)))
         (id (func-id name)))
    `(progn
       (%log-profile-event ,id (,*perf-time-function*))
       (multiple-value-prog1 (progn ,@body)
         (%log-profile-event ,(- id) (,*perf-time-function*))))))

;;------------------------------------------------------------

(defun reverse-hash-map (hm)
  (let ((r (make-hash-table :test #'eql)))
    (maphash (lambda (k v) (setf (gethash v r) k)) hm)
    r))

(defun analyze (&optional (data-path "/tmp/perf") (map-path "/tmp/perf-map"))
  (let ((map (make-hash-table :test #'equal))
        (freq (make-hash-table :test #'equal))
        (track (make-hash-table :test #'equal))
        (total-time (make-hash-table :test #'equal))
        (per-call (make-hash-table :test #'equal))
        (depth 0))
    (with-open-file (s map-path)
      (loop :for (name . id) :in (read s nil nil) :do
         (setf (gethash id map) name)))
    (with-open-file (s data-path :element-type '(unsigned-byte 64))
      (loop :for id := (read-byte s nil nil) :while id :do
         (let* ((id (u64-to-signed id))
                (name (gethash (abs id) map))
                (time (read-byte s nil nil)))
           (when time
             (if (>= id 0)
                 (progn
                   (incf depth)
                   (analyze-in-entry depth (or name id) time
                                     track freq total-time))
                 (progn
                   (analyze-out-entry depth (or name id) time
                                      track freq total-time)
                   (decf depth)))))))
    (maphash (lambda (k v)
               (setf (gethash k per-call)
                     (list (/ (float (gethash k total-time))
                              (float v))
                           v)))
             freq)
    (let ((per-frame-freq (make-hash-table :test #'equal))
          (frames (gethash (intern "HOST-SWAP" :cepl.host) freq)))
      (when frames
        (maphash (lambda (k v)
                   (setf (gethash k per-frame-freq) (/ v (float frames))))
                 freq))
      (list freq
            total-time
            per-call
            (when frames per-frame-freq)))))

(defun analyze-in-entry (depth name time
                         track freq total-time)
  (declare (ignore total-time))
  (setf (gethash (cons name depth) track) time)
  (incf (gethash name freq 0)))

(defun analyze-out-entry (depth name time
                          track freq total-time)
  (declare (ignore freq))
  (let* ((key (cons name depth))
         (in-time (gethash key track))
         (func-time (when in-time (- time in-time))))
    (remhash key track)
    (if func-time
        (incf (gethash name total-time 0) func-time)
        (format t "~%missing 'in time' for ~a" name))))

(defun overview (analyze-results)
  (destructuring-bind (freq total-time per-call per-frame-freq) analyze-results
    (let ((ticks-per-nanosecond
           (/ (funcall (symbol-function *perf-second-as-units-function*))
              1000000f0)))
      (when per-frame-freq
        (let* ((names-of-the-busy
                (mapcar #'car (remove-if-not
                               (lambda (x) (> x 1))
                               (alexandria:hash-table-alist per-frame-freq)
                               :key #'cdr))))
          (format t "~%Per Frame Freq~%--------------~%~{~s~%~}"
                  (sort (alexandria:hash-table-alist per-frame-freq)
                        #'> :key #'cdr))
          (format t "~%~%Per Call Cost (avg)~%--------------~%~{~s~%~}"
                  (mapcar (lambda (x)
                            (destructuring-bind (n p y) x
                              (declare (ignore y))
                              (cons n (/ p ticks-per-nanosecond))))
                          (sort (alexandria:hash-table-alist per-call)
                                #'> :key #'cadr)))
          (format t "~%~%Interesting Funcs (name calls-per-frame cost-per-call frame-time)~%--------------~%~{~s~%~}"
                  (sort (mapcar (lambda (n)
                                  (destructuring-bind (time cnt) (gethash n per-call)
                                    (declare (ignore cnt))
                                    (let* ((ffreq (gethash n per-frame-freq))
                                           (nano (/ time ticks-per-nanosecond)))
                                      (list n
                                            ffreq
                                            nano
                                            (* ffreq nano)))))
                                names-of-the-busy)
                        #'> :key #'fourth)))))
    (list freq total-time per-call per-frame-freq)))

(defun u64-to-signed (num)
  (cffi:with-foreign-object (x :uint64)
    (setf (cffi:mem-aref x :uint64) num)
    (cffi:mem-aref x :int64)))

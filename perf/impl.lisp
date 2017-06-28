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
  (:method ((perf-provider (eql :sdl2)))
    (setf *perf-time-function*
          (intern "GET-PERFORMANCE-COUNTER" :sdl2))
    (setf *perf-second-as-units-function*
          (intern "GET-PERFORMANCE-FREQUENCY" :sdl2))))

;;------------------------------------------------------------

(defconstant +chanl-size+ 60)
(defconstant +buffer-size+ (* 16 1024))
(defconstant +max-elems+
  (floor (/ (/ (- +buffer-size+ 1) (cffi:foreign-type-size :uint64)) 2)))

(defvar *current-profiling-session* nil)

(defstruct perf-session
  (chanl-to-output-thread
   (make-instance 'chanl:bounded-channel :size +chanl-size+)
   :type chanl:bounded-channel)
  (chanl-to-cepl-thread
   (make-instance 'chanl:bounded-channel :size +chanl-size+)
   :type chanl:bounded-channel)
  (current-buffer nil :type (or null cffi:foreign-pointer))
  (current-buffer-pos 1 :type (unsigned-byte 32)))

(defun output-loop (session stream)
  (format stream "~%-- perf thread starting up ---")
  (with-open-file (o "/tmp/perf" :direction :output
                     :if-exists :rename-and-delete
                     :if-does-not-exist :create)
    (let ((fd (sb-posix:file-descriptor o))
          (from-cepl (perf-session-chanl-to-output-thread session))
          (to-cepl (perf-session-chanl-to-cepl-thread session))
          ;; Provide a bunch of
          (buffers (loop :for i :below (floor (* 0.75 +chanl-size+)) :collect
                      (cffi:foreign-alloc :uint8 :count +buffer-size+)))
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
                (if (cffi:null-pointer-p message)
                    (setf running nil)
                    (let ((data (cffi:inc-pointer
                                 message (cffi:foreign-type-size :uint64)))
                          (len (* (cffi:mem-aref message :uint64 0)
                                  (cffi:foreign-type-size :uint64))))
                      (sb-posix:write fd data len)
                      (chanl:send to-cepl message))))
               (t (err message))))))
      (chanl:send to-cepl (lambda () (map nil #'cffi:foreign-free buffers)))
      (format stream "~%-- perf thread shut down ---"))))

(defun start-profiling ()
  (if *current-profiling-session*
      (format t "~%profile session already in progress")
      (let* ((session (make-perf-session))
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

;;------------------------------------------------------------

(defvar *last-id* 0)
(defvar *func-map* (make-hash-table))

(defun func-id (name)
  (or (gethash name *func-map*)
      (setf (gethash name *func-map*) (incf *last-id*))))

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
          (setf buffer (chanl:recv (perf-session-chanl-to-cepl-thread session)
                                   :blockp t))
          (setf pos 1))
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
        (setf (perf-session-current-buffer-pos session) pos)))))

(define-defn-declaration profile (&rest tags)
  (when (or (eq *tags* t) (intersection tags *tags*))
    (assert *perf-time-function*)
    (let* ((id (func-id %func-name)))
      (values `(%log-profile-event ,id (,*perf-time-function*))
              `(%log-profile-event ,(- id) (,*perf-time-function*))))))

;;------------------------------------------------------------

(defun reverse-hash-map (hm)
  (let ((r (make-hash-table :test #'eql)))
    (maphash (lambda (k v) (setf (gethash v r) k)) hm)
    r))

(defun analyze ()
  (let ((map (reverse-hash-map *func-map*))
        (depth 0))
    (with-open-file (s "/tmp/perf" :element-type '(unsigned-byte 64))
      (loop :for id := (read-byte s nil nil) :while id :collect
         (let* ((id (u64-to-signed id))
                (dir (if (< id 0) :out :in))
                (name (gethash (abs id) map))
                (time (read-byte s nil nil)))
           (if (>= id 0)
               (incf depth)
               (decf depth))
           (when time
             (list depth dir (or name id) time)))))))

(defun u64-to-signed (num)
  (cffi:with-foreign-object (x :uint64)
    (setf (cffi:mem-aref x :uint64) num)
    (cffi:mem-aref x :int64)))

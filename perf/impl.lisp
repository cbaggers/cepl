(in-package :cepl.perf)

(defconstant +in+ 0)
(defconstant +out+ 1)

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

(defstruct perf-event
  (id nil :type symbol)
  (direction 0 :type bit)
  (time 0 :type (unsigned-byte 64))
  (units-per-second 0 :type (unsigned-byte 32)))

(defstruct profile-session
  (chanl-to-thread (error "profile-session must be created with a channel")
                   :type chanl:channel)
  (chanl-from-thread (error "profile-session must be created with a channel")
                     :type chanl:channel)
  (thread (error "profile-session must be created with a thread")
          :type bt:thread))

(defvar *file-id* -1)

(defvar *current-profiling-session*
  nil)

(defvar *profile-chanl*
  (make-instance 'chanl:bounded-channel :size 10000))

(defun profile-loop (incoming-chanl outgoing-chanl filepath)
  (let ((session (make-profile-session :chanl-to-thread incoming-chanl
                                       :chanl-from-thread outgoing-chanl
                                       :thread (bt:current-thread))))
    ;; announce our existence
    (chanl:send outgoing-chanl session :blockp t))
  (unwind-protect
       (with-open-file (file-stream filepath :direction :output
                                    :if-exists :rename-and-delete
                                    :if-does-not-exist :create)
         (loop :for event := (chanl:recv incoming-chanl :blockp t)
            :while (not (eq event t)) :do
            (format file-stream
                    "(:id ~a :direction ~a :time ~a :units-per-second ~a)~%"
                    (perf-event-id event)
                    (if (= (perf-event-direction event) +in+) :in :out)
                    (perf-event-time event)
                    (perf-event-units-per-second event))))
    (chanl:send outgoing-chanl :closed :blockp t)
    (setf *current-profiling-session* nil)
    (values)))

(defun start-profiling ()
  (if *current-profiling-session*
      (format t "profile session already in progress")
      (let* ((outgoing-chanl *profile-chanl*)
             (return-chanl (make-instance 'chanl:bounded-channel :size 10))
             (log-rel-path (format nil "./perf/log~a" (incf *file-id*)))
             (log-path (asdf:system-relative-pathname :cepl.perf
                                                      log-rel-path)))
        (print "; -- starting profiling thread --")
        (bt:make-thread (lambda ()
                          (profile-loop outgoing-chanl return-chanl log-path))
                        :name "cepl-perf-session")
        (print "; -- waiting on thread init --")
        (setf *current-profiling-session* (chanl:recv return-chanl :blockp t))
        (print "; -- profiling session live --"))))

(defun stop-profiling ()
  (if *current-profiling-session*
      (let ((chanl-from-thread (profile-session-chanl-from-thread
                                *current-profiling-session*)))
        (print "; -- sending stop profiling --")
        (chanl:send *profile-chanl* t :blockp t)
        (print "; -- waiting on thread --")
        (print (chanl:recv chanl-from-thread :blockp t))
        (print "; -- profiling stopped --")
        (values))
      (format t "No profile session in progress")))

;;------------------------------------------------------------

(defun %log-profile-event (name direction time units-per-second)
  (let ((event (make-perf-event
                :id name
                :direction direction
                :time time
                :units-per-second units-per-second)))
    (or (chanl:send *profile-chanl* event :blockp nil)
        (print "-- profile event lost --"))))

(defmacro log-profile-event (name direction)
  `(%log-profile-event ',name
                       ,direction
                       (,*perf-time-function*)
                       (,*perf-second-as-units-function*)))

(define-defn-declaration profile (name &rest tags)
  ;; - change 'name' to signature
  ;; - add way for defn to provide signature to declares
  ;; - add map for signature to uid
  ;; - pack ID, direction & times in a struct
  (if (or (eq *tags* t) (intersection tags *tags*))
      (values `(log-profile-event ,name +in+)
              `(log-profile-event ,name +out+))))

;;------------------------------------------------------------

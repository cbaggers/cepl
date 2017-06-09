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
  (asdf:load-system cepl-host)
  t)

(defgeneric set-perf-options (perf-provider)
  (:method ((perf-provider (eql :sdl2)))
    (setf *perf-time-function*
          (intern "GET-PERFORMANCE-COUNTER" :sdl2))
    (setf *perf-second-as-units-function*
          (intern "GET-PERFORMANCE-FREQUENCY" :sdl2))))

(defun %log-profile-event (name direction time units-per-second)
  (print (list :> name direction time units-per-second)))

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
      (values `(log-profile-event ,name :in)
              `(log-profile-event ,name :out))))

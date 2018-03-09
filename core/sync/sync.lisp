(in-package :cepl.sync)

;;------------------------------------------------------------

;; Whilst GL is open to there being many kinds of sync object currently there
;; is only fence, and it only has one condition, so we will hardcode this stuff

(defn make-gpu-fence () gpu-fence
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%make-gpu-fence
   (%gl:fence-sync #.(gl-enum :sync-gpu-commands-complete) 0)))

;;------------------------------------------------------------
;; ugly foreign versions
;;
;; Rather than have cffi be nice and convert the enum to a keyword
;; we just want the value as only our code will see it

(%gl::defglextfun ("glClientWaitSync" cepl-client-wait-sync) :int32
  (sync %gl::sync)
  (flags %gl::SyncObjectMask)
  (timeout :uint64))

;;------------------------------------------------------------


(defn wait-on-gpu-fence ((fence gpu-fence)
                         &optional (timeout (signed-byte 64) 0)
                         (flush boolean))
    symbol
  (assert (>= timeout -1))
  (let ((sync (%gpu-fence-obj fence)))
    (flet ((no-timeout ()
             (when flush (gl:flush))
             (%gl:wait-sync sync 0 :timeout-ignored)
             :condition-satisfied)
           (with-timeout ()
             (let* ((flags (if flush
                               #.(gl-enum :sync-flush-commands-bit)
                               0))
                    (status (%gl:client-wait-sync sync flags timeout)))
               (case status
                 ((:already-signaled-apple :already-signaled)
                  :already-signaled)
                 ((:condition-satisfied :condition-satisfied-apple)
                  :condition-satisfied)
                 ((:timeout-expired :timeout-expired-apple)
                  :timeout-expired)
                 (otherwise
                  (error "CEPL: Unknown status code from wait-on-fence: ~a"
                         status))))))
      ;;
      (if (= timeout -1)
          (no-timeout)
          (with-timeout)))))


(defn gpu-fence-signalled-p ((fence gpu-fence)) boolean
  (let* ((sync (%gpu-fence-obj fence))
         (status (%gl:client-wait-sync sync 0 0)))
    (or (eq status :already-signaled)
        (eq status :already-signaled-apple)
        (eq status :condition-satisfied)
        (eq status :condition-satisfied-apple))))

(defn free-gpu-fence ((fence gpu-fence)) null
  (%gl:delete-sync (%gpu-fence-obj fence))
  (setf (%gpu-fence-obj fence) (null-pointer))
  nil)

(defmethod free ((obj gpu-fence))
  (free-gpu-fence obj))

;;------------------------------------------------------------

;; :already-signaled indicates that sync​ was signaled at the time that
;; glclientwaitsync was called.
;;
;; :timeout-expired indicates that at least timeout​ nanoseconds passed
;; and sync​ did not become signaled.
;;
;; :condition-satisfied indicates that sync​ was signaled before the
;; timeout expired.
;;
;; :wait-failed indicates that an error occurred. additionally, an
;; opengl error will be generated.

;;------------------------------------------------------------

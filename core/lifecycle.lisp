(in-package :cepl.lifecycle)

;; we use very little of the lifecyle right now (only :uninitialized
;; :active & :shutting-down) but it's here for completeness

;; This is not related to 'focus' or whether the window is minimized/maximized
;; which are a seperate matters

;; we should expose this to cepl.host at some point, but it's not urgent

(defvar *lifecycle-state* :uninitialized)

(define-const +lifecycle-states+
  '(:uninitialized
    :suspended
    :active
    :shutting-down))

;;----------------------------------------------------------------------
;; inspect

(defun+ uninitialized-p ()
  (eq *lifecycle-state* :uninitialized))

(defun+ suspended-p ()
  (eq *lifecycle-state* :suspended))

(defun+ active-p ()
  (eq *lifecycle-state* :active))

(defun+ shutting-down-p ()
  (eq *lifecycle-state* :shutting-down))

;;----------------------------------------------------------------------
;; listeners

(defvar *suspended-listeners* nil)
(defvar *active-listeners* nil)
(defvar *shutting-down-listeners* nil)

(defun+ listen-to-lifecycle-changes (func &rest states-to-subscribe-to)
  (assert (every (lambda (x) (member x +lifecycle-states+))
                 states-to-subscribe-to))
  (let ((stst (or states-to-subscribe-to +lifecycle-states+)))
    (when (and (member :suspended stst)
               (not (member func *suspended-listeners*)))
      (push func *suspended-listeners*))
    (when (and (member :active stst)
               (not (member func *active-listeners*)))
      (push func *active-listeners*))
    (when (and (member :shutting-down stst)
               (not (member func *shutting-down-listeners*)))
      (push func *shutting-down-listeners*))))

(defun+ stop-listening-to-lifecycle-changes (func)
  (setf *suspended-listeners* (remove func *suspended-listeners*)
        *active-listeners* (remove func *active-listeners*)
        *shutting-down-listeners* (remove func *shutting-down-listeners*)))

(defun+ call-listeners ()
  (ecase *lifecycle-state*
    (:suspended
     (loop :for l :in *suspended-listeners* :do
        (if (symbolp l)
            (funcall (symbol-function l) :suspended)
            (funcall l :suspended))))
    (:active
     (loop :for l :in *active-listeners* :do
        (if (symbolp l)
            (funcall (symbol-function l) :active)
            (funcall l :active))))
    (:shutting-down
     (loop :for l :in *shutting-down-listeners* :do
        (if (symbolp l)
            (funcall (symbol-function l) :shutting-down)
            (funcall l :shutting-down))))))


;;----------------------------------------------------------------------
;; state logic

(defun+ change-state (target-state)
  (ecase *lifecycle-state*
    (:uninitialized (from-unitialized target-state))
    (:suspended (from-suspended target-state))
    (:active (from-active target-state))))

(defun+ from-unitialized (target-state)
  (assert (uninitialized-p))
  (ecase target-state
    (:suspended (to-suspended target-state))
    (:active (to-suspended target-state))))

(defun+ from-suspended (target-state)
  (assert (suspended-p))
  (ecase target-state
    (:active (to-active target-state))
    (:shutting-down (to-shutting-down))))

(defun+ from-active (target-state)
  (assert (active-p))
  (ecase target-state
    (:suspended (to-suspended target-state))
    (:shutting-down (to-suspended target-state))))

(defun+ to-suspended (target-state)
  (setf *lifecycle-state* :suspended)
  (call-listeners)
  (when (not (eq *lifecycle-state* target-state))
    (change-state target-state)))

(defun+ to-active (target-state)
  (setf *lifecycle-state* :active)
  (call-listeners)
  (when (not (eq *lifecycle-state* target-state))
    (change-state target-state)))

(defun+ to-shutting-down ()
  (setf *lifecycle-state* :shutting-down)
  (call-listeners)
  ;; do shutdown stuff
  (cepl.host::%shutdown cepl.host::*current-host*)
  ;; go back to uninitialized
  (setf *lifecycle-state* :uninitialized))

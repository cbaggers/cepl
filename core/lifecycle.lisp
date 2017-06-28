(in-package :cepl.lifecycle)

;; we use very little of the lifecyle right now (only :uninitialized
;; :interactive & :shutting-down) but it's here for completeness

;; This is not related to 'focus' or whether the window is minimized/maximized
;; which are a seperate matters

;; we should expose this to cepl.host at some point, but it's not urgent

(defvar *lifecycle-state* :uninitialized)

(defvar *lifecycle-states*
  '(:uninitialized
    :suspended
    :active
    :interactive
    :shutting-down))

;;----------------------------------------------------------------------
;; inspect

(defun2 uninitialized-p ()
  (eq *lifecycle-state* :uninitialized))

(defun2 suspended-p ()
  (eq *lifecycle-state* :suspended))

(defun2 active-p ()
  (eq *lifecycle-state* :active))

(defun2 interactive-p ()
  (eq *lifecycle-state* :interactive))

(defun2 shutting-down-p ()
  (eq *lifecycle-state* :shutting-down))

;;----------------------------------------------------------------------
;; listeners

(let (suspended
      active
      interactive
      shutting-down)
  ;;
  (defun2 listen-to-lifecycle-changes (func &rest states-to-subscribe-to)
    (assert (every (lambda (x) (member x *lifecycle-states*))
                   states-to-subscribe-to))
    (let ((stst (or states-to-subscribe-to *lifecycle-states*)))
      (when (and (member :suspended stst)
                 (not (member func suspended)))
        (push func suspended))
      (when (and (member :foregroun stst)
                 (not (member func active)))
        (push func active))
      (when (and (member :interactive stst)
                 (not (member func interactive)))
        (push func interactive))
      (when (and (member :shutting-down stst)
                 (not (member func shutting-down)))
        (push func shutting-down))))
  ;;
  (defun2 stop-listening-to-lifecycle-changes (func)
    (setf suspended (remove func suspended)
          active (remove func active)
          interactive (remove func interactive)
          shutting-down (remove func shutting-down)))
  ;;
  (defun2 call-listeners ()
    (ecase *lifecycle-state*
      (:suspended
       (loop :for l :in suspended :do
          (if (symbolp l)
              (funcall (symbol-function l) :suspended)
              (funcall l :suspended))))
      (:active
       (loop :for l :in active :do
          (if (symbolp l)
              (funcall (symbol-function l) :active)
              (funcall l :active))))
      (:interactive
       (loop :for l :in interactive :do
          (if (symbolp l)
              (funcall (symbol-function l) :interactive)
              (funcall l :interactive))))
      (:shutting-down
       (loop :for l :in shutting-down :do
          (if (symbolp l)
              (funcall (symbol-function l) :shutting-down)
              (funcall l :shutting-down)))))))


;;----------------------------------------------------------------------
;; state logic

(defun2 change-state (target-state)
  (ecase *lifecycle-state*
    (:uninitialized (from-unitialized target-state))
    (:suspended (from-suspended target-state))
    (:active (from-active target-state))
    (:interactive (from-interactive target-state))))

(defun2 from-unitialized (target-state)
  (assert (uninitialized-p))
  (ecase target-state
    (:suspended (to-suspended target-state))
    (:active (to-suspended target-state))
    (:interactive (to-suspended target-state))))

(defun2 from-suspended (target-state)
  (assert (suspended-p))
  (ecase target-state
    (:active (to-active target-state))
    (:interactive (to-active target-state))
    (:shutting-down (to-shutting-down))))

(defun2 from-active (target-state)
  (assert (active-p))
  (ecase target-state
    (:suspended (to-suspended target-state))
    (:interactive (to-interactive target-state))
    (:shutting-down (to-suspended target-state))))

(defun2 from-interactive (target-state)
  (assert (interactive-p))
  (ecase target-state
    (:suspended (to-active target-state))
    (:active (to-active target-state))
    (:shutting-down (to-active target-state))))


(defun2 to-suspended (target-state)
  (setf *lifecycle-state* :suspended)
  (call-listeners)
  (when (not (eq *lifecycle-state* target-state))
    (change-state target-state)))

(defun2 to-active (target-state)
  (setf *lifecycle-state* :active)
  (call-listeners)
  (when (not (eq *lifecycle-state* target-state))
    (change-state target-state)))

(defun2 to-interactive (target-state)
  (setf *lifecycle-state* :interactive)
  (call-listeners)
  (when (not (eq *lifecycle-state* target-state))
    (change-state target-state)))

(defun2 to-shutting-down ()
  (setf *lifecycle-state* :shutting-down)
  (call-listeners)
  ;; do shutdown stuff
  (cepl.host:shutdown)
  (setf cepl.context:*gl-context* nil)
  ;; go back to uninitialized
  (setf *lifecycle-state* :uninitialized))

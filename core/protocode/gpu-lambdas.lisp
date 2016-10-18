
(defun-g foo ((vec :vec4))
  (values vec vec))

(lambda-g ((vec :vec4))
  (values vec vec))

(defmacro lambda-g (args &body body)
  (make-instance 'gpu-lambda :args args :body body))

(defclass gpu-lambda ()
  ((args :initarg :args)
   (body :initarg :body)))

(defmacro g-> (context &body gpipe-args))

(g-> ()
  (lambda-g ((vec :vec4))
    (values vec (s~ vec :xy)))
  (lambda-g ((tc :vec2) &uniform tex)
    (texture tex tc)))

(g-> ()
  (lambda-g ((vec :vec4))
    (values vec (s~ vec :xy)))
  (lambda-g ((tc :vec2) &uniform tex)
    (texture tex tc)))

;; Even new pipeline needs to be object as:
;;
;;    (g-> a b)
;;
;; is gonna have totally different signatures each
;; time a or b change

;;; OK IT'S DECIDED

;; lambda pipelines are snapshots and don't participate in live reloading
;; it's a bummer but neccesary. A regular lambda that calls a regular function
;; breaks too if the function signature changes.

;; Counter Argument: but not if the signature doesnt change.

;; Ok so we accept the first tenet but not when the disconnect should happen.
;; My argument would be it needs to be consistent, whatever the behaviour.

;;------------------------------------------------------------
;; Funcallable object
(defclass constructor ()
     ((name :initarg :name :accessor constructor-name)
      (fields :initarg :fields :accessor constructor-fields))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((c constructor) &key)
  (with-slots (name fields) c
    (set-funcallable-instance-function
      c
      #'(lambda ()
          (let ((new (make-array (1+ (length fields)))))
            (setf (aref new 0) name)
            new)))))

(setq c1 (make-instance 'constructor
                        :name 'position :fields '(x y)))


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

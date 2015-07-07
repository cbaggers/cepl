
;; rather than interfaces make proxies

;; a proxy is defined as a list of functions the object must adhere to
;; a proxy is then a struct that has a field for the object and then one
;; for each function

(defgeneric defproxy-impl (proxy-name target-type-name))

(defstruct proxy
  (target (error "All proxies must be initialized with a target") :type t))

(defmacro defproxy (name &body func-descriptions)
  `(progn
     (defstruct (,(symb name 'proxy) (:include proxy) (:conc-name %prxy-))
       ,@(loop :for fd :in func-descriptions :collect
            `(,(first fd) (error ,(format nil "Method ~a for Proxy ~a must be provided"
                                          name (first fd)))
               :type function)))

     ,@(loop :for fd :in func-descriptions :collect
          (let ((slot-name (symb '%prxy- (first fd))))
            `(defun ,(first fd) (prxy ,@(rest fd))
               (let ((target (proxy-target prxy)))
                 (funcall (,slot-name target) target ,@(rest fd))))))))

;; def-proxy-impl is a macro that lets you define a implementation for a proxy
;; for a given type
;; you can then use (make-*-proxy x) where * is the type (proxy *) which is
;; generic and specialized on the type.

(def-proxy-impl (printable node)
    :print #'print-node)

(defmacro def-proxy-impl ((proxy-type target-type) &body funcs)
  `(progn
     (defun ,(symb 'make- target-type '-proxy) (x)
       (,(symb 'make- proxy-type)
         ,@funcs))
     (defmethod proxy ((x ,target-type) (p (eql ',proxy-type)))
       (declare (ignore p))
       (,(symb 'make- target-type '-proxy) x))))

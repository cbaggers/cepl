(in-package :cepl)

;; ugly but threw in together in 10 mins so meh!
(defmacro defun-static (name args -> returns &body body)
  (assert (string-equal :-> ->))
  (assert (every (lambda (x) (or (listp x)
                                 (and (symbolp x)
                                      (char= #\& (elt (symbol-name x) 0)))))
                 args))
  (let ((tmp-props (if (listp name) name (list name))))
    (destructuring-bind (name &rest func-properties) tmp-props
      (labels ((is (x) (member x func-properties))
               (last1 (x) (car (last x)))
               (get-args (x))
               (get-types (x)))
        (let* ((types (loop for i in args append (when (listp i) (list (last1 i)))))
               (decls (loop for i in args collect
                           (when (listp i) (list (last1 i) (first i)))))
               (args (loop for i in args collect (if (listp i)
                                                     (let ((tmp (butlast i)))
                                                       (if (= 1 (length tmp))
                                                           (first tmp)
                                                           tmp))
                                                     i)))
               (doc? (when (stringp (first body)) (first body)))
               (declares (subseq body (if doc? 1 0)
                                 (max (if doc? 1 0)
                                      (position-if-not
                                       (lambda (x)
                                         (and (listp x)
                                              (eq 'declare (first x))))
                                       body))))
               (body (subseq body (+ (if doc? 1 0)
                                     (length declares)))))
          `(progn
             (declaim ,@(when (is :inline) `((inline ,name)))
                      (ftype (function ,types ,returns) ,name))
             (defun ,name ,args
               ,@(when doc? (list doc?))
               (declare ,@decls)
               ,@declares
               ,@body)))))))

;; (defun-static make-vector4
;;     ((x single-float) (y single-float)
;;      (z single-float) (w single-float)) -> (simple-array single-float (4))
;;   "This takes 4 floats and give back a vector4, this is just an
;;    array but it specifies the array type and populates it.
;;    For speed reasons it will not accept integers so make sure
;;    you hand it floats."
;;   (declare (optimize (speed 3) (safety 0)))
;;   (let ((vec (make-array 4 :element-type `single-float)))
;;     (setf (aref vec 0) x
;; 	  (aref vec 1) y
;; 	  (aref vec 2) z
;; 	  (aref vec 3) w)
;;     vec))

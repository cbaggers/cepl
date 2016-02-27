;; range

(defun range (x &optional y z u v)
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :below end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :above end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :below end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :above end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null) (if (> x 0) (basic 0 x) (basic-down 0 x)))
        (number (if (or (null z) (keywordp z))
                    (if (> y x) (basic x y) (basic-down x y))
                    (if (> y x) (fun x y z) (fun-down x y z))))
        (function (if (> x 0) (fun 0 x y) (fun-down 0 x y)))))))

(defun rangei (x &optional y z u v)
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :upto end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :downto end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :upto end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :downto end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null) (if (> x 0) (basic 0 x) (basic-down 0 x)))
        (number (if (or (null z) (keywordp z))
                    (if (> y x) (basic x y) (basic-down x y))
                    (if (> y x) (fun x y z) (fun-down x y z))))
        (function (if (> x 0) (fun 0 x y) (fun-down 0 x y)))))))

(defun arange (x &optional y z u v)  
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :below end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :above end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :below end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :above end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null) 
         (make-array x :initial-contents
                     (if (> x 0) (basic 0 x) (basic-down 0 x))))
        (number (make-array (abs (- y x))
                            :initial-contents
                            (if (or (null z) (keywordp z))
                                (if (> y x) (basic x y) (basic-down x y))
                                (if (> y x) (fun x y z) (fun-down x y z)))))
        (function (make-array x :initial-contents
                              (if (> x 0) (fun 0 x y) (fun-down 0 x y))))))))

(defun arangei (x &optional y z u v)  
  (let ((step (or (and (eq y :step) z)
                  (and (eq z :step) u)
                  (and (eq u :step) v)
                  1)))
    (labels ((basic (start end) (loop :for i :from start :upto end
                                   :by step :collect i))
             (basic-down (start end) (loop :for i :from start :downto end
                                        :by step :collect i))
             (fun (start end fun) (loop :for i :from start :upto end
                                     :by step :collect (funcall fun i)))
             (fun-down (start end fun) (loop :for i :from start :downto end
                                          :by step :collect (funcall fun i))))
      (typecase y
        ((or symbol null) 
         (make-array (1+ x) :initial-contents
                     (if (> x 0) (basic 0 x) (basic-down 0 x))))
        (number (make-array (1+ (abs (- y x)))
                            :initial-contents
                            (if (or (null z) (keywordp z))
                                (if (> y x) (basic x y) (basic-down x y))
                                (if (> y x) (fun x y z) (fun-down x y z)))))
        (function (make-array (1+ x) :initial-contents
                              (if (> x 0) (fun 0 x y) (fun-down 0 x y))))))))


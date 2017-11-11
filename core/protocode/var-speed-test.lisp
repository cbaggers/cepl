;;

(declaim
 (ftype (function (single-float single-float single-float) single-float)
        top-level-closure)
 (notinline top-level-closure))
(let ((the-id 0))
  (declare (type (signed-byte 32) the-id))
  (defun top-level-closure (x y z)
    (declare (type single-float x y z)
             (optimize (speed 3) (safety 1) (debug 1)))
    (when (= z 987654321.0)
      (setf the-id 10))
    (+ the-id (* x (/ y z)))))


(declaim
 (ftype (function (single-float single-float single-float) single-float)
        var-based-id)
 (notinline var-based-id)
 (type (signed-byte 32) *the-id-2*))
(defvar *the-id-2* 0)
(defun var-based-id (x y z)
  (declare (type single-float x y z)
           (optimize (speed 3) (safety 1) (debug 1)))
  (when (= z 987654321.0)
    (setf *the-id-2* 10))
  (+ *the-id-2* (* x (/ y z))))

(declaim
 (ftype (function (single-float single-float single-float) single-float)
        top-level-closure-3)
 (notinline top-level-closure-3))
(let ((the-id 0)
      (another-id 0)
      (yet-more-id 0))
  (declare (type (signed-byte 32) the-id))
  (defun top-level-closure-3 (x y z)
    (declare (type single-float x y z)
             (optimize (speed 3) (safety 1) (debug 1)))
    (when (= z 987654321.0)
      (setf the-id 10)
      (setf another-id 10)
      (setf yet-more-id 10))
    (+ the-id another-id yet-more-id (* x (/ y z)))))


(declaim
 (ftype (function (single-float single-float single-float) single-float)
        var-based-id-3)
 (notinline var-based-id-3)
 (type (signed-byte 32) *the-id-2-3* *another-id-2-3* *yet-more-id-2-3*))
(defvar *the-id-2-3* 0)
(defvar *another-id-2-3* 0)
(defvar *yet-more-id-2-3* 0)
(defun var-based-id-3 (x y z)
  (declare (type single-float x y z)
           (optimize (speed 3) (safety 1) (debug 1)))
  (when (= z 987654321.0)
    (setf *the-id-2-3* 10)
    (setf *another-id-2-3* 10)
    (setf *yet-more-id-2-3* 10))
  (+ *the-id-2-3* *another-id-2-3* *yet-more-id-2-3* (* x (/ y z))))


(defstruct ids
  (the 0 :type (signed-byte 32))
  (another 0 :type (signed-byte 32))
  (yet-more 0 :type (signed-byte 32)))
(declaim
 (ftype (function (single-float single-float single-float) single-float)
        var-based-id-3)
 (notinline var-based-id-3)
 (type ids *ids*))
(defvar *ids* (make-ids))
(defun var-based-packed (x y z)
  (declare (type single-float x y z)
           (optimize (speed 3) (safety 1) (debug 1))
           (inline ids-the ids-another ids-yet-more))
  (let ((ids *ids*))
    (declare (type ids ids))
    (when (= z 987654321.0)
      (setf (ids-the ids) 10)
      (setf (ids-another ids) 10)
      (setf (ids-yet-more ids) 10))
    (+ (ids-the ids) (ids-another ids) (ids-yet-more ids) (* x (/ y z)))))

(defun make-circle-of-n-floats (n)
  (let ((r (loop :for i :below n :collect (random 10000f0))))
    (setf (cdr (last r)) r)
    r))


(defun test ()
  (let ((data (make-circle-of-n-floats 1000))
        (dummy nil))
    (print "top")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (top-level-closure a b c))))
    (print dummy)
    (print "var")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (var-based-id a b c))))
    (print dummy)
    (print "top")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (top-level-closure a b c))))
    (print dummy)
    (print "var")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (var-based-id a b c))))
    (print dummy)
    (print "top")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (top-level-closure-3 a b c))))
    (print dummy)
    (print "var")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (var-based-id-3 a b c))))
    (print dummy)
    (print "packed")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (var-based-packed a b c))))
    (print dummy)
    (print "top")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (top-level-closure-3 a b c))))
    (print dummy)
    (print "var")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (var-based-id-3 a b c))))
    (print dummy)
    (print "packed")
    (time
     (loop :for i :below 100000000 :for (a b c) :on data :do
        (setf dummy (var-based-packed a b c))))
    (print dummy)))


;; so if we take the difference between the closure and var
;;
;; (- 0.930 0.864) -> 0.065999985
;;
;; and then assume 5000 drawcalls a frame
;;
;; (/ 100000000 5000) -> 20000
;;
;; then we are saving 0.065999985 over 20000 frames
;;
;; that is (/ (- 0.930 0.864) 20000) -> 3.2999992e-6 per frame
;;
;; So 3 microseconds a frame cost over the closure approach
;;
;; Oh but wait! the packed approach is way better!
;; the gain is bigger over the closure that the cost of the seperate vars
;; awesome

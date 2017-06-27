(in-package :cepl)

(defn tester ((a single-float)) single-float
  (declare (profile t))
  (* a 10))

(defun test-it ()
  (loop :for i :below 100 :do
     (tester (float i 0f0))))

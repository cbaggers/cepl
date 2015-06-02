(defstruct slice
  (start 0)
  (end :end)
  (step 1))

(defun slice (&optional (start 0) end (step 1) sequence)
  (if sequence
      (%perform-slice start end step sequence)
      (make-slice :start start :end (or end :end) :step step)))

(defun %perform-slice (start end step sequence)
  )

(defun range (num-or-slice)
  )

(in-package :cepl.space)

;;----------------------------------------------------------------------
;; ancestor walking function

(declaim (ftype (function ((function (t vec-space) t) vec-space t)
                          t)
                %reduce-ancestors))
(defun %reduce-ancestors (function of-space initial-value)
  (declare (type vec-space of-space)
           (type (function (t vec-space) t) function)
           (optimize (speed 3) (safety 1) (debug 1)))
  (labels ((walk (accum space)
             (if space
                 (walk (funcall function accum space)
                       (%space-parent space))
                 accum)))
    (walk initial-value of-space)))

(declaim (ftype (function ((function (t vec-space) t)
                           vec-space
                           vec-space
                           t)
                          t)
                %reduce-ancestors-until-space))
(defun %reduce-ancestors-until-space (function of-space until-space
                                      initial-value)
  (declare (type vec-space of-space until-space)
           (type (function (t vec-space) t) function)
           (optimize (speed 3) (safety 1) (debug 1)))
  (assert (typep until-space 'vec-space))
  (labels ((walk (accum space)
             (cond ((eq space until-space) accum)
                   ((null space)
                    (error 'not-ancestor
                           :ancestor-space until-space
                           :start-space of-space))
                   (t (walk (funcall function accum space)
                            (%space-parent space))))))
    (walk initial-value of-space)))

(defun reduce-ancestors (function of-space &optional until-space initial-value)
  (labels ((walk (accum space)
             (cond ((eq space until-space) accum)
                   ((null space) (if until-space
                                     (error 'not-ancestor
                                            :ancestor-space until-space
                                            :start-space of-space)
                                     accum))
                   (t (walk (funcall function accum space)
                            (%space-parent space))))))
    (walk initial-value of-space)))

(define-compiler-macro reduce-ancestors
    (function of-space &optional (until-space nil supplied) initial-value)
  (if (and supplied (not (null until-space)))
      `(%reduce-ancestors-until-space ,function ,of-space ,until-space
                                      ,initial-value)
      `(%reduce-ancestors ,function ,of-space ,initial-value)))

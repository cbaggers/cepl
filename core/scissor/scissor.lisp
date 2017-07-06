(in-package :cepl.scissor)

;;------------------------------------------------------------
;; Scissor Viewport

(define-context-func scissor-viewport (&optional (index (unsigned-byte 8) 0))
    (or null viewport)
    (current-scissor-viewports gl-version-float)
  (when (< gl-version-float 4.1)
    (assert (= index 0)))
  (aref current-scissor-viewports index))


(define-context-func (setf scissor-viewport)
    ((viewport (or null viewport))
     &optional (index (unsigned-byte 8) 0))
    (or null viewport)
    (current-scissor-viewports gl-version-float)
  (declare not-inline-internals)
  (let ((current (aref current-scissor-viewports index)))
      (unless (eq current viewport)
        (if (>= gl-version-float 4.1)
            (if viewport
                (progn
                  (unless current
                    (%gl:enable-i :scissor-test index))
                  (%gl:scissor-indexed index
                                       (%viewport-origin-x viewport)
                                       (%viewport-origin-y viewport)
                                       (%viewport-resolution-x viewport)
                                       (%viewport-resolution-y viewport)))
                (when current
                  (%gl:disable-i :scissor-test index)))

            ;; If gl-version < 4.1
            (progn
              (assert (= index 0))
              (if viewport
                  (progn
                    (unless current
                      (%gl:enable :scissor-test))
                    (gl:scissor (%viewport-origin-x viewport)
                                (%viewport-origin-y viewport)
                                (%viewport-resolution-x viewport)
                                (%viewport-resolution-y viewport)))
                  (when current
                    (%gl:disable :scissor-test)))))
        (setf (aref current-scissor-viewports index) viewport)))
  viewport)

;;------------------------------------------------------------

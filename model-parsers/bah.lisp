
;; grr need to just gen basic models

;; (defun plane (width height 
;;               &key (color (v! 1 0 1)) (normal (v! 0 0 1))
;;                 (horizontal-segments 1) (vertical-segments 1)
;;                 (bottom (- (/ height 2.0))) (left (- (/ width 2.0)))
;;                 (index-offset 0))
;;   (if (or (< horizontal-segments 1) (< vertical-segments 1))
;;       (error "too few segments")
;;       (let ((h-seg-length (/ width horizontal-segments))
;;             (v-seg-length (/ height vertical-segments)))
;;         (let ((points (loop for x below horizontal-segments
;;                          for y below vertical-segments
;;                          collect (list (v! (+ left (* x h-seg-length))
;;                                            (+ bottom (* y v-seg-length))
;;                                            0) normal color)))
;;               (faces (loop :for x :below horizontal-segments
;;                         :with offset = index-offset
;;                         :append (loop :for y :below vertical-segments
;;                                    :append
;;                                    (list offset
;;                                          (+ 1 offset)
;;                                          (+ offset (+ vertical-segments 1))
;;                                          (+ 1 offset)
;;                                          (+ 1 offset (+ vertical-segments 1))
;;                                          (+ offset (+ vertical-segments 1)))
;;                                    :do (setf offset (+ 1 offset)))
;;                         :do (setf offset (+ 1 offset)))))
;;           (list points faces)))))

;; (defun box (width height depth &key (offset (v! 0 0 0)) (color (v! 1 0 1)))
;;   (let ((width (/ width 2.0))
;;         (height (/ height 2.0))
;;         (depth (/ depth 2.0)))
;;     (let (points (list 
;;                   (list (v3:v+ (v! -width height -depth) offset) 
;;                         (v! 0 1 0)
;;                         color)
;;                    (list (v3:v+ (v! -width height -depth) offset) 
;;                          (v! 0 1 0)
;;                          color)
;;                    (list (v3:v+ (v! -width height -depth) offset) 
;;                          (v! 0 1 0)
;;                          color)
;;                    (list (v3:v+ (v! -width height -depth) offset) 
;;                          (v! 0 1 0)
;;                          color))))))

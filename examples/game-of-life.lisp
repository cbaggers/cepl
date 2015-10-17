(in-package :cepl)

(defvar field-a nil)
(defvar field-b nil)

(defun-g pass-through-vert ((vert g-pt))
  (values (v! (pos vert) 1) (tex vert)))

(defun-g pass-through-frag ((tc :vec2) &uniform (board :sampler-2d))
  (let ((c (x (texture board tc))))
    (v! c 0 c 0)))

(defun-g the-meat! ((tc :vec2) &uniform (board :sampler-2d))
  (let* ((offset (/ 1.0 1024.0))
         (score (x (+ (texture board (+ tc (v! (- offset) (- offset))))
                      (texture board (+ tc (v! (- offset)  0)))
                      (texture board (+ tc (v! (- offset)  offset)))
                      (texture board (+ tc (v!  0 (- offset))))
                      (texture board (+ tc (v!  0  offset)))
                      (texture board (+ tc (v!  offset (- offset))))
                      (texture board (+ tc (v!  offset  0)))
                      (texture board (+ tc (v!  offset  offset))))))
         (current (texture board tc)))
    (cond
      ((or (< score 2) (> score 3)) (v! 0 0 0 0))
      ((and (= score 3) (= (x current) 0)) (v! 1 0 0 0))
      (t current))))

(defpipeline copy-pass () (g-> #'pass-through-vert #'pass-through-frag))
(defpipeline gol-pass () (g-> #'pass-through-vert #'the-meat!))

(defpipeline gol-render (fbo-current fbo-new)
    (g-> (fbo-new (gol-pass *quad-stream* :board (attachment fbo-current 0)))
         (nil (copy-pass *quad-stream* :board (attachment fbo-new 0)))))

(let ((flip-flop t))
  (defun game-o-life ()
    (if (setf flip-flop (not flip-flop))
        (map-g #'gol-render field-b field-a)
        (map-g #'gol-render field-a field-b))))

(defun init_ ()
  (let ((a (make-c-array
            (loop for i below 1024 collect
                 (loop for i below 1024 collect (if (= 1 (random 5))
                                                    (v!ubyte 255 0 0 0)
                                                    (v!ubyte 0 0 0 0))))
            :dimensions '(1024 1024) :element-type :ubyte-vec4)))
    (setf field-a (make-fbo `(:c ,(make-texture a)))
          field-b (make-fbo `(:c ,(make-texture a))))))

(defun step-main ()
  (clear)
  (game-o-life)
  (cgl:update-display))

;; this macro is a lazy way to get a function called on start (:init)
;; and a function called every
(live:main-loop :init init_ :step step-main)

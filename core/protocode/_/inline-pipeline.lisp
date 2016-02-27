;; could we have inlien shader code?

;; this is original
(defpipeline prog-1 ((vert vert-data))
  (:vertex (setf gl-position (pos vert))
           (out (the-color :smooth) (col vert)))
  (:fragment (let ((lerp-value (/ (y gl-frag-coord) 500.0)))
               (out outputColor (mix the-color
                                     (v! 0.2 0.2 0.2 1.0)
                                     lerp-value)))))

;; so we we can infer the arguments using stemcells right?
;; hmm how do we infer vertex data?

(draw (&optional vertex-data)
  )

;; so if I just want to blat pixels to the screen I leave out the vertex data

(draw (&optional vertex-data)
  )

;; I want to be able to provide vertex data but have it passed through
;; unchanged for the frag shader... but this assumes I have some knowledge
;; of what part of the source data is position or color etc etc

;; Could the vertex stage be nmerged with the fragment stage?

(defpipeline+ prog-1 ((vert vert-data))
  (across :triangle (pos vert)
    (mix (smoothly (col vert))
         (v! 0.2 0.2 0.2 1.0)
         (/ (y gl-frag-pos) 500.0))))

;; inline this

(loop :while running :do
       (case-events (event) (:quit () (setf running nil)))
       (update-swank)
       (continuable
        (gl:clear :color-buffer-bit)
        (render
         (across :triangle (pos vert)
          (mix (smoothly (col vert))
               (v! 0.2 0.2 0.2 1.0)
               (/ (y gl-frag-pos) 500.0))))
        (gl:flush)
        (jungl:swap)))


;; what about

(let ((c (across-prim (col vert) :smooth)))
  )

;; this defines a value interpolated across the primitive. This basically means
;; set it as an output of the vertex shader
;; maybe add let-ap

(let-ap ((c (col vert) :smooth)
         (p (pos vert)))
  )
;; which expands to the let across-prim


(defpipeline+ prog-1 ((vert vert-data))
  (let-ap ((c (col vert) :smooth)
           (p (pos vert)))
    (mix c (v! 0.2 0.2 0.2 1.0) (/ (y p) 500.0))))
;; last output of last form is color output

;; is equivilent to

(defpipeline prog-1 ((vert vert-data))
  (:vertex (setf gl-position (pos vert))
           (out (the-color :smooth) (col vert)))
  (:fragment (let ((lerp-value (/ (y gl-frag-coord) 500.0)))
               (out outputColor (mix the-color
                                     (v! 0.2 0.2 0.2 1.0)
                                     lerp-value)))))

;; if I have a combined language then I really dont want setf confusing things.
;; auto lifting of shader expressions requires knowledge of how things can
;; interpolate, what their cost is, and how frequently their argument change
;; (eg every compile/execution/vertex/fragment).

;; The reason for needing cost is that glsl puts a limit on the number of data
;; items that can be passed between stages. If the number of expressions that
;; can be lifted to the vertex shader exceeds the number of slots available then
;; we need to be able to make the best choice on what is lifted.

;; does varjo know if something is constant? nope. Actually varjo is a bit
;; broken around this and we should read the todo fiel for more info

;;
;; inline pipeline will work if we can specify the name (the number) of the
;; glsl-program when we compile it. This means we could replace the inline
;; form with (run-program n) where n is decided at macro expansion time, we save
;; the form in a dictioanry against n and then compiel it when the context is
;; created.

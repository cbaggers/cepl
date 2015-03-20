(let ((2nd (make-fbo )))
  (defun do-it (verts fbo t1)
    (g-> fbo
         (1st (first-pass verts) )
         (2nd (second-pass verts :tex t1))
         (nil ())
         (third-pass *quad* :t1 (col 1st) :t2 (col 2nd)))))

;; kind of want 1st to be an exising fbo -OR- some local one
;; I guess this is why I got confused with defpipeline as I wanted some
;; way to have this done for me.

;; hey... each form could have a third arg...still cant make a closure without
;; owning the function

;; ok so before I go wrong again, on the walk home I decided that g-> would be
;; for composing pipelines unless inside defpipeline. In that case it was for
;; composing g-functions into pipelines.

;; Now I am thinking that if you use let syntax you are combining pipelines
;; so:

(defpipeline first-pass (g-> #'a #'b))
(defpipeline second-pass (g-> #'a #'c))
(defpipeline third-pass (g-> #'d #'e)) ;; this is composing g-funcs

(defpipeline compound
    (g-> (1st (first-pass stream)) ;; and this is composing pipelines
         (2nd (second-pass stream))
         (third-pass quad :t1 (col 1st) :t2 (col 2nd))))

;; need a way of indicating where the fbos are 'from'. Meaning if they
;; are in a closure over the pipeline or are passed in as arguments

;; third arg of let form could indicate the 'from' part:
;; t meaning create a default fbo
;; or just make fbo args... cool

(defpipeline compound
    (g-> (1st (first-pass stream) t) ;; and this is composing pipelines
         (2nd (second-pass stream) (:c (v! 10 20)))
         (third-pass quad :t1 (col 1st) :t2 (col 2nd))))

;; or use :resources

(defpipeline compound
    (g-> (1st (first-pass stream) t) ;; and this is composing pipelines
         (2nd (second-pass stream))
         (third-pass quad :t1 (col 1st) :t2 (col 2nd)))
  :resources
  (2nd (make-fbo :c (v! 10 20))))

;; may have to be specific so I can assume types

(defpipeline compound (stream)
    (g-> (1st (first-pass stream)) ;; and this is composing pipelines
         (2nd (second-pass stream))
         (third-pass quad :t1 (col 1st) :t2 (col 2nd)))
  :fbo (1st :c :d) (2nd :c (v! 10 20)))

;; thats actually very nice


(with-bind-fbo (*geom-fbo*)
  (gmap #'first-pass (gstream *wibble*)
        :model-space-light-pos (v:s~ cam-light-vec :xyz)
        :light-intensity (v! 1 1 1 0)
        :model-to-cam model-to-cam-matrix
        ;; :normal-model-to-cam normal-to-cam-matrix
        :ambient-intensity (v! 0.2 0.2 0.2 1.0)
        :textur *wib-tex*))
(with-fbo-slots (c) *geom-fbo*
  (gmap #'hmm-pass (gstream *bird*)
        :model-space-light-pos (v:s~ cam-light-vec :xyz)
        :light-intensity (v! 1 1 1 0)
        :model-to-cam model-to-cam-matrix
        ;; :normal-model-to-cam normal-to-cam-matrix
        :ambient-intensity (v! 0.2 0.2 0.2 1.0)
        :textur *bird-tex* :fbo-tex (slot-value c 'cgl::texture)
        :bird-tex *bird-tex2*
        :loop *loop-pos*))

;; A continuum is a generalization of sequence.

;; Sequence implies finite steps, continuum need not have that.

;; continuum is sampled to get data at a point.

;; Can be used for curves/waves etc

;; continuum can be:
;; - function based e.g. #'sin
;; - sequence based with no sampler '(1 2 3 4) essentially becomes a square wave
;; - sequence based with sampler ('(1 2 3 4) #'lerp)

;; actually the no sampler option is really just ('(1 2 3 4) #'floor).. and the
;; first can just has a nil sequence. so it becomes:

;; - (nil #'sin)
;; - ('(1 2 3 4) #'floor)
;; - ('(1 2 3 4) #'lerp)

;; will be a struct with bit flag to indicate if sequence based or not
;; (for optimization)

;; need lots of helpers for sampling, summing and other sequence based stuff

;; extracting a sequence is sampling at intervals
;; has-natural-sample-point is T if sequence backed
;; naturally-sample returns sequence backing continuum
;; .. maybe

;; continuums have start and end defined in term of their domain
;; so generally this will be a number in |N| but may be in complex domain
;; seems dumb when it is just a function...
;; maybe this idea has no legs

;; it could be ok if we allow functions as args to continuum things
;; but then again a continuum is just a partially applied function

#'sin

(fn~ λ(elt '(1 2 3 4) (floor _)))

(fn~ λ(lerp (elt '(1 2 3 4) (floor _))
            (elt '(1 2 3 4) (1+ (floor _)))))

;; three parts, data, sample-transform, result transform

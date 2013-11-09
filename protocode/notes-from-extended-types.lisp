(defparameter test-type '(-> (-> 10) -> (:float 10)))

;; [TODO] is there anything else we may want from other slots?
;; (-> :int)
;; (-> (:int 10))
;; (-> -> (:int 10))
;; ((-> 10) :int)
;; ((-> slot-name) ())

;; (-> (-> 10) -> (:FLOAT 10))
;; what does it mean to have (-> 10) as the second item? which one do we pick?
;; seems that this should then spit out an enhanced array with the rest of the 
;; spec as its etype

;; it should be invalid to have any non pointer type is any pre-end position


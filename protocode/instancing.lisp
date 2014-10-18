



(some-call args)

(with-instancing stream
  (some-call args))

;; expand to 
(some-call args &instancing stream)

;; this means that the defshader macro needs to add the instancign info

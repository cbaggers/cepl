;; we should have some syntax in defpipeline for pattern matching
;; the basic idea is that often a simple cpu side flag may wish to 
;; change rendering path. If the pipelines are very similar (as in
;; combinations ofthe same shaders) then it seems crazy to have
;; different pipelines and force the selection mechanism to be in 
;; various places in the user code... so we have pattern matched
;; paths in defpipeline.

(defpipeline thing (stuff &uniforms a b c)
  (:> (path stuff)
      (1 #'vert-a #'frag-a)
      (2 #'vert-a #'frag-b)
      (t #'vert-z #'frag-z))

;; Would be cool to chain in other pipelines for multipass but that means 
;; implicitly using fbos...unless there is a clean way to make it explicit


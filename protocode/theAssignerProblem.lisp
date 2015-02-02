;; walk each path until you find an uncertainty
;; and uncertainty is any point where a variable is invloved as an index.
;; at that point you cant tell what contents will be accessed so we have
;; to assume that all is used.

;; Then we look at the varjo type. 
;; arrays: For each element create assigner (element may be struct)
;; struct: for each field create an assigner (field may be an array)

(defun make-assigners (uniform)
  )

;; (defglstruct ham (d :short) (e :short 3))
;; (defglstruct jam (a :float 3) (b :ham 2) (c :int))
;; uniforms: ((test jam 5))
;; paths: test[0].a test[2].b[1].d  test[i].b[0].e[i]

;; need: all of test[*]

;; We could be wrong, this is always a possibility when other compilers 
;; are ahead of ours (which will be most of the time!) So maybe we need
;; to do the stupid version, create an assigner for EVERY part of EVERY
;; element of the uniforms and then only use them if the uniform is given
;; AND there is a cached position. The positions still get set at runtime.
;; We have certainty of coverage. Complex uniforms balloon pipeline code
;; but it will all be hidden...it's not too basd actually this will 
;; probably be the safest place to start just so we can get moving on a 
;; working release again.

;; From glwiki: The maximum number of available locations within a single
;;              program is GL_MAX_UNIFORM_LOCATIONS, which will be at least
;;              1024 locations.

;; Stupid Allocator Generator
;; recursive walk

(defun create-assinger (simple-thing)
  (do the stuff))

(defun create-struct-assigner (thing)
  (loop :for field :in (fields thing) :collect
     (if (array thing)
         (create-array-assigner)
         (if struct 
             (create-struct-assigner field)
             (create-assinger field)))))


(cond ((null thing) nil)
      ((struct thing) ))

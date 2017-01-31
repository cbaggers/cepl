
(let ((cepl.pipelines::prog-id nil)
      (cepl.pipelines::implicit-uniform-upload-func
       #'cepl.pipelines::fallback-iuniform-func)
      (#:g1581 -1)
      (#:image-unit1582 -1)
      (#:g1583 -1))
  (declare (type function cepl.pipelines::implicit-uniform-upload-func))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cepl.pipelines::update-pipeline-spec
     (cepl.pipelines::make-pipeline-spec 'foo-pipeline
                                         (list
                                          (cepl.pipelines::new-func-key
                                           'foo-vert '(g-pt))
                                          (cepl.pipelines::new-func-key
                                           'foo-frag '(:vec2)))
                                         'nil)))
  (labels ((#:|init1584| ()
              (let ((cepl.pipelines::image-unit -1))
                (declare (ignorable cepl.pipelines::image-unit))
                (multiple-value-bind
                      (cepl.pipelines::compiled-stages
                       cepl.pipelines::new-prog-id)
                    (cepl.pipelines::%compile-link-and-upload
                     'foo-pipeline
                     (list
                      (cons :vertex
                            (cepl.pipelines::new-func-key 'foo-vert
                                                          '(g-pt)))
                      (cons :fragment
                            (cepl.pipelines::new-func-key 'foo-frag
                                                          '(:vec2)))))
                  (declare (ignorable cepl.pipelines::compiled-stages))
                  (setf cepl.pipelines::prog-id cepl.pipelines::new-prog-id)
                  (setf cepl.pipelines::implicit-uniform-upload-func
                        (or
                         (cepl.pipelines::%create-implicit-uniform-uploader
                          cepl.pipelines::compiled-stages)
                         #'cepl.pipelines::fallback-iuniform-func)))
                (setf #:g1581
                      (cl-opengl:get-uniform-location cepl.pipelines::prog-id
                                                      "tex"))
                (setf #:image-unit1582 (incf cepl.pipelines::image-unit))
                (setf #:g1583
                      (cl-opengl:get-uniform-location cepl.pipelines::prog-id
                                                      "depth"))
                (cepl.pipelines::%post-init nil)
                cepl.pipelines::prog-id)))
    (lambda (cepl.pipelines::mapg-context stream &key tex depth)
      (declare (ignore cepl.pipelines::mapg-context)
               (ignorable tex depth))
      (unless cepl.pipelines::prog-id
        (setf cepl.pipelines::prog-id (#:|init1584|))
        (unless cepl.pipelines::prog-id (return-from foo-pipeline)))
      (cepl.pipelines::use-program cepl.pipelines::prog-id)
      (let ((depth depth) (tex tex))
        (when tex
          (let ((cepl.pipelines::val tex))
            (when (>= #:g1581 0)
              (unless
                  (eq (%cepl.types:%sampler-type cepl.pipelines::val)
                      :sampler-2d)
                (error "incorrect type of sampler passed to shader"))
              (cepl.textures::active-texture-num #:image-unit1582)
              (cepl.textures::bind-texture
               (%cepl.types:%sampler-texture cepl.pipelines::val))
              (if cepl.samplers::*samplers-available*
                  (cl-opengl-bindings:bind-sampler #:image-unit1582
                                                   (%cepl.types:%sampler-id
                                                    cepl.pipelines::val))
                  (cepl.textures::fallback-sampler-set cepl.pipelines::val))
              (cepl.pipelines::uniform-sampler #:g1581 #:image-unit1582))))
        (when depth
          (let ((cepl.pipelines::val depth))
            (when (>= #:g1583 0)
              (cepl.pipelines::uniform-1f #:g1583 cepl.pipelines::val)))))
      (locally
          (declare (optimize (speed 3) (safety 1)))
        (funcall cepl.pipelines::implicit-uniform-upload-func
                 cepl.pipelines::prog-id))
      (when stream (cepl.pipelines::draw-expander stream :triangles))
      (cepl.pipelines::use-program 0)
      (cl-opengl-bindings:bind-sampler #:image-unit1582 0)
      stream)))

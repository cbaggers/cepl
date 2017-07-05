(in-package :cepl)

;; this
(with-temp-sampler ((tex sampler-a) (norm sampler-b) (spec sampler-a))
  (map-g #'pline stream :tex tex :norm norm :specular spec))

;; could be like with-slots and make symbols macros for this
(map-g #'pline stream
       :tex (sample tex sampler-a)
       :norm (sample norm sampler-b)
       :specular (sample spec sampler-a))

;; which means we have to work out what this means
(defmacro with-temp-sampler (bindings-pairs &body body)
  (let ((syms (loop for i in bindings-pairs collect (gensym))))
    `(let ,(loop :for (_ form) :in bindings-pairs :for g :in syms
              :collect `(,g ,form))
       (symbol-macrolet ,(loop :for g :in syms :for (name _) :in bindings-pairs
                            :do (assert (symbolp name))
                            :collect `(,name '(sample ,name ,g)))
         ,@body))))

;; calling glActiveTexture and then calling bindTexture is really
;; bind-texture-to-texture-unit. Make this.
;; - Is unbind bindTexture 0? Yes, it e.g. glBindTexture(GL_TEXTURE_2D, 0);
;;
;; So we can define with-bind-texture-to-texture-unit

(defmacro with-bind-texture-to-texture-unit* ((texture unit) &body body)
  (progn
    (bind-texture-to-texture-unit ,texture ,unit)
    ,@body
    (unbind-texture-from-texture-unit ,texture ,unit)))

;; unbind because of multiple texture type 'slots'


;;- - - - - - - - - - - - - - - - - - - -
;; Ok, I worked it out!
;; All textures will have a fixnum slot for sampler-id.
;; This will almost always be zero
;; with-temp-sampler will set the internal sampler value of the texture and set
;; it back to zero at the end of the block.
;;
;; With this we take the value into the dispatch functions and can use it.
;; BOOM, solved.


(defmacro with-temp-sampler (bindings-pairs &body body)
  (let* ((tex-syms (loop for i in bindings-pairs collect (gensym "texture")))
         (sampler-syms (loop for i in bindings-pairs collect (gensym "sampler")))
         (letting (loop for b in bindings-pairs
                     for ts in tex-syms
                     for ss in sampler-syms
                     append `((,ts ,(first b)) (,ss ,(second b)))))
         (setting (loop for ts in tex-syms
                     for ss in sampler-syms
                     collect `(setf (slot-value ,ts 'sampler-object-id)
                                   (%sampler-id ,ss))))
         (reverting (loop for ts in tex-syms
                       for ss in sampler-syms
                       collect `(setf (slot-value ,ts 'sampler-object-id) 0))))
    `(let ,letting
       ,@setting
       ,@body
       ,@reverting)))

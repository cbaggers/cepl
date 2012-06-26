
;; taking inspiration from structs
;; (defstruct entity 
;;   (pos (make-vector3 0.0 0.0 -20.0))
;;   (loop-angle 0.0)
;;   (scale 1.0))

;; could do where 3 is the length
;; (def-data-format vert-col-norm 
;;   (:name pos :type :float :length 3)
;;   (:name color :type :unsigned-char :length 3)
;;   (:name normal :type :float :length 3))

;; or like this
;; (def-data-format vert-col-norm 
;;   (:name pos :type :float :componants (x y z))
;;   (:name color :type :unsigned-char :componants (r g b))
;;   (:name normal :type :float :componants (nx ny nz)))
;; compnant names can clash? is this allowed?

;; [TODO] add option for sequential, default is interleaving
;; [TODO] Add option for tight-packing so it wont try and 
;;        optimize the memory layout.

(defun def-data-format (name &)
  )

(defun make-stream (buffer stream-format)
  )

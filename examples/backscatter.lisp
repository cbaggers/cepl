(in-package :cepl)

;; 'Backscatter' is nice little raycasting demo made by a good friend.
;; It also happens to be very easy to read so I'm going to be copying
;; at least some of it here as a learning exercise.

(defstruct-g material ()
  (additive-color :vec3)
  (diffuse-color :vec3)
  (specular-color :vec3)
  (specular-exponent :float)
  (background-ammount :float))

(defstruct-g scene-result ()
  (d :float)
  (material material))

(defun-g mix-materials ((mat-a material) (mat-b material) (amount :float))
  (make-material
   (mix (material-additive-color mat-a) (material-additive-color mat-b) amount)
   (mix (material-diffuse-color mat-a) (material-diffuse-color mat-b) amount)
   (mix (material-specular-color mat-a) (material-specular-color mat-b) amount)
   (mix (material-specular-exponent mat-a) (material-specular-exponent mat-b) amount)
   (mix (material-background-ammount mat-a) (material-background-ammount mat-b) amount)))

(defun-g rotate-x ((vec :vec3) (angle :float)))

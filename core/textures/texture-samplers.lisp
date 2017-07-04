(in-package :cepl.textures)

;; These only exist so the sampler objects can use them
;; they are neccesary in cases where the GL version is less that 3.3
;; In those cases proper sampler objects are not available and we have
;; to use our standins.
;;
;; We could allow using the methods below to set the sampling params on
;; the texture directly but this makes for a less consistant api with no
;; other benefit.

(defun+ (setf tex-lod-bias) (value texture)
  (%with-scratch-texture-bound texture
    (%gl:tex-parameter-f (texture-type texture) :texture-lod-bias value))
  texture)


(defun+ (setf tex-min-lod) (value texture)
  (%with-scratch-texture-bound texture
    (%gl:tex-parameter-f (texture-type texture) :texture-min-lod value))
  texture)


(defun+ (setf tex-max-lod) (value texture)
  (%with-scratch-texture-bound texture
    (%gl:tex-parameter-f (texture-type texture) :texture-max-lod value))
  texture)


(defun+ (setf tex-magnify-filter) (value texture)
  (assert (member value '(:linear :nearest)))
  (%with-scratch-texture-bound texture
    (%gl::tex-parameter-i (texture-type texture) :texture-mag-filter
                          (%gl::foreign-enum-value '%gl:enum value)))
  texture)


(defun+ (setf tex-minify-filter) (value texture)
  (%with-scratch-texture-bound texture
    (%gl::tex-parameter-i (texture-type texture) :texture-min-filter
                          (%gl::foreign-enum-value '%gl:enum value)))
  texture)

(defun+ (setf tex-wrap) (value texture)
  (let ((options '(:repeat :mirrored-repeat :clamp-to-edge :clamp-to-border
                   :mirror-clamp-to-edge))
        (value (if (keywordp value)
                   (vector value value value)
                   value)))
    (assert (and (vectorp value)
                 (= (length value) 3)
                 (every (lambda (x) (member x options)) value)))
    (%with-scratch-texture-bound texture
      (%gl::tex-parameter-i (texture-type texture) :texture-wrap-s
                            (%gl::foreign-enum-value '%gl:enum (aref value 0)))
      (%gl::tex-parameter-i (texture-type texture) :texture-wrap-t
                            (%gl::foreign-enum-value '%gl:enum (aref value 1)))
      (%gl::tex-parameter-i (texture-type texture) :texture-wrap-r
                            (%gl::foreign-enum-value '%gl:enum (aref value 2)))))
  texture)


(defun+ (setf tex-compare) (value texture)
  (%with-scratch-texture-bound texture
    (if value
        (progn
          (%gl:tex-parameter-i
           (texture-type texture) :texture-compare-mode
           (%gl::foreign-enum-value
            '%gl:enum :compare-ref-to-texture))
          (%gl:tex-parameter-i
           (texture-type texture) :texture-compare-func
           (%gl::foreign-enum-value
            '%gl:enum
            (case value
              ((:never nil) :never)
              ((:always t) :always)
              ((:equal := =) :equal)
              ((:not-equal :/= /=) :not-equal)
              ((:less :< <) :less)
              ((:greater :> >) :greater)
              ((:lequal :<= <=) :lequal)
              ((:gequal :>= >=) :gequal)
              (otherwise (error "Invalid compare func for texture ~a" value))))))
        (%gl:tex-parameter-i
         (texture-type texture) :texture-compare-mode
         (%gl::foreign-enum-value '%gl:enum :none))))
  texture)

(defun+ fallback-sampler-set (sampler)
  (let ((texture (%sampler-texture sampler))
        (id (%sampler-id sampler)))
    (unless (= id (texture-last-sampler-id texture))
      (setf (tex-lod-bias texture) (%sampler-lod-bias sampler)
            (tex-min-lod texture) (%sampler-min-lod sampler)
            (tex-max-lod texture) (%sampler-max-lod sampler)
            (tex-minify-filter texture) (%sampler-minify-filter sampler)
            (tex-magnify-filter texture) (%sampler-magnify-filter sampler)
            (tex-wrap texture) (%sampler-wrap sampler)
            (tex-compare texture) (%sampler-compare sampler)))
    sampler))

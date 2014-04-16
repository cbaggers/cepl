(in-package :cgl)

;; So what kind of data is allowed in a glstruct?

;; primitive cffi types 
;; primitive extra types (vec matrix etc)
;; glstructs
;; arrays

;; What does an accessor give?
;; Should be the a-w wrapper unless is array
;; or unless is primitive, then it should be the value



(autowrap:define-foreign-record
    'sdl-assert-data
    :struct
  384
  64
  '((:always-ignore :int :bit-size 32 :bit-offset 0 :bit-alignment
     32)
    (:trigger-count :unsigned-int :bit-size 32 :bit-offset 32
     :bit-alignment 32)
    (:condition (:string) :bit-size 64 :bit-offset 64 :bit-alignment
     64)
    (:filename (:string) :bit-size 64 :bit-offset 128 :bit-alignment
     64)
    (:linenum :int :bit-size 32 :bit-offset 192 :bit-alignment 32)
    (:function (:string) :bit-size 64 :bit-offset 256 :bit-alignment
     64)
    (:next (:pointer (:struct (sdl-assert-data))) :bit-size 64
     :bit-offset 320 :bit-alignment 64)))

(autowrap:define-foreign-alias
    'sdl-assert-data
    '(:struct (sdl-assert-data)))

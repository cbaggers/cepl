;; make-c-array
;; make-gpu-array

;; make from nested lists
;; make from nested arrays
;; make from multidimensional arrays

;; dimensions optional

;; ok so if they are provided

;; validate them unless no-validate flag is t

;; validate means:
;; if array
;;  - compare dimensions filling out pattern where :?
;; if list or vector
;;  - walk down and check sized (or get size if pattern is :?)
;;  - validate returns the correct dimensions OR nil

;; if result of validate is null then error else it is the dimensions

;; c-populate:
;; This also needs updating to assume that if array and not sequence
;; then loop over dimensions

 

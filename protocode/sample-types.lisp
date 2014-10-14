;; 
;; Should types denote internal machine structure?
;; or rather should they indicate the fundametal properties of the data.
;;
;; The core idea I am thinking about right now is data over time.
;; discrete data is fine, we can establish some mapping between time and the 
;; dataset and extract values...but whatif the data is (sin x)?
;;
;; Sampling is a loss of data. Now there are ways to, for example, get the 
;; average value of (sin x) where (and (>= x 0.2) (<= x 0.4)) but we have to
;; manage their application manually.
;;
;; Also look again at the discrete data. What if the sampling is less frequent 
;; than the element intevals? What if it is more frequent? How do we resolve 
;; this?
;;
;; Well first we have some function sample which is a HOF (higher order
;; function) taking a resolve function

(defun sample (resolver data start end)
  ...)

;; this makes explicit the fact the mapping is not direct
;; Do we have equivilent to :key and :test from #'equal?
;; do we have :key and :fold?

(defun sample (fold key data start end)
  ...)

;; key establishes the element from start and end.
;; fold gets called given the new elements and data

;; am i really saying anything new here? doubt it

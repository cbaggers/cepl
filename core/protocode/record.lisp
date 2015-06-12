
;; - first verion, static phone ui
;; - messages

;; message format
;; (start-tag type count data)

;; turn into events and put through event system

;; Record is seperate
(defvar r (evt:record #'filter #'transform))
;; returns recording object

(stop r)
;; disconnects from source, cannot be reconnected

(data r)
;; returns the payload as list

(play-into r #'some-func)
;; creates temporal lambda that will call the func with the events
;; (with new timestamps if applicable) at the correct offsets


;; wait could we implement these?
;; - pause
;; - seek
;; - loop
;; Meh not yet, just do the base case

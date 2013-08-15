(in-package :cgl)

;; When instantiated this object represents opengl's state machine
;; On creation it runs queries to find the mins, maxs and other details of the 
;; various opengl parameters.
;; You can then directly set state in opengl just by setf'ing the specific slots
;; of the state-machine.



;; type spec is a list made of:
;; - principle type
;; - structure
;; - length
;;
;; function in-spec is list of above (multiple input args)
;; if any part is set to 't' then it allows anything. e.g.
;; (t array t) 
;; means that the argument can be any type of array of any length
;;
;; the output spec is as follows
;; - principle type
;; - structure
;; - length
;; this can be hard coded or you can specify an in-var by name
;; the type will be pulled from that var.

;; there will be a few stages to evaluation
;; * literal evaluation - Numbers are numbers so swap them out for code-object
;; * macro evaluation, run the 'macro' functions and let them swap out the code.
;; 


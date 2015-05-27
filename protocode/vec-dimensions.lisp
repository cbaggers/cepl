;; OK so the 'list -v- vector for dimensions' comes back to haunt me
;; I need to support lists as this is the correct and standard way to specify
;; things. HOWEVER we like working with vectors in graphics, the damn things are
;; everywhere, and it would be nice to be able to use them in places that take
;; dimensions.

;; It's one of those 'its wrong but feels good' things

;; Ok so what kinds of vector?

;; in would make the most sense for it to be an int but then we cant use all the
;; vector transformt that specify on float vectors ARGGG

;; So float vecs it is 
(make-c-array (v! 100 200) :element-type :int)

;; looks nice

;; So find the places where the lists suck..

;; First is locality, it cant be unboxed so we have this thing that is weightier
;; that it should be. This could be important in say viewport or fbos where
;; we need the sizes many times a frame.
;; To be honest though then we split them up in the struct, have two int fields
;; use those internally and then make it a list when the user asks for it.

;; So optimization is a crap argument, so what is left, ease of use I guess.

;; when I want to use it as a vec it is too much work. So let's do this

(v! 1 #(2) '(3 4))

#(1.0 2.0 3.0 4.0)

;; bingo, now turning a list to a vector is

(v! x)

#(4.5 2.11)

;; nice, next problem :D

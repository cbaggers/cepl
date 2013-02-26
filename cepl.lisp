;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This is a dumping ground for homless functions
;; If anything is here currently then it needs to be rehomed or
;; the essence of the functionality decided, generalised and 
;; rehomed.

(in-package :cepl)

(defun repl ()
  (in-package :cepl)
  (if (sdl:init-sdl)
      (sdl:window 640 480 :icon-caption "CEPL REPL" :title-caption "CEPL REPL")
      (error "Failed to initialise SDL"))
  (format t "-----------------~%    CEPL-REPL    ~%-----------------"))

(defun collect-sdl-event-types ()
  (let ((x (sdl:new-event)))
    (loop until (= 0 (lbm-sdl-cffi::sdl-poll-event x))
	  collect (sdl:event-type x))
    (sdl:free-event x)))

(defun get-sdl-event (&optional event)
  (let ((event (or event (sdl:new-event))))
    (if (= (sdl-cffi::SDL-Poll-Event event) 0)
        (progn (sdl:free-event event)
               nil)
        event)))

(defmacro case-events ((event-var) &body cases)
  (if (symbolp event-var)
      `(let ((,event-var (sdl:new-event)))
         (loop :while (not (eq (sdl-cffi::SDL-Poll-Event 
                                ,event-var) 0))
            :do (case (sdl:event-type ,event-var)
                  ,@cases))
         (sdl:free-event ,event-var))
      (error "event-var must be a symbol")))



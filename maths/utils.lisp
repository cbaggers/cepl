;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cepl)

(defun range (x &optional y z w)
  (cond (w (loop :for wi :below w :append
              (loop :for zi :below z :append
                 (loop :for yi :below y :append
                    (loop :for xi :below x :collect `(,xi ,yi ,zi ,wi))))))
        (z (loop :for zi :below z :append
              (loop :for yi :below y :append
                 (loop :for xi :below x :collect `(,xi ,yi ,zi)))))
        (y (loop :for yi :below y :append
              (loop :for xi :below x :collect `(,xi ,yi))))
        (t (loop :for xi :below x :collect xi))))

(defun v-range (x y &optional z w)
  (cond (w (loop :for wi :below w :append
              (loop :for zi :below z :append
                 (loop :for yi :below y :append
                    (loop :for xi :below x :collect (v! xi yi zi wi))))))
        (z (loop :for zi :below z :append
              (loop :for yi :below y :append
                 (loop :for xi :below x :collect (v! xi yi zi)))))
        (t (loop :for yi :below y :append
              (loop :for xi :below x :collect (v! xi yi))))))


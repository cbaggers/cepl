(in-package :cgl)

(defconstant +restorable-viewport+ t)
(defparameter +default-resolution+ (list 640 480))

(defparameter *viewport-size* (v! (first cgl:+default-resolution+)
                                  (second cgl:+default-resolution+)))
(defparameter *viewport-origin* (v! 0 0))


;; {TODO} add declarations
(defun viewport (&optional (size cgl:+default-resolution+) (origin (v! 0 0)))
  (gl:viewport (v:x origin) (v:y origin) (first size) (second size)))


(defmacro with-viewport ((size &optional (origin (v! 0 0))) &body body)
  (let ((tmp-o (gensym "origin"))
        (tmp-s (gensym "size")))
    `(progn
       (let* ((,tmp-s ,size)
              (,tmp-o ,origin)
              ,@(when +restorable-viewport+
                      `((*viewport-size* ,tmp-s)
                        (*viewport-origin* ,tmp-o))))
         (viewport ,tmp-s ,tmp-o)
         ,@body)
       ,@(when +restorable-viewport+
               '((viewport *viewport-size* *viewport-origin*))))))


(defmacro with-fbo-viewport ((fbo &optional (attachment :color-0)) &body body)
  (let ((tex-array (gensym "tx-array"))
        (size (gensym "size")))
    `(let* ((,tex-array (attachment ,fbo ,attachment))
            (,size (dimensions ,tex-array)))
       (with-viewport ((v! (first ,size) (second ,size)))
         ,@body))))

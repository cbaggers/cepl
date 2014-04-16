;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package model-parsers)

(defun parse-lisp-model (filename)
  (let* ((data (utils:safe-read-from-string
                (utils:file-to-string filename))))
    (list (loop :for vert :in (first data) :collect
             (loop :for i :in vert :collect
                (if (listp i) 
                    (make-array 
                     (length i)
                     :element-type 'single-float
                      :initial-contents i)
                    i)))
          (second data))))

(defun load-lisp-model (filename data-type
                        &optional (index-type :unsigned-short))
  (destructuring-bind (verts indicies)
      (parse-lisp-model filename)
    (let ((vert-gpu (cgl:make-gpu-array
                     verts :element-type data-type))
          (index-gpu (cgl:make-gpu-array 
                      indicies
                      :element-type index-type
                      :dimensions (length indicies))))
      (values (cgl:make-vertex-stream 
               vert-gpu :index-array index-gpu)
              vert-gpu index-gpu))))

;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package base-matrices)

(labels ((convert (x)
           (coerce x 'single-float)))
  (defun m! (&rest components)
    (let* ((components
            (loop :for c :in components
               :if (listp c)
               :append (loop :for e :in c
                          :collect (coerce e 'single-float)) :else
               :if (typep c 'array)
               :append (loop :for e :across c
                          :collect (coerce e 'single-float)) :else
               :collect (coerce c 'single-float)))
           (len (length components))
           (arr (make-array (length components) :element-type 'single-float
                  :initial-contents components)))
      (cond
        ((= len 16) (m4:transpose arr))
        ((= len 9) (m3:transpose arr))
        (t (error "incorrect number of components for a vector: ~a ~a" len
                  components)))))

  (defun m!one-arg (x)
    (if (listp x)
        (make-array (length x) :element-type 'single-float :initial-contents
                    (mapcar #'convert x))
        (make-array (length x) :element-type 'single-float :initial-contents
                    (loop for i across x
                       collect (convert i)))))

  (define-compiler-macro m!
      (&whole form &rest components)
    (cond
      ((every #'numberp components)
       (let ((components
              (loop :for c :in components
                 :collect (coerce c 'single-float)))
             (len (length components)))
         (cond
           ((= len 16) `(m4:make-matrix4 ,@components))
           ((= len 9) `(m3:make-matrix3 ,@components))
           (t (error "incorrect number of components for a vector: ~a ~a" len
                     components)))))
      ((= (length components) 1) (list 'm!one-arg (first components)))
      (t form))))

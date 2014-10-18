(in-package :cepl)

(defmacro fn+ (function &rest more-functions)
  `(alexandria:compose ,function ,@more-functions))

(defun lambda-list-split (template lam-list)
  (let ((template (when template
                    (cons nil (mapcar #'utils:kwd template))))
        (split (hlp lam-list)))
    (if (or (null template)
            (every (fn+ (λ member % template) #'first) split))
        (clean-alist split)
        (error "&symbol found that was not specified in template ~s" 
               (mapcar #'first split))))) 

(defun hlp (lam-list &optional current-modifier accum)
  (let ((item (first lam-list)))
    (cond ((null lam-list) accum) 
          ((and (symbolp item) (eql (elt (symbol-name item) 0) #\&))
           (hlp (rest lam-list)
                (utils:symbolicate-package :keyword item)
                accum))
          (t (hlp (rest lam-list)
                  current-modifier
                  (acons current-modifier 
                         (cons item (cdr (assoc current-modifier accum)))
                         accum))))))


(defun clean-alist (alist &optional accum)
  (let ((item (first alist)))
    (cond ((null alist) accum)
          ((atom item) (clean-alist (rest alist) accum))
          ((not (assoc (first item) accum))
           (clean-alist (rest alist) (cons item accum)))
          (t (clean-alist (rest alist) accum)))))

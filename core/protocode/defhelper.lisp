(in-package :cepl)

(defmacro fn+ (function &rest more-functions)
  `(alexandria:compose ,function ,@more-functions))

(defun lambda-list-split (template lam-list)
  (labels ((collector (lam-list &optional current-modifier accum)
             (let ((item (first lam-list)))
               (cond ((null lam-list) accum)
                     ((and (symbolp item) (eql (elt (symbol-name item) 0) #\&))
                      (collector (rest lam-list)
                                 (cepl-utils:symb-package :keyword item)
                                 accum))
                     (t (collector (rest lam-list)
                                   current-modifier
                                   (acons current-modifier
                                          (cons item
                                                (cdr (assoc current-modifier
                                                            accum)))
                                          accum))))))
           (clean-alist (alist &optional accum)
             (let ((item (first alist)))
               (cond ((null alist) accum)
                     ((atom item) (clean-alist (rest alist) accum))
                     ((not (assoc (first item) accum))
                      (clean-alist (rest alist) (cons item accum)))
                     (t (clean-alist (rest alist) accum)))))
           (first-in-template (x) (member (first x) template)))
    (let ((template (when template (cons nil (mapcar #'cepl-utils:kwd template))))
          (split (collector lam-list)))
      (if (or (null template)
              (every #'first-in-template split))
          (clean-alist split)
          (error "&symbol found that was not specified in template ~s"
                 (mapcar #'first split))))))

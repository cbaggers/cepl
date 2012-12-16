;; `(defmethod expand-to-foreign-dyn (value var body 
;; 					  (type ,name))
;;    `(with-foreign-object (,var ,,type ,,length)
;;       ,,@(loop for i below length
;; 		      collect ``(setf (cffi:mem-aref ,var 
;; 						     ,,type 
;; 						     ,,i) 
;; 				      (aref ,value ,,i)))
;;       ,@body))

;; (list 
;;  'defmethod 'expand-from-foreign `(form (type ,name))
;;  `(let ((f-pointer-sym (gensym "F-POINTER")))
;;     `(let ((,f-pointer-sym ,form))
;;        (let ((result 
;;               (make-array
;;                ,,length 
;;                :element-type ',,lisp-type
;;                :initial-contents 
;;                (list ,,@(loop for i below length
;;                            collect ``(cffi:mem-aref 
;; 					     ,f-pointer-sym
;; 					     ,,type 
;; 					     ,,i))))))
;;          ;; included as I'm worried about the 
;; 		;; result memory
;;          (foreign-free ,f-pointer-sym) result)))

;; (defmacro defglstruct (name &body slot-descriptions)
;;   "This macro creates a foreign struct called name and
;;    also creates a load of helper functions so that this 
;;    struct can be used as if it were a native lisp struct.
;;    To explain these helper functions lets take the following
;;    glstruct defintion as an example.

;;    (defglstruct vert-data
;;      (position :type :float :length 3)
;;      (color :type :float :length 4))

;;    So along with the foreign struct vert-data you will also 
;;    get the following functions:
;;    - vert-data-position 
;;    - vert-data-color 
;;    These, just like lisp for structs, allow you to access 
;;    the data in the slot. If the :length is greater than 1 then
;;    the value is an array. These functions are also setf'able
;;    so you can do the following:
   
;;    > (setf *test-array* (make-gl-array 'vert-data :length 10)
;;      *test-array*
;;    > (setf (vert-data-position (aref-gl *test-array* 0)) 
;;                                #(1.0 2.0 3.0))
;;      #(1.0 2.0 3.0)
;;    > (vert-data-position (aref-gl *test-array* 0))
;;      #(1.0 2.0 3.0)
   
;;    It also provides methods so that the following generic functions
;;    will work with the new type.
;;    - destructuring-populate
;;    - destructuring-allocate
;;    - gl-pull
;;    - gl-push
;;    You dont have to worry about these yourself.

;;    So why use this at all? 
;;    Well firstly it makes things more lispy, which really helps
;;    when doing things from the REPL as you can focus on what you
;;    are writing without thinking about some opengl specific 
;;    methodology.
;;    Secondly because you are using structs the data in the buffer
;;    is interleaved for you. This can really help with performance
;;    as graphics processing generally prefers blocks of ordered 
;;    contiguous data. As an example if p=position and c=color then 
;;    you could lay your buffer out as follows
;;    >ppppppcccccc< but to read the position and color data for the 
;;    first vertex means pulling values from two different places.
;;    Using the glstruct means your data will be laid out like this:
;;    >pcpcpcpcpcpc< Which is faster for the gpu to handle.
;;    Obviously, nothing is for free and normally the cost is having
;;    to specify all this stuff in your vaos when you come to rendering
;;    but cepl-gl takes care of this for you if you have defined you 
;;    use types defined using glstruct.

;;    So all in all it's quite handy!"
;;   (let ((clauses (glstruct-slot-descriptions-to-clauses
;; 		  slot-descriptions)))
;;     `(progn 
;;        ;; make the c-struct that will be used by the glarrays
;;        (defcstruct ,name 
;;          ,@(loop for clause in clauses append (rest clause)))
;;        ;; make the getters for the foreign array
;;        ;; [TODO] make this generic for gl-arrays and gpu-arrays
;;        ,@(loop for clause in clauses
;;             collect (gen-component-getters name clause))
;;        ;; make the setters for the foreign array
;;        ;; [TODO] make this generic for gl-arrays and gpu-arrays
;;        ,@(loop for clause in clauses
;;             collect (gen-component-setters name clause))
;;        ;; make the generic destructuring-populate method for this
;;        ;; type.
;;        ,@(let ((loop-token (gensym "LOOP"))
;; 	       (clause-names (mapcar #'car clauses)))
;;            (list
;; 	    `(defmethod dpopulate ((array-type (eql ',name))
;; 				   gl-array
;; 				   data)
;; 	       (loop for ,clause-names in data
;; 		  for ,loop-token from 0
;; 		  do ,@(loop for c in clause-names
;; 			  collect
;; 			    `(setf (,(cepl-utils:symb name '- c)
;; 				     (aref-gl gl-array
;; 					      ,loop-token))
;; 				   ,c))))
;; 	    `(defmethod glpull-entry ((array-type (eql ',name))
;; 				      gl-array
;; 				      index)
;; 	       (list ,@(loop for c in clause-names
;; 			  collect
;; 			    `(,(cepl-utils:symb name '-	c)
;; 			       (aref-gl gl-array index)))))))
;;        ,(let ((stride (if (> (length clauses) 1)
;;                           `(cffi:foreign-type-size ',name)
;;                           0)))
;;              `(defmethod gl-type-format ((array-type (EQL ',name)) &optional (address-offset 0))
;;                 (list ,@(loop for slotd in slot-descriptions
;;                            collect 
;;                              (destructuring-bind (slot-name &key type (length 1) (normalised nil) 
;;                                                             &allow-other-keys)
;;                                  slotd
;;                                `(list ,length ,type ,normalised ,stride 
;;                                       (cffi:make-pointer 
;;                                        (+ (foreign-slot-offset ',name ',(cepl-utils:symb slot-name 0))
;;                                           address-offset))))))))
;;        ',name)))


;; (defun gen-component-setters (type-name clause)
;;   (let ((comp-name (first clause))
;; 	(slots (rest clause)))
;;     (if (eq (length slots) 1)
;; 	(let ((slot (first slots)))
;; 	  `(defun 
;; 	       (setf ,(cepl-utils:symb type-name '- comp-name)) 
;; 	       (value instance)
;; 	     (setf (foreign-slot-value instance 
;; 				       ',type-name
;; 				       ',(first slot))
;; 		   value)))
;; 	`(defun 
;; 	     (setf ,(cepl-utils:symb type-name '- comp-name))
;; 	     (value instance)
;; 	   ,@(loop for slot in slots
;; 		for i from 0
;; 		collect 
;; 		  `(setf (foreign-slot-value instance
;; 					     ',type-name
;; 					     ',(first slot))
;; 			 (aref value ,i)))
;; 	   value))))

;; ;; [TODO] Type specifiers for returned arrays? YES DEFINITELY
;; (defun gen-component-getters (type-name clause)
;;   (let ((comp-name (first clause))
;; 	(slots (rest clause)))
;;     `(defun ,(cepl-utils:symb type-name '- comp-name) (instance)
;;        ,(if (eq (length slots) 1)
;; 	    (let ((slot (first slots)))
;; 	      `(foreign-slot-value instance 
;; 				   ',type-name
;; 				   ',(first slot)))
;; 	    `(make-array ,(length slots)
;; 			 ;; :element-type 
;; 			 ;; ',(gl::symbolic-type->real-type 
;; 			 ;;   (second (first slots)))
;; 			 :initial-contents
;; 			 (list ,@(loop for slot in slots
;; 				    collect
;; 				      `(foreign-slot-value 
;; 					instance 
;; 					',type-name
;; 					',(first slot)))))))))

;; (defun glstruct-slot-descriptions-to-clauses (slot-descriptions)
;;   (loop for clause in slot-descriptions
;;      collect 
;;        (destructuring-bind (name &key type (length 1) 
;;                                  &allow-other-keys)
;; 	   clause
;; 	 (if (or (< length 1) (> length 4))
;; 	     (error "Invalid length of slot"))
;; 	 (cons name (loop for i below length
;; 		       collect `(,(cepl-utils:symb name i)
;; 				  ,type))))))

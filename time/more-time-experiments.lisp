;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; Functions and macros for handling time in games
;; This area is very prone to change as time is so integral
;; to game building.

(in-package :base-time)

;;----------------------------------------------------------------------

;; (tlambda ()
;;     (repeat ((before (from-now 300))
;;              (setf (pos *e*) (lerp (pos *e*) (v! 1 2 3))))
;;             ((before (from-now 400))
;;              (setf (pos *e*) (lerp (pos *e*) (v! 0 0 0)))))
;;   (before (from-now (seconds 4)) (print "first 4 seconds"))
;;   (print "hi"))

;; (let ((~c-0 0)
;;       (~dl-1 (from-now (seconds 4)))
;;       (~dl-2 (from-now 300))
;;       ~dl-3)
;;   (lambda ()
;;     (case ~c-0
;;       (0 (if (beforep (~dl-2))
;;              (progn (setf (pos *e*) (lerp (pos *e*) (v! 1 2 3))))
;;              (setf ~c-0 1 ~dl-3 (from-now 400))))
;;       (1 (if (beforep (~dl-3))
;;              (progn (setf (pos *e*) (lerp (pos *e*) (v! 0 0 0))))
;;              (setf ~c-0 0 ~dl-2 (from-now 300)))))
;;     (when (beforep (~dl-1) (print "first 4 seconds")))
;;     (print "hi")))

;; (body test expired-test inner-let closed-vars)

(tlambda+ (x y) (+ x y))

;;{TODO} should "every time" stil print after it expires?
(tlambda+ ()
  ((before (from-now 100)) (print "moo"))
  (print "every time"))

(tlambda+ () 
  ((and (each (seconds 1)) (before (from-now (seconds 4)))) 
   (print "hi")))

;;{TODO} I think ther are ways to have conflicting expired, address this
;;       I was slightly off but had the right idea...only this macro must
;;       signal expiry, the other must idicate it so they can be nested
(defmacro tlambda+ (args &body body)
  (destructuring-bind (body test expired-test inner-let closed-vars)
      (tprogn+ body)
    (declare (ignore test inner-let))
    `(let (,@closed-vars)
       (lambda ,args
         ,(if expired-test              
              `(if ,expired-test
                   (signal-expired)
                   ,body)
              body)))))

(defun %compile-time-syntax+ (form)
  (if (atom form) (values form nil nil nil)
      (let* ((tname (first form))
             (state nil)
             (expiredp nil)
             (anaphora nil)
             (body (loop :for item :in (rest form) :collect
                      (multiple-value-bind (c e a s) (%compile-time-syntax+ item)
                        (setf state (append s state))
                        (setf anaphora (append a anaphora))
                        (when e (push e expiredp))
                        c))))
        (multiple-value-bind (c s e a) (time-syntax-expand tname body)
          (values c 
                  (if (and (symbolp e) expiredp) (cons e expiredp) e)
                  (remove nil (append a (reverse anaphora)))
                  (remove nil (append s (reverse state))))))))

(defun %process-tprogn-forms (forms)
  (loop :for form :in forms :collect
     (if (atom form) (list form t)
         (case (first form)
           (repeat (trepeat+ (rest form)))
           (then (tthen+ (rest form)))
           (progn (tprogn+ (rest form)))
           (otherwise (if (or (atom (first form))
                              (eq (caar form) 'lambda))
                          (list form t)
                          (cons (second form)
                                (multiple-value-list 
                                 (%compile-time-syntax+ (first form))))))))))

;; 1. body - 
;; 2. test - 
;; 3. expired-test - 
;; 4. inner-let - 
;; 5. closed-vars - proper let form

;;{TODO} the expired is wrong, must hand it down
(defun tprogn+ (forms)
  (let* ((processed-forms (%process-tprogn-forms forms))
         (expire-forms (remove nil (mapcar #'third processed-forms))))
    (list `(let (,(remove nil (mapcan #'fourth processed-forms)))
             ,@(loop :for (body test expired-test inner-let closed-vars) 
                  :in processed-forms :collect
                  (if (and (eq test t) (null expired-test))
                      body
                      `(when ,test ,body))))
          t          
          (when expire-forms `(and ,@expire-forms))
          nil
          (remove nil (mapcan #'fifth processed-forms)))))

(defun tthen+ (forms)
  (let* ((processed-forms (%process-tprogn-forms forms))
         (counter-sym (gensym "counter"))
         (%closed-vars (remove nil (mapcan #'fifth (copy-tree processed-forms)))))
    (list `(let (,(remove nil (mapcar #'fourth processed-forms)))
             (case ,counter-sym
               ,@(loop :for (body test expired-test inner-let closed-vars)
                    :in processed-forms
                    :for index :from 0 :collect
                    `(,index 
                      (if ,test 
                          ,body
                          ,(when expired-test
                                 `(when ,expired-test
                                    (incf ,counter-sym)
                                    ,@(when (< index (1- (length forms)))
                                            (loop :for i :in (fifth (nth (+ index 1) processed-forms)) :collect `(setf ,@i))))))))))
          `(< ,counter-sym ,(length forms))
          `(= ,counter-sym ,(length forms))
          nil
          (cons `(,counter-sym 0)
                (cons (first %closed-vars) 
                      (mapcar #'listify (rest %closed-vars)))))))

;;{TODO}
;; ---------------------
;; EXPAND THE CODE BELOW
;; ---------------------
;; Find out how to make sure nested closed-vars are spliced in correctly,
;; be careful with mapcan. It seems to be related to %closed-vars in trepeat+

(tlambda+ ()
  (repeat ((before (from-now 100)) (print "--1"))
        ((before (from-now 200)) (print "--2"))
        ((before (from-now 300)) (print "--3"))
        ((before (from-now 400)) (print "--4"))))

(tlambda+ ()
  (repeat (then ((before (from-now (seconds 2))) (print "1"))
                ((before (from-now (seconds 3))) (print "2")))
          ((before (from-now 100)) (print "moo"))
          ((before (from-now 999)) (print "wwwwww"))
          ((before (from-now 200)) (print "woo"))))

(tlambda+ ()
  (repeat (then ((before (from-now (seconds 2))) (print "1")))
          ((before (from-now 100)) (print "moo"))))

(defun trepeat+ (forms) 
  (let* ((processed-forms (%process-tprogn-forms forms))
         (counter-sym (gensym "counter"))
         (%closed-vars (remove nil (mapcan #'fifth (copy-tree processed-forms)))))
    (list `(let (,(remove nil (mapcar #'fourth (copy-list processed-forms))))
             (case ,counter-sym
               ,@(loop :for (body test expired-test inner-let closed-vars)
                    :in processed-forms
                    :for index :from 0 :collect
                    `(,index 
                      (if ,test 
                          ,body
                          ,(when expired-test
                                 `(when ,expired-test
                                    ,@(if (= index (1- (length forms)))
                                          (cons `(setf ,counter-sym 0)
                                                (loop :for i :in (fifth (first processed-forms)) :collect `(setf ,@i)))
                                          (cons `(incf ,counter-sym)
                                                (loop :for i :in (fifth (nth (+ index 1) processed-forms)) :collect `(setf ,@i)))))))))))
          t
          nil
          nil
          (cons `(,counter-sym 0)
                (cons (first %closed-vars)
                      (mapcar #'listify (rest %closed-vars)))))))

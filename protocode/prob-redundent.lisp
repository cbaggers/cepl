+;; {TODO} add this to create array etc
+(defun calc-dimensions (pattern data)
+  (labels ((%calc-dimensions (pattern data accum)  
+  (if pattern
+      (progn
+        (unless (or (listp data) (arrayp data))
+          (error "pattern doest match data ~a ~a"
+                 pattern data))
+        (%calc-dimensions (rest pattern)
+                          (first data)
+                          (if (eq :? (first pattern))
+                              (cons (length data) accum)
+                              (if (= (first pattern)
+                                     (length data))
+                                  (cons (first pattern) accum)
+                                  (error "pattern doest match data ~a ~a"
+                                         pattern data)))))
+      (reverse accum))))
+    (cond ((eq :? pattern) 
+           (if (or (listp (first data)) (arrayp (first data))) 
+               (error "pattern doest match data ~a ~a"
+                      pattern data)
+               (length data)))
+          ((listp pattern) (%calc-dimensions pattern data nil))
+          (t (error "invalid pattern")))))
+

(LET ((PROGRAM NIL))
  (DEFUN PROG-1 (STREAM &KEY A B)
    (WHEN (NOT PROGRAM)
      (SETF PROGRAM
              (LET* ((GLSL-SRC
                      (VARJO:ROLLING-TRANSLATE
                       '((VERT VERT-DATA) &UNIFORM (A VERT-DATA) (B :VEC4))
                       (LIST
                        '(:VERTEX (SETF GL-POSITION (POS VERT))
                          (OUT (THE-COLOR :SMOOTH) (COL VERT)))
                        '(:FRAGMENT
                          (LET ((LERP-VALUE (/ (Y GL-FRAG-COORD) 500.0)))
                            (OUT OUTPUTCOLOR
                             (MIX THE-COLOR (VEC4 0.2 0.2 0.2 1.0) LERP-VALUE)))))))
                     (SHADERS
                      (LOOP FOR (TYPE CODE) IN GLSL-SRC
                            :COLLECT (MAKE-SHADER TYPE CODE)))
                     (PROGRAM-ID (LINK-SHADERS SHADERS (PROGRAM-MANAGER 'PROG-1)))
                     (ASSIGNERS
                      (CREATE-UNIFORM-ASSIGNERS PROGRAM-ID '((A VERT-DATA) (B :VEC4))
                                                :jungl))
                     (A-ASSIGNER (NTH 0 ASSIGNERS))
                     (B-ASSIGNER (NTH 1 ASSIGNERS)))
                (DECLARE (IGNORABLE ASSIGNERS))
                (print assigners)
                (WHEN *CACHED-GLSL-SOURCE-CODE*
                  (SETF (GETHASH #'PROG-1 *CACHED-GLSL-SOURCE-CODE*) GLSL-SRC))
                (MAPCAR #'DELETE-SHADER SHADERS)
                (UNBIND-BUFFER)
                (FORCE-BIND-VAO 0)
                (FORCE-USE-PROGRAM 0)
                (LAMBDA (STREAM &KEY A B)
                  (USE-PROGRAM PROGRAM-ID)
                  (WHEN A (DOLIST (FUN A-ASSIGNER) (FUNCALL FUN A)))
                  (WHEN B (DOLIST (FUN B-ASSIGNER) (FUNCALL FUN B)))
                  (WHEN STREAM (NO-BIND-DRAW-ONE STREAM))))))
    (FUNCALL PROGRAM STREAM :A A :B B)))

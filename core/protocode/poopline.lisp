(PROGN
 (LET ((CEPL.PIPELINES::PROG-ID NIL)
       (CEPL.PIPELINES::IMPLICIT-UNIFORM-UPLOAD-FUNC
        #'CEPL.PIPELINES::FALLBACK-IUNIFORM-FUNC)
       (#:G1581 -1)
       (#:IMAGE-UNIT1582 -1)
       (#:G1583 -1))
   (DECLARE (TYPE FUNCTION CEPL.PIPELINES::IMPLICIT-UNIFORM-UPLOAD-FUNC))
   (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (CEPL.PIPELINES::UPDATE-PIPELINE-SPEC
      (CEPL.PIPELINES::MAKE-PIPELINE-SPEC 'DRAW-TEXTURE-PIPELINE
                                          (LIST
                                           (CEPL.PIPELINES::NEW-FUNC-KEY
                                            'DRAW-TEXTURE-VERT '(G-PT))
                                           (CEPL.PIPELINES::NEW-FUNC-KEY
                                            'DRAW-TEXTURE-FRAG '(:VEC2)))
                                          'NIL)))
   (LABELS ((#:|init1584| ()
              (LET ((CEPL.PIPELINES::IMAGE-UNIT -1))
                (DECLARE (IGNORABLE CEPL.PIPELINES::IMAGE-UNIT))
                (MULTIPLE-VALUE-BIND
                    (CEPL.PIPELINES::COMPILED-STAGES
                     CEPL.PIPELINES::NEW-PROG-ID)
                    (CEPL.PIPELINES::%COMPILE-LINK-AND-UPLOAD
                     'DRAW-TEXTURE-PIPELINE
                     (LIST
                      (CONS :VERTEX
                            (CEPL.PIPELINES::NEW-FUNC-KEY 'DRAW-TEXTURE-VERT
                                                          '(G-PT)))
                      (CONS :FRAGMENT
                            (CEPL.PIPELINES::NEW-FUNC-KEY 'DRAW-TEXTURE-FRAG
                                                          '(:VEC2)))))
                  (DECLARE (IGNORABLE CEPL.PIPELINES::COMPILED-STAGES))
                  (SETF CEPL.PIPELINES::PROG-ID CEPL.PIPELINES::NEW-PROG-ID)
                  (SETF CEPL.PIPELINES::IMPLICIT-UNIFORM-UPLOAD-FUNC
                          (OR
                           (CEPL.PIPELINES::%CREATE-IMPLICIT-UNIFORM-UPLOADER
                            CEPL.PIPELINES::COMPILED-STAGES)
                           #'CEPL.PIPELINES::FALLBACK-IUNIFORM-FUNC)))
                (SETF #:G1581
                        (CL-OPENGL:GET-UNIFORM-LOCATION CEPL.PIPELINES::PROG-ID
                                                        "TEX"))
                (SETF #:IMAGE-UNIT1582 (INCF CEPL.PIPELINES::IMAGE-UNIT))
                (SETF #:G1583
                        (CL-OPENGL:GET-UNIFORM-LOCATION CEPL.PIPELINES::PROG-ID
                                                        "DEPTH"))
                (CEPL.PIPELINES::%POST-INIT NIL)
                CEPL.PIPELINES::PROG-ID)))
     (PROGN
      (DEFUN %TOUCH-DRAW-TEXTURE-PIPELINE (&KEY CEPL.PIPELINES::VERBOSE)
        (LET ((CEPL.PIPELINES::*VERBOSE-COMPILES* CEPL.PIPELINES::VERBOSE))
          (UNLESS CEPL.PIPELINES::PROG-ID
            (SETF CEPL.PIPELINES::PROG-ID (#:|init1584|))))
        (WHEN CEPL.PIPELINES::VERBOSE
          (FORMAT T "
----------------------------------------

name: DRAW-TEXTURE-PIPELINE

pipeline compile context: NIL

uniform assigners:
((((#:G1581 (CL-OPENGL:GET-UNIFORM-LOCATION CEPL.PIPELINES::PROG-ID \"TEX\"))
   (#:IMAGE-UNIT1582 (INCF CEPL.PIPELINES::IMAGE-UNIT)))
  (WHEN TEX
    (LET ((CEPL.PIPELINES::VAL TEX))
      (WHEN (>= #:G1581 0)
        (UNLESS
            (EQ (%CEPL.TYPES:%SAMPLER-TYPE CEPL.PIPELINES::VAL) :SAMPLER-2D)
          (ERROR \"incorrect type of sampler passed to shader\"))
        (CEPL.TEXTURES::ACTIVE-TEXTURE-NUM #:IMAGE-UNIT1582)
        (CEPL.TEXTURES::BIND-TEXTURE
         (%CEPL.TYPES:%SAMPLER-TEXTURE CEPL.PIPELINES::VAL))
        (IF CEPL.SAMPLERS::*SAMPLERS-AVAILABLE*
            (CL-OPENGL-BINDINGS:BIND-SAMPLER #:IMAGE-UNIT1582
                                             (%CEPL.TYPES:%SAMPLER-ID
                                              CEPL.PIPELINES::VAL))
            (CEPL.TEXTURES::FALLBACK-SAMPLER-SET CEPL.PIPELINES::VAL))
        (CEPL.PIPELINES::UNIFORM-SAMPLER #:G1581 #:IMAGE-UNIT1582)))))
 (((#:G1583 (CL-OPENGL:GET-UNIFORM-LOCATION CEPL.PIPELINES::PROG-ID \"DEPTH\")))
  (WHEN DEPTH
    (LET ((CEPL.PIPELINES::VAL DEPTH))
      (WHEN (>= #:G1583 0)
        (CEPL.PIPELINES::UNIFORM-1F #:G1583 CEPL.PIPELINES::VAL))))))

uniform transforms:
((DEPTH DEPTH) (TEX TEX))

----------------------------------------"))
        T)
      (DEFUN DRAW-TEXTURE-PIPELINE
             (CEPL.PIPELINES::MAPG-CONTEXT STREAM &KEY TEX DEPTH)
        (DECLARE (IGNORE CEPL.PIPELINES::MAPG-CONTEXT)
                 (IGNORABLE TEX DEPTH))
        (UNLESS CEPL.PIPELINES::PROG-ID
          (SETF CEPL.PIPELINES::PROG-ID (#:|init1584|))
          (UNLESS CEPL.PIPELINES::PROG-ID (RETURN-FROM DRAW-TEXTURE-PIPELINE)))
        (CEPL.PIPELINES::USE-PROGRAM CEPL.PIPELINES::PROG-ID)
        (LET ((DEPTH DEPTH) (TEX TEX))
          (WHEN TEX
            (LET ((CEPL.PIPELINES::VAL TEX))
              (WHEN (>= #:G1581 0)
                (UNLESS
                    (EQ (%CEPL.TYPES:%SAMPLER-TYPE CEPL.PIPELINES::VAL)
                        :SAMPLER-2D)
                  (ERROR "incorrect type of sampler passed to shader"))
                (CEPL.TEXTURES::ACTIVE-TEXTURE-NUM #:IMAGE-UNIT1582)
                (CEPL.TEXTURES::BIND-TEXTURE
                 (%CEPL.TYPES:%SAMPLER-TEXTURE CEPL.PIPELINES::VAL))
                (IF CEPL.SAMPLERS::*SAMPLERS-AVAILABLE*
                    (CL-OPENGL-BINDINGS:BIND-SAMPLER #:IMAGE-UNIT1582
                                                     (%CEPL.TYPES:%SAMPLER-ID
                                                      CEPL.PIPELINES::VAL))
                    (CEPL.TEXTURES::FALLBACK-SAMPLER-SET CEPL.PIPELINES::VAL))
                (CEPL.PIPELINES::UNIFORM-SAMPLER #:G1581 #:IMAGE-UNIT1582))))
          (WHEN DEPTH
            (LET ((CEPL.PIPELINES::VAL DEPTH))
              (WHEN (>= #:G1583 0)
                (CEPL.PIPELINES::UNIFORM-1F #:G1583 CEPL.PIPELINES::VAL)))))
        (LOCALLY
         (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 1)))
         (FUNCALL CEPL.PIPELINES::IMPLICIT-UNIFORM-UPLOAD-FUNC
                  CEPL.PIPELINES::PROG-ID))
        (WHEN STREAM (CEPL.PIPELINES::DRAW-EXPANDER STREAM :TRIANGLES))
        (CEPL.PIPELINES::USE-PROGRAM 0)
        (CL-OPENGL-BINDINGS:BIND-SAMPLER #:IMAGE-UNIT1582 0)
        STREAM)
      (DEFINE-COMPILER-MACRO DRAW-TEXTURE-PIPELINE
          (&WHOLE CEPL.PIPELINES::WHOLE CEPL.PIPELINES::MAPG-CONTEXT &REST REST)
        (DECLARE (IGNORE REST))
        (UNLESS (CEPL.PIPELINES::MAPG-CONSTANTP CEPL.PIPELINES::MAPG-CONTEXT)
          (ERROR 'CEPL.ERRORS:DISPATCH-CALLED-OUTSIDE-OF-MAP-G :NAME
                 'DRAW-TEXTURE-PIPELINE))
        CEPL.PIPELINES::WHOLE))))
 (DEFUN CEPL::~~-DRAW-TEXTURE-PIPELINE ()
   (FORMAT T "~&; recompile cpu side of (~a ...)~&" 'DRAW-TEXTURE-PIPELINE)
   (FORCE-OUTPUT)
   (LET ((*STANDARD-OUTPUT* (MAKE-STRING-OUTPUT-STREAM)))
     (HANDLER-BIND ((WARNING #'MUFFLE-WARNING))
       (EVAL
        (CEPL.PIPELINES::%DEFPIPELINE-GFUNCS 'DRAW-TEXTURE-PIPELINE
                                             '(#'DRAW-TEXTURE-VERT
                                               #'DRAW-TEXTURE-FRAG)
                                             'NIL T)))))
 (CEPL::~~-DRAW-TEXTURE-PIPELINE))

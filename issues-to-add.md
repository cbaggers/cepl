# Yo fuckface, add these to github

- store the compile-file and that shit so we can apply it on the recompile
- switch to using uiop:define-package
- holy crap push-g on vectors is broken

- in draw-arrays our (buffer-stream-start stream) is causing an optimize warning
  note:
    unable to
      optimize
    due to type uncertainty:
      The first argument is a REAL, not a SINGLE-FLOAT.
    --> BLOCK MULTIPLE-VALUE-PROG1 CFFI:FOREIGN-FUNCALL LET LET TRUNCATE
    --> TRUNCATE LET
    ==>
      (SB-KERNEL:%UNARY-TRUNCATE SB-C::X)

  note:
    unable to
      optimize
    due to type uncertainty:
      The first argument is a REAL, not a DOUBLE-FLOAT.
    --> BLOCK MULTIPLE-VALUE-PROG1 CFFI:FOREIGN-FUNCALL LET LET TRUNCATE
    --> TRUNCATE LET
    ==>
      (SB-KERNEL:%UNARY-TRUNCATE SB-C::X)



- in draw-elements our (buffer-stream-start-byte stream) is causing an optimize warning
  note:
   doing unsigned word to integer coercion (cost 20)
   --> BLOCK MULTIPLE-VALUE-PROG1 CFFI:FOREIGN-FUNCALL LET LET LET LET LET
   --> IF CFFI-SYS:POINTER-ADDRESS BLOCK
   ==>
     (SB-SYS:SAP-INT CFFI-SYS::PTR)

cepl
====

Code Evaluate Play Loop

My first lisp project, a chance to play about and get something up in opengl.

-----

**Requirements**

* cl-opengl
* lispbuilder-mini (or lispbuilder <see end for note>)
* varjo
* cl-utilities
* cl-ppcre
* symbol-munger

If you are using quicklisp then drop this in your local-projects directory and run the following in your repl:
    (ql:quickload :cepl)
    (cepl:repl)

*Note regarding lispbuilder*
Lispbuilder is awesome but compatibilty is not complete yet as 
lispbuilder-mini is changing so fast. As soon as possible though
I will make sure that you can just uncomment a couple of lines
in the package.lisp file and be under way using lispbuilder.

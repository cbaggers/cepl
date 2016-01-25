# Jungl

> Welcome to the jungl, we've got funny names

Jungl is a lisp abstraction over opengl.

#### What does Jungl do?

Jungl makes opengl sane(r) and lispy.

#### What doesnt Jungl do?

- create the gl context
- handle input events
- handle audio

For these things see [CEPL](https://github.com/cbaggers/cepl) which provides Jungl, sets it up and pairs it with input handling. If you want to play with Jungl you almost certainly actually want CEPL.

#### Who's this for?

It's mainly spun off to keep CEPL and Jungl from depending on each other too tightly. I hope this will keep Jungl purely focused on GL and CEPL focused on all the surrounding stuff.

But if you need the GL core of CEPL without the code that connects it to backends then here it is :)

`p.s:` If you want CEPL with a different backend please contact me, I'd love to have CEPL support more backends and it definitely can (with a little refactoring :) )

#||

Q: Why is it that we compile things in init rather than sometime earlier

A: Because we dont know the gl version until runtime, this affects the glsl
   version which affects what is valid
||#

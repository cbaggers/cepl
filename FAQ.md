# General Questions

#### Q: What is a 'host' in CEPL and why do I need one?

CEPL, like OpenGL, doesn't manage windows or window events.  CEPL simply works with (and around) GL and the GPU.  In order to function, CEPL needs something to provide the OpenGL context it will use. This is the job of the 'host'.

A host only needs to satisfy a very basic API, so it's easy for you to make your own project a host. Or you can use the pre-made hosts like `cepl.sdl2`

# Cepl Systen Architecture Questions

#### Q: Why are no hosts included by default? You could have them as part of this repo

True, but I wanted to make it clear that hosts are totally seperate from CEPL and that any project can host it, if it satifies the cepl.host API.


#### Q: Why are 'hosts' called 'hosts' and not something like 'backends'?

To me this implies that cepl is a frontend.  This feels wrong as CEPL is just a tool; the frontend to you application is yours to define.

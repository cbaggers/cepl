# General Questions

#### Q: What is a 'host' in cepl and why do I need one?

Cepl, like opengl, doesnt dictate how you will manage windows or window events, it simply works with (and around) GL and the Gpu. So to work cepl needs something to provide the OpenGL context it will use. This is the job of the 'host'.

A host only needs to satisfy a very basic api so it's easy for you to make your own project a host. Or you can use the pre-made hosts like `cepl.sdl2`

# Cepl Systen Architecture Questions

#### Q: Why are no hosts included by default? You could have them as part of this repo

True, but I wanted to make it clear that hosts are totally seperate from cepl and that any project can host it if it satifies the cepl.host api.


#### Q: Why are 'hosts' called 'hosts' and not something like 'backends'?

To me this gave the impression that cepl was a frontend, this feels wrong as cepl is just a tool, the frontend to you application is yours to define.

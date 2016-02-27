ok so where am I?

Well I'm trying not to add spaces to everything. It's very tempting to add to g-arrays structs etc etc, but that way Im baking concepts into low level concepts. This is dumb and will hurt later on.

However we need to be clear on what goes where so first job is to find out what the default space of each shader stage should be.
There may be a protocode for this or maybe it's somewhere else, I hope not. that would be dumbg... hey check the older laptop, may be in there. Also check docs on macbook.

FOUND IT! ../../stages.lisp check it out

hmm after we work that out just add it to cepl. Dotn care about what it breaks, just do it. The we need to extend context to support 'space. Thsi will set what the default form is.

Want to know the fundamental spaces and bake em ...grr this burns the brain.

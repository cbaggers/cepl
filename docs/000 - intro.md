# Cepl - (Alpha)

Cepl is a sane interface to the modern opengl api.

### OpenGL Background

OpenGL gets a bad rap for its api, and for good reason, it is actually 2 (pretty much) incompatible apis in one box.

The first api is the old fixed function pipeline and the other is the modern shader based pipeline. The ugly thing is that these apis often reuse functions even when the behaviour underneath is very different. This makes for a very fustrating experience.

The belief help by Cepl is that there is an good api in there, it's just very well hidden :)

### Cepl's Goals

- Make a sane interface to the modern opengl api.
- Make working opengl lispy & repl friendly
- Do the above without wasting performance (see perf section below)

### Performance

The third and second goals can often be at odds in a project like this. We want to get the abosulte maximum power that it's possible to get out of common lisp *but* we also adore the repl and the power it brings.

The goal therefore is to always have a codepath that is optimized as hard as we can (whilst being as pleasant as possible) and then in those situations where it can't be made repl friendly we have a more generic code path that sacrifices a bit of performace for coder-experience.

There should be as little of the later as possible though. Very high level of abstractions tend to be geared to a certain aproach or technique and those things are best kept in a seperate library that depends on Cepl.


### Can I do 'x'?

If there is a feature in modern opengl that is good practice and is either impossible or prohibitively slow then 95% of the time that is considered a bug.

A goal is that each thing people fine that isnt going to be supported must be recorded in the [wont-support.md](./wont-support.md) file and be justified. We can't guarentee that the reason will be satisfying but it should be in keeping with the project.

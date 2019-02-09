# Framebuffer Objects (FBOs)

When we render in OpenGL, we render into a framebuffer. Framebuffers contain a number of `attachments`, and these attachments are where the stuff we are rendering ends up. The logic of what ends up where will be described a little further down.

We can either render into the default framebuffer or a user defined one.

#### Default

The *default framebuffer* is the framebuffer that is created along with the OpenGL context. Unlike user-defined FBOs, one (well, usually one) of the attachments represents what you actually see on the screen.

#### User Defined

We can make our own FBOs and tell CEPL to render to these instead. When we do this, we don't see anything we rendered on screen but instead have the rendering result in the attachments of our FBO.

Why do this? Well, our attachments can have texture backed `gpu-array`s in them. This means we can render into a texture. Have you have ever seen a game which had 'security cam' footage on an in-game screen? Chances are they are rendering part of the world to a texture and texturing the screen with that result. Rendering to texture is useful for much more than this of course.

#### Attachments

Before we start making FBOs we need to know a little more about attachments.

In CEPL, attachments contain something we are rendering into and some metadata on how to render into it. Right now, the only thing we put in an attachment is a texture-backed `gpu-array`. In the future we may support OpenGL's renderbuffer, but there are far fewer cases where this is useful (compared to rendering into a texture) so it has been ommited for now.

An attachment has a certain format. It can be a `color`, `depth` or `stencil` attachment.

#### Color Attachments

An FBO can have **some** color attachments. The number that **some** stands for is `at least 8 but maybe more` according to OpenGL, the exact figure is defined by `:MAX_COLOR_ATTACHMENTS` (we will cover querying these values in another chapter).

For us, a color attachment is one that holds a `gpu-array` with any of these image formats:
```
:r8 :r8_snorm :r16 :r16_snorm :rg8 :rg8_snorm :rg16 :rg16_snorm :r3_g3_b2 :rgb4
:rgb5 :rgb8 :rgb8_snorm :rgb10 :rgb12 :rgb16_snorm :rgba2 :rgba4 :rgb5_a1 :rgba8
:rgba8_snorm :rgb10_a2 :rgb10_a2ui :rgba12 :rgba16 :srgb8 :srgb8_alpha8 :r16f :rg16f
:rgb16f :rgba16f :r32f :rg32f :rgb32f :rgba32f :r11f_g11f_b10f :rgb9_e5 :r8i :r8ui
:r16i :r16ui :r32i :r32ui :rg8i :rg8ui :rg16i :rg16ui :rg32i :rg32ui :rgb8i :rgb8ui
:rgb16i :rgb16ui :rgb32i :rgb32ui :rgba8i :rgba8ui :rgba16i :rgba16ui :rgba32i :rgba32ui
```

FBOs are cool because you can render to multiple of these color attachments at once.

#### Depth Attachment

You can have one depth attachment on an FBO. This means that the depth of each fragment from your fragment shader will be stored in here.

Only `gpu-array`s with depth-formats are allowed to be used in this attachment. The valid formats are

```
:depth-component16, :depth-component24, :depth-component32 :depth-component32f
```

The OpenGL wiki stresses that

>  Even if you don't plan on reading from this depth_attachment, an off screen buffer that will be rendered to should have a depth attachment

So it's always worth having one.


#### Stencil/Depth-Stencil Attachments

Are not supported yet, sorry


#### Making FBOs

CEPL really tries to ensure that you don't make an FBO that is in an invalid state. Getting those kinds of *'fbo incompleteness'* bugs is very annoying and rather easy to do in regular GL.

Because of this, CEPL is very flexible with its `make-fbo` syntax. We will go those the general patterns below.

Let's make one now.
```lisp
CEPL>(make-fbo :c)
#<FBO COLOR-ATTACHMENTS (0)>
```
This is a way of making a fbo with one color attachment. Let's make an fbo with 2 color attachments:
```lisp
CEPL>(make-fbo 0 1)
#<FBO COLOR-ATTACHMENTS (0 1)>
```

How about with 2 color and 1 depth attachment
```lisp
CEPL> (make-fbo 0 1 :d)
#<FBO COLOR-ATTACHMENTS (0 1) DEPTH-ATTACHMENT T>
```
But we didnt specify what was in the attachments. Let's have a look and see what it is.

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

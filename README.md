# Hypergiant
Hypergiant is an OpenGL-based library for games and other interactive media applications written in CHICKEN Scheme. Its philosophy is that it should be easy to efficiently perform common operations, but it shouldn’t be hard to do anything else. Hypergiant is therefore not a framework or an engine, and doesn’t force you into any one mode of operation. Rather it’s role is to act as a glue between other graphics libraries.

The goal of Hypergiant is to make it as easy to perform simple tasks as something like LÖVE 2D or CHICKEN’s own doodle (except with Hypergiant, working in 3D is just as easy as working in 2D), while keeping the full power of OpenGL available – essentially making it possible to go from prototype to polished project with the same library.

Hypergiant should run on anything that supports OpenGL (including ES).

Note that this is an early release of Hypergiant. Some features that you might expect are missing, but there is more to come. Feel free to pass feature requests my way, though. Or better yet: patches!


## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install hypergiant`.


## Requirements
* opengl-glew
* glfw3
* gl-utils
* gl-math
* gl-type
* glls
* Hyperscene
* noise
* soil
* random-mtzig
* miscmacros
* srfi-42
* srfi-99

While Hypergiant doesn’t require any external libraries directly, opengl-glew and glfw3 depend on OpenGL, [GLEW](http://glew.sourceforge.net/), [GLFW](http://www.glfw.org/) (the most recent major version is required: 3.X). gl-type depends on [Freetype](http://www.freetype.org/)

When installing GLFW on OS X through Homebrew, an extra step is needed. Homebrew renames the library’s from the default. You can fix this by creating a link that points to the library that gets installed. E.g. `sudo ln -s <homebrew-lib-dir>/glfw3.dylib /usr/local/lib/glfw.dylib`


## Documentation
Hypergiant is a largely a glue library, intending to make the creation of real-time graphical applications easier. The main tasks that it supports are:

- Window opening and initialization, including a main loop
- Hypergiant acts as a glue between Hyperscene and glls. These two libraries were designed to work well together, making it possible to render an entire application in pure C, despite using Scheme to define the bulk of (if not all of) the rendering tasks. Hypergiant removes the boiler-plate required to use these libraries together.
- Intuitive control of input events, namely mouse and keyboard events (joystick support to come)
- Creation of geometric primitives as well as animated sprites
- [IQM](http://sauerbraten.org/iqm/) model loading and animation
- A particle system
- Simple shaders to make simple visualization easy

Hypergiant reexports (and uses) the following libraries:

- [opengl-glew](http://wiki.call-cc.org/eggref/4/opengl-glew) (prefix `gl:`): Bindings to core OpenGL or OpenGL ES
- [glls](http://wiki.call-cc.org/eggref/4/glls) (some macros modified, as noted below): Creates OpenGL Shader Language shaders in Scheme, and compiles rendering functions in C for use with the shaders
- [Hyperscene](http://wiki.call-cc.org/eggref/4/hyperscene) (some functions modified, as noted below): Scene management with a scene-graph, cameras, frustum culling, and a lighting extension (extensible only in C)
- [gl-utils](http://wiki.call-cc.org/eggref/4/gl-utils) (gl-utils-core is prefixed with `gl:`, all other modules have no prefix): Extends OpenGL to help make common operations easier
- [gl-math](http://wiki.call-cc.org/eggref/4/gl-math): Provides fast matrix, quaternion, and vector manipulation functions, suitable for use with OpenGL
- [gl-type](http://wiki.call-cc.org/eggref/4/gl-type): Loads Truetype fonts and renders them as OpenGL objects
- [soil](http://wiki.call-cc.org/eggref/4/soil): Image loading for OpenGL
- [noise](http://wiki.call-cc.org/eggref/4/noise): Noise functions that run on the GPU, created as glls shaders

Because Hypergiant reexports from all of these eggs, when the import list of one of these eggs changes, Hypergiant must be reinstalled in order to reflect the change. You can use the following command to ensure that a full update is performed:

    chicken-install opengl-glew glfw3 gl-utils gl-math glls hyperscene gl-type soil noise hypergiant

Take care with using any of the functions from these libraries that aren’t exported by Hypergiant (including glfw3). This probably means that you need to understand how those functions will interact with Hypergiant before you use them.


### Running Hypergiant applications
Hypergiant is designed to work either compiled or interpreted.

When interpreted with `csi`, Hypergiant frees up the REPL so that commands can still be entered, allowing for live-coding. This is reported to not work when the readline egg is active, although it works fine with parley.

When compiling Hypergiant, `csc FILE.scm` is usually sufficient, *unless* a pipeline has been defined (with `define-pipeline`). In this case, linking to OpenGL is needed:

- On Linux: `csc -lGL FILE.scm`
- On OS X: `csc -framework OpenGL FILE.scm`
- On Windows: `csc -lopengl32 FILE.scm`

Hypergiant is designed to work by default with OpenGL 3.3 and GLSL version 330 (and version 2 and 120, respectively for OpenGL ES). This is a relatively old standard, and even older hardware should have drivers available that support this. You can still use any versions you want by passing context-version arguments to `start`, although the pre-defined pipelines and shaders are stuck at their current versions for now. If you have any issues with this, let me know and we can try to work something out.


### Main loop and window
    [procedure] (start WIDTH HEIGHT TITLE [init: INIT] [update: UPDATE] [pre-render: PRE-RENDER] [post-render: POST-RENDER] [cleanup: CLEANUP] . WINDOW-HINTS)

Start the main body of the program, creating a new window with dimensions `WIDTH` and `HEIGHT`, and the given `TITLE`. `INIT` may be a function of zero arguments that will be called during the initialization sequence, after all libraries are initialized, but before the main loop. `UPDATE` may be a function of one argument (`delta`: the time that passed between the current update and the last one) that is called once per frame before scenes are updated and rendered. `PRE-RENDER` and `POST-RENDER` may be functions of zero arguments that perform some action immediately before and after `render-cameras` is called, respectively. `CLEANUP` may be a function of zero arguments which is called before the window is closed. `WINDOW-HINTS` accepts the same keyword arguments as [`make-window`](http://api.call-cc.org/doc/glfw3/make-window).

    [procedure] (stop)

Ends the main loop and closes the windowing, triggering any cleanup that was passed to `start`.

    [procedure] (get-window-size)

Return the size of the window as two values: width and height.

    [procedure] (get-framebuffer-size)

Return the size of the framebufffer as two values: width and height.

    [procedure] (get-time)

Return the time, in seconds, that has elapsed since `start` was called.

    [procedure] (frame-rate)

Return the current frame-rate, averaged over a number of frames. If rendering the frame rate, consider using `update-string-mesh!`.

    [procedure] (get-window-position)

Returns the `(X Y)` position (as values) of the upper left corner of the window, in screen coordinates.

    [procedure] (set-window-position X Y)

Sets the position of the upper left corner of the window, to `(X Y)` in screen coordinates.

    [procedure] (get-clipboard-string)

Returns the contents of the clipboard.

    [procedure] (set-clipboard-string STRING)

Sets the contents of the clipboard to `STRING`.


### Input
Input is managed by *bindings*: sets of keys and the actions that they are supposed to trigger. Different input methods use separate stacks for tracking which bindings are current. Bindings are represented as lists of `binding` records.

Most standard English keyboard keys are named after a code that looks like `+key-***+`. Alpha and numeric keys are named based on their character, such as `+key-a+` and `+key-1+`, with special keys being named based on the following descriptors (e.g. `+key-up+`): up, down, left, right, delete, space, backspace, tab, enter, escape, slash, backslash, period, comma, apostrophe, minus, equal, semicolon, grave-accent, right-bracket, left-bracket, insert, end, home, page-down, page-up, right-super, right-alt, right-control, right-shift, left-super, left-alt, left-control, left-shift, pause, print-screen, num-lock, scroll-lock, caps-lock, last, menu, world-2, and world-1. F keys f1 through f25 are defined, as are keypad keys kp-0 through kp-9, kp-equal, kp-enter, kp-add, kp-subtract, kp-multiply, kp-divide, and kp-decimal.

Mouse buttons `+mouse-button-middle+`, `+mouse-button-right+`, `+mouse-button-left+`, `+mouse-button-last+`, and `+mouse-button-1+` through `+mouse-button-8+` are also defined.

    [record] (binding)

The mostly-opaque binding record, which can be created with `make-binding` or `make-bindings`. List of `binding` records, are collectively referred to as *bindings*.

    [procedure] (make-binding ID KEY [scancode?: SCANCODE?] [mods: MODS] [press: PRESS] [release: RELEASE] [toggle: TOGGLE] [reverse-toggle: REVERSE-TOGGLE])

Create a binding with the identifier `ID` for the key (or button) `KEY`. `ID` may be any sort of object that can be compared with `equal?`, that should be unique for a collection of bindings. `SCANCODE?` is a boolean (defaulting to `#f`) that indicates whether `KEY` is a scancode or not (a scancode is a system+hardware-specific integer that corresponds to a particular key). `MODS` is a list of modifiers that must be held at the same time as the `KEY` for the action to take place, and may be a list containing any or all of `+mod-super+`, `+mod-alt+`, `+mod-control+`, and `+mod-shift+`. `PRESS` is a zero element function that is activated on a press event. `RELEASE` is a zero element function that is activated on a release event. `TOGGLE` and `REVERSE-TOGGLE` expect a parameter that must be an integer. `TOGGLE` indicates that when the key (plus mods) is pressed, the parameter should be incremented, and decremented when released. `REVERSE-TOGGLE` indicates the opposite. If `TOGGLE` or `REVERSE-TOGGLE` are specified, `PRESS` and `RELEASE` will be ignored, and only one of `TOGGLE` or `REVERSE-TOGGLE` may be used for a binding. 

`TOGGLE` and `REVERSE-TOGGLE` are useful when two keys are used to perform a continuous action (that may be negated). The game’s update loop should check the parameter passed to the toggle binding to see what action should be performed. For instance, if a parameter `move` is defined as `0`, and set as a toggle when the right key is pressed and reverse-toggle when the left key is pressed, the movement of a character for a given frame can be determined by multiplying `(move)` by the character’s movement speed.

    [procedure] (binding? X)

Return true when passed a binding record.

    [procedure] (binding-id BINDING)

Return the value of the `ID` of the given binding.

    [procedure] (make-bindings BINDING-LIST)

Create a list of bindings. `BINDING-LIST` should be a list of `(ID KEY [scancode?: SCANCODE?] [mods: MODS] [press: PRESS] [release: RELEASE] [toggle: TOGGLE] [reverse-toggle: REVERSE-TOGGLE])` lists. These lists should be `apply`-able to `make-binding`.

    [procedure] (add-binding BINDINGS BINDING)

Non-destructively modify the list `BINDINGS` with the new binding. `BINDING` should be a list that is `apply`-able to `make-binding`.

    [procedure] (get-binding BINDINGS ID)

Return the binding with the given `ID` from the list of `BINDINGS`.

    [procedure] (remove-binding BINDINGS ID)
Non-destructively modify the list `BINDINGS`, removing the binding with the given `ID`.

    [procedure] (change-binding BINDINGS ID BINDING)

Non-destructively modify the list `BINDINGS`, replacing the binding with the given `ID` with the new binding. `BINDING` should be a list that is `apply`-able to `make-binding`.

#### Key bindings
    [procedure] (push-key-bindings BINDINGS)

Set the currently active action that will be triggered when a key is pressed, pushing that action onto a stack. `BINDINGS` may be a list of bindings, in which case those bindings will be obeyed. `BINDINGS` may also be a function of four arguments – `(key scancode action mods)` – in which case this function is called when a key press occurs. `key` is a key code, whereas `scancode` is the corresponding scancode for the key that was pressed. `action` is one of `+press+`, `+release+`, or `+repeat+`, and `mods` is the integer formed by anding together the modifier keys – `+mod-super+`, `+mod-alt+`, `+mod-control+`, and `+mod-shift+` – that were pressed at the time of the key event.

    [procedure] (pop-key-bindings)

Return the state of the key bindings to before the last `push-key-bindings`.

    [parameter] (char-callback)

This parameter may be set to be a one argument function that is called whenever a unicode character is generated. The function will be called with an integer representing the character. Use this rather than relying on key codes when text input is desired. Note that it is often desirable to have some form of key bindings present even when the char-callback function is set (e.g. pressing escape may exit a text field), but if no other key bindings are desired, an empty list may be passed to `push-key-bindings`.

#### Mouse bindings
    [procedure] (push-mouse-bindings BINDINGS)

Set the currently active bindings that will be referenced when a mouse button is pressed, pushing those bindings onto a stack. `BINDINGS` must be a list of bindings.

    [procedure] (pop-mouse-bindings)

Return the state of the mouse bindings to before the last `push-mouse-bindings`.

#### Cursor and scrolling
    [procedure] (get-cursor-position)

Return the `(x y)` coordinates of the cursor as values, in pixels, relative to the upper-left of the window (down is the direction of positive Y).

    [procedure] (set-cursor-position X Y)

Set the position of the cursor on the window to `(X Y)`, in pixels, relative to the upper-left of the window (down is the direction of positive Y).

    [procedure] (get-cursor-world-position CAMERA)

Returns two values: the near and far coordinates ( `(x y z)` three-element f32vectors) representing the cursor position projected onto the near and far planes of the `current-camera-view-projection` of `CAMERA` (see [Scenes](#scenes)).

    [parameter] (cursor-movement-callback)

A parameter that may be set to a two argument function, which is called when the cursor is moved. The two arguments for the function represent the new x and y coordinates of the cursor, in pixels, relative to the upper-left of the window (down is the direction of positive Y).

    [parameter] (scroll-callback)

A parameter that may be set to a two argument function, which is called when the mouse wheel or track-pad is scrolled. The two arguments for the function represent the x and y distance (in pixels) that has been scrolled, respectively.


### Scenes
Hypergiant reexports most of [Hyperscene](http://wiki.call-cc.org/eggref/4/hyperscene) except for `resize-cameras`, since it manages this functionality. `resize-cameras` is handled by `start`. `add-node`, `add-light`, `make-camera`, and `set-max-lights!` are modified as described below.

Cameras are automatically resized in Hypergiant so that their projection matrix matches the bounds of the window.

    [constant] ui

A Hyperscene scene that has one orthographic camera, included for UI elements. The camera is always sized to the window and positioned such that its upper left corner is `(0 0)`. The UI scene’s camera is always rendered last.

    [parameter] resize-hooks

A parameter that contains a list of functions of two arguments, width and height, that are called every time the window is resized.

    [procedure] (make-camera TYPE STYLE SCENE [near: NEAR] [far: FAR] [angle: ANGLE] [width: WIDTH] [height: HEIGHT] [viewport-width-ratio: VIEWPORT-WIDTH-RATIO] [viewport-height-ratio: VIEWPORT-HEIGHT-RATIO] [static-viewport?: STATIC-VIEWPORT?])

Identical to Hyperscene’s `make-camera`, except `WIDTH` and `HEIGHT` default to the window’s dimensions.

Create a new camera associated with the given scene. `TYPE` must be one of `#:ortho` or `#:perspective` for an orthographic or a perspective camera, respectively. `STYLE` must be one of `#:position`, `#:look-at`, `#:orbit`, or `#:first-person`. New cameras are automatically activated. `NEAR` is the near plane of the camera, defaulting to `1`. `FAR` is the far plane of the camera, defaulting to `10000`. `ANGLE` is the view-angle, in degrees, for perspective cameras, defaulting to `70`. `WIDTH` and `HEIGHT` should be initialized to the size of camera’s viewport. `VIEWPORT-WIDTH-RATIO` and `VIEWPORT-HEIGHT-RATIO` scale the camera’s viewport (its view frustum’s near plane) in the width and height direction. The effects of the scaling persist after `resize-cameras` is called. If `STATIC-VIEWPORT?` is `#t`, the camera’s viewport dimensions will be fixed such that they won’t be changed by `resize-cameras`, although `VIEWPORT-WIDTH-RATIO` and `VIEWPORT-HEIGHT-RATIO` still effect the final viewport size.

    [procedure] (add-light PARENT COLOR INTENSITY [direction: direction] [spot-angle: SPOT-ANGLE] [position: POSITION] [radius: RADIUS])

As in Hyperscene, adds a new light to the given `PARENT` node (or scene) with `#f32(r g b)` `COLOR`. `INTENSITY` is the floating point value associated with the brightness of the light. `DIRECTION` is an `#f32(x y z)` vector that indicates the direction that the light is pointing, defaulting to `#f32(0 0 0)`. `SPOT-ANGLE` indicates the angle in radians that the light is spread over (defaulting to `0`, representing a non-spotlight source). A node is returned that can be moved, rotated, and sized like any other node.

This function is extended with `POSITION`, which sets the initial position of the light, and `RADIUS`, which sets the radius of the light’s bounding sphere.

    [procedure] (add-node PARENT PIPELINE [mesh: MESH] [vao: VAO] [mode: MODE] [n-elements: N-ELEMENTS] [element-type: ELEMENT-TYPE] [offset: OFFSET] [usage: USAGE] [draw-arrays?: DRAW-ARRAYS?] [position: POSITION] [radius: RADIUS] [data: DATA] [delete: DELETE] . UNIFORM-ARGS)

This extension of the Hyperscene function of the same name (along with Hypergiant’s extension of `define-pipline`) is where the majority of the magic of Hypergiant happens. Unlike its cousin, Hypergiant’s `add-node`’s `PIPELINE` argument accepts the special *render-pipeline* object defined by Hypergiant’s `define-pipeline` rather than a Hyperscene pipeline.  Because of this, Hyperscene pipelines never need to be manually created. When a non-render-pipeline (i.e. a Hyperscene pipeline) is passed to `add-node`, it acts identically to the Hyperscene version, except with the addition of the `POSITION` and `RADIUS` keywords, and `DATA` and `DELETE` are keyword arguments.

`POSITION` expects a gl-math point. When `POSITION` is provided, `set-node-position!` is called with `POSITION` after the node is created. `RADIUS` expects a float. When `RADIUS` is provided, `set-node-bounding-sphere!` is called with `RADIUS` after the node is created.

When `PIPELINE` is a render-pipeline the node data that is created is a [glls renderable](http://wiki.call-cc.org/eggref/4/glls#renderables) object. `MESH`, `VAO`, `MODE`, `N-ELEMENTS`, `ELEMENT-TYPE`, and `OFFSET` all function as they do when making a renderable. Additionally, `MESH` may be passed to `add-node` when its VAO has not yet been created (i.e. with [`mesh-make-vao!`](http://api.call-cc.org/doc/gl-utils/mesh-make-vao%21)), and `mesh-make-vao!` will be called automatically, influenced by the optional `USAGE` keyword (defaulting to `#:static`). `DRAW-ARRAYS?` is a boolean that indicates whether or not the renderable’s array rendering function should be used (i.e. `draw-arrays` is used instead of `draw-elements`). `DRAW-ARRAYS?` defaults to `#t` if `MESH` has no index data, and `#f` otherwise. `add-node` accepts other keyword `UNIFORM-ARGS`, which are used to set the value for each uniform in the pipeline, as required by glls renderable makers.

`add-node` appends a number of Hyperscene values to its renderable creation call, for convenience. The following keys and values are added, which must correspond to the names of uniforms in the renderable’s pipeline if they are to be used:

- `mvp: (current-camera-model-view-projection)`
- `view: (current-camera-view)`
- `projection: (current-camera-projection)`
- `view-projection: (current-camera-view-projection)`
- `camera-position: (current-camera-position)`
- `inverse-transpose-model: (current-inverse-transpose-model)`
- `n-lights: (n-current-lights)`
- `light-positions: (current-light-positions)`
- `light-colors: (current-light-colors)`
- `light-intensities: (current-light-intensities)`
- `light-directions: (current-light-directions)`
- `ambient: (current-ambient-light) `

It’s worth noting that when `add-node` is called with a mesh, no references to that node are kept. Make sure you keep your meshes live, lest they become garbage.

`add-node`, along with `define-pipeline` does some magic so that things keep working when the program is evaluated (as opposed to compiled), but the end result is that it should Just Work.


### Render-pipelines
    [macro] (define-pipeline PIPELINE-NAME . SHADERS)
    [macro] (define-alpha-pipeline PIPELINE-NAME . SHADERS)

Accepts arguments identical to glls’ [`define-pipeline`](http://api.call-cc.org/doc/glls/define-pipeline), but additionally defines a *render-pipeline* object with the name `PIPELINE-NAME-render-pipeline`. This render-pipeline object should be passed to `add-node`, and contains all the functions necessary to render the pipeline’s renderables. In other words, this creates managed Hyperscene pipeline objects that you don’t need to worry about. Additional work occurs if you evaluate (i.e. don’t compile) a program that uses `define-pipeline`.

The functions in the Hyperscene pipelines created by `define-pipeline` correspond to the begin render, render, and end render [“fast” functions](http://wiki.call-cc.org/eggref/4/glls#fast-render-functions) created by glls. Setting the `unique-textures?` parameter, to `#f` (for syntax), if a pipeline is known to use only one texture (for each sampler type), may improve speed.

`define-alpha-pipeline` works the same, but the creates a pipeline that is potentially transparent to some degree. This additional macro is necessary since Hyperscene renders alpha objects at a different stage, in a different order from opaque objects. 

    [macro] (export-pipeline . PIPELINES)

Since `define-pipeline` defines multiple objects, this macro exports everything related to each pipeline in `PIPELINES`, except for the `set-SHADER-NAME-renderable-UNIFORM!` setters. These must be exported individually.


### Pre-defined pipelines and shaders
For convenience, Hypergiant provides a number of pipelines and shaders to cover common operations. Don’t forget the each pipeline has a `NAME-render-pipeline` counterpart that should be used with `add-node`.

All of these pipelines use uniforms that are automatically added by `add-node` (notably `mvp` for the model-view-projection matrix). All of these pipelines uses the same attribute naming scheme used by Hypergiant’s [geometry](#geometry) generating functions: `position` for the position vector, `color` for a three element colour, `normal` for the normals, and `tex-coord` for texture coordinates. The uniform `tex` is used when a sampler is required.

    [constant] Pipeline: mesh-pipeline

**Attributes**

- `position` – `#:vec3`

**Uniforms**

- `mvp` – `#:mat4`
- `color` – `#:vec3`

A very basic pipeline for shading with a flat colour.

    [constant] Pipeline: color-pipeline

**Attributes**

- `position` – `#:vec3`
- `color` – `#:vec3`

**Uniforms**

- `mvp` – `#:mat4`

A pipeline with a colour associated with each vertex.

    [constant] Pipeline: texture-pipeline

**Attributes**

- `position` – `#:vec3`
- `tex-coord` – `#:vec2`

**Uniforms**

- `mvp` – `#:mat4`
- `tex` – `#:sampler-2d`

A pipeline with a 2D texture coordinate associated with each vertex.

    [constant] Pipeline: sprite-pipeline

**Attributes**

- `position` – `#:vec3`
- `tex-coord` – `#:vec2`

**Uniforms**

- `mvp` – `#:mat4`
- `tex` – `#:sampler-2d`

A pipeline with a 2D texture coordinate associated with each vertex, with possible transparency (i.e. defined with `define-alpha-pipeline`).

    [constant] Pipeline: text-pipeline

**Attributes**

- `position` – `#:vec2`
- `tex-coord` – `#:vec2`

**Uniforms**

- `mvp` – `#:mat4`
- `tex` – `#:sampler-2d`
- `color` – `#:vec3`

A pipeline for use with a single channel alpha texture, such as a texture atlas. The opaque sections are shaded with the given colour.

    [constant] Shader: phong-lighting

**Uniforms**

    ((inverse-transpose-model #:mat4)
     (camera-position #:vec3)
     (ambient #:vec3)
     (n-lights #:int)
     (light-positions (#:array #:vec3 N-LIGHTS))
     (light-colors (#:array #:vec3 N-LIGHTS))
     (light-intensities (#:array #:float N-LIGHTS))
     (material #:vec4))

Note that all of these uniforms (except `material`) are automatically provided by `add-node`, although they depend on Hyperscene’s lighting extension to be activated. E.g. `(activate-extension SCENE (lighting)`

The function `make-material` can be used to create a vector suitable for passing as the `material` uniform value.

**Exports**

    (light (SURFACE-COLOR #:vec4) (POSITION #:vec3) (NORMAL #:vec3)) -> #:vec4

This shader is designed to provided per-fragment multiple-light Phong lighting, in combination with Hyperscene’s `lighting` extension. It provides a single function used for calculating the fragment colour: `light`. Given a base, `SURFACE-COLOR` the resulting colour after lighting is calculated given the fragments’ `POSITION` and `NORMAL` (and the uniforms listed above).

When using this shader, make sure it is defined with the proper number of lights, `N-LIGHTS`, that will be used by the lighting extension with the shader. This defaults to 8, but can be set with `set-max-lights!`.

Here is an example of a simple pipeline using this shader:

``` Scheme
(define-pipeline phong-pipeline 
  ((#:vertex input: ((position #:vec3) (normal #:vec3) (tex-coord #:vec2))
             uniform: ((mvp #:mat4) (model #:mat4))
             output: ((p #:vec3) (n #:vec3) (t #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! t tex-coord)
     (set! n normal)))
  ((#:fragment input: ((n #:vec3) (p #:vec3) (t #:vec2))
               use: (phong-lighting)
               uniform: ((tex #:sampler-2d)
                         (camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (light (texture tex t) p n)))))
```

    [procedure] (set-max-lights! N)

This function replaces `set-max-lights!`, from Hyperscene. It should be used in the same way. The only difference is that this function selects an appropriate `phong-lighting` shader based on the number of lights set. The `phong-lighting` shaders differ in the array size of the uniforms, and an appropriate `N` value should be used in the uniforms of any pipeline using `phong-lighting`: the size of each uniform array should be greater than or equal to `N` and a power of 2 from 8 to 64.


    [constant] Shader: calc-bone-matrix

**Uniforms**

    ((bone-matrices (#:array #:mat4 100)))

Note that this uniform is automatically provided when creating an animated model with `add-new-animated-model`.
 
**Exports**

    (calc-bone-matrix (BONE-INDICES #:vec4) (BONE-WEIGHTS #:vec3)) -> #:mat4

This shader function calculates the matrix resulting from taking the given `BONE-INDICES` and `BONE-WEIGHTS` and summing the weighted matrices taken from the uniform `bone-matrices` array.

Here’s a simple shader using this shader:

```scheme
(define-pipeline bone-pipeline
  ((#:vertex input: ((position #:vec3) (tex-coord #:vec2)
                     (blend-indexes #:vec4) (blend-weights #:vec4))
             uniform: ((mvp #:mat4)
                       (bone-matrices (#:array #:mat4 100)))
             use: (calc-bone-matrix)
             output: ((tex-c #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp
                          (calc-bone-matrix blend-indexes
                                            blend-weights)
                          (vec4 position 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
	       output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (texture tex tex-c)))))
```


### Geometry
Hypergiant provides numerous functions for generating [gl-utils mesh](http://api.call-cc.org/doc/gl-utils#sec:gl-utils-mesh) primitives. The attributes of these meshes are all named with the same convention as the [pre-defined pipelines](#pre-defined-pipelines-and-shaders): The vertex position, normal, three element colour, and texture coordinate attributes are named `position`, `normal`, `color`, and `tex-coord`, respectively. Positions and normals are always `vec3`s, represented as floats in memory, while the type of the other attributes can be controlled with arguments.

    [procedure] (line-mesh POINTS [mode: MODE])

Create a non-indexed mesh of `POINTS`. `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:line-strip`.

    [procedure] (rectangle-mesh WIDTH HEIGHT [centered?: CENTERED?] [texture-width: TEXTURE-WIDTH] [texture-height: TEXTURE-HEIGHT] [texture-offset: TEXTURE-OFFSET] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a rectangular mesh of `WIDTH` and `HEIGHT`. If `CENTERED?` is not `#f`, the centre of the rectangle with be at the origin, otherwise the bottom left corner will be (default is `#t`). When both `TEXTURE-WIDTH` and `TEXTURE-HEIGHT` are specified, two element texture coordinates are added to the mesh, with `TEXTURE-OFFSET` representing the upper left corner, defaulting to `(0 0)`. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The rectangle mesh vertices are ordered as follows: `(upper-left upper-right lower-left lower-right)`. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. These all default to `#:ushort`.

    [procedure] (circle-mesh RADIUS RESOLUTION [texture-radius: TEXTURE-RADIUS] [texture-offset: TEXTURE-OFFSET] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a circular mesh with `RADIUS` and `RESOLUTION` triangular slices, with the centre of the circle at the origin. When `TEXTURE-RADIUS` is specified, two element texture coordinates are added to the mesh, with `TEXTURE-OFFSET` representing the centre of the circle, defaulting to `(0.5 0.5)`. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The circle mesh vertices are ordered with the first vertex at `(0 0)`, then moving in a circle starting at `(RADIUS 0)` and proceeding counter-clockwise for a total of `RESOLUTION + 1` vertices. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. These all default to `#:ushort`.

    [procedure] (cube-mesh LENGTH [normals?: NORMALS?] [cube-map?: CUBE-MAP?] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a cube mesh with sides of `LENGTH` with the centre of the cube at the origin. Each face of the cube has a unique set of vertices – i.e. no face shares vertices. When `NORMALS?` is `#t`, normals are added to the mesh. When `CUBE-MAP?` is `#t`, three element texture coordinates are added to the mesh, corresponding to a unit-cube. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a three element list of the texture coordinate at that index. The cube mesh vertices are ordered as follows: each face has vertices `(upper-left upper-right lower-left lower-right)`, and the faces are ordered `(front right back left top bottom)`. The up direction for each face can be seen in the image below:

![Cube wrapping](https://github.com/AlexCharlton/hypergiant/raw/master/doc-images/cube-mapping.png)

Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. `TEXTURE-TYPE` defaults to `#:float`, while `COLOUR-TYPE` and `INDEX-TYPE` default to `#:ushort`.


    [procedure] (sphere-mesh RADIUS RESOLUTION [type: TYPE] [normals? NORMALS?] [texture-width: TEXTURE-WIDTH] [texture-height: TEXTURE-HEIGHT] [texture-offset: TEXTURE-OFFSET] [cube-map? CUBE-MAP?] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a spherical mesh of `RADIUS` with the centre of the sphere at the origin. Depending on `TYPE`, the way the sphere is created will differ: `#:uv` will create a UV sphere – with vertices positioned on longitude/latitude points – `#:cube` will create a cube sphere – with vertices positioned on a deformed cube. The former is useful for 2D texture mapping, but will necessarily have increasingly large deformation towards the poles, and lower resolution near the equator. The latter is useful for a cube-mapped texture, for minimal deformation. The images below illustrates a UV sphere and cube sphere, respectively.

![UV sphere](https://github.com/AlexCharlton/hypergiant/raw/master/doc-images/uv-sphere.gif) ![Cube sphere](https://github.com/AlexCharlton/hypergiant/raw/master/doc-images/cube-sphere.gif)

For the UV sphere, `RESOLUTION` sets the number of longitudinal subdivisions, half of which is the number of latitudinal subdivisions (`RESOLUTION` must be a factor of 2). Setting both `TEXTURE-WIDTH` and `TEXTURE-HEIGHT` for a UV sphere causes 2 element texture coordinates to be added to the mesh, with `TEXTURE-OFFSET` representing the upper left corner of the texture, defaulting to `(0 0)`. The texture’s upper left corner is mapped to the left side of the sphere, wrapping counter-clockwise such that the left half of the texture corresponds to the front half of the sphere. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The cube mesh vertices are ordered as a rectangular array with `RESOLUTION + 1` columns and `RESOLUTION/2 + 1` rows. The first row corresponds to the “north” pole, while the last corresponds to the “south” pole. The first column has the same position as the last. This array is wrapped counter-clockwise around the sphere, starting on the left. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. 

For the cube sphere, `RESOLUTION` sets the number of vertical and horizontal subdivision of each face of the cube. Like the cube mesh, each face of the cube has a unique set of vertices – i.e. no face shares vertices. When `CUBE-MAP?` is `#t`, three element texture coordinates are added to the mesh, corresponding to a unit-sphere. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a three element list of the texture coordinate at that index. The cube mesh vertices are ordered as follows: each face is a rectangular array of `RESOLUTION + 1` by `RESOLUTION + 1` points, and the faces are ordered `(front right back left top bottom)`, the same as the `cube-mesh` faces. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. 

For both types of spheres, when `NORMALS?` is `#t`, normals are added to the mesh. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. `TEXTURE-TYPE` defaults to `#:float` for cube spheres and `#:ushort` for UV spheres, while `COLOUR-TYPE` and `INDEX-TYPE` default to `#:ushort`.

    [procedure] (cylinder-mesh LENGTH RADIUS VERTICAL-SUBDIVISIONS RESOLUTION [normals? NORMALS?] [texture-width: TEXTURE-WIDTH] [texture-height: TEXTURE-HEIGHT] [texture-offset: TEXTURE-OFFSET] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a cylindrical mesh of `LENGTH` and `RADIUS`, with the length oriented along the Y-axis, and the centre of the bottom of the cylinder at the origin. `VERTICAL-SUBDIVISIONS` sets the number of subdivisions along the length of the mesh, while `RESOLUTION` sets the number of subdivisions along the circumference. Setting both `TEXTURE-WIDTH` and `TEXTURE-HEIGHT` causes 2 element texture coordinates to be added to the mesh, with `TEXTURE-OFFSET` representing the upper left corner of the texture, defaulting to `(0 0)`. The texture’s upper left corner is mapped to the left side of the cylinder, wrapping counter-clockwise such that the left half of the texture corresponds to the front half of the cylinder. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The cube mesh vertices are ordered as a rectangular array with `RESOLUTION + 1` columns and `VERTICAL-SUBDIVISIONS + 1` rows. The first row corresponds to the top of the cylinder pole, while the last corresponds to the bottom. The first column has the same position as the last. This array is wrapped counter-clockwise around the sphere, starting on the left. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. When `NORMALS?` is `#t`, normals are added to the mesh. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. `TEXTURE-TYPE`, `COLOUR-TYPE` and `INDEX-TYPE` all default to `#:ushort`.


### Animated sprites
Hypergiant provides functions that handle the common needs for animated sprites. Such sprites use a sprite sheet – a texture that contains all of the frames of the animation, arranged in a grid. From this texture a `sprite-sheet` object is created, which is a mesh of rectangles and texture coordinates corresponding to the frames on the texture. Based on these sprite sheets, `animations` are created, which are essentially a list of frames, in the order that they should be played. A animation may be used with more than one sprite sheet (although, if the frames of each sprite sheet doesn’t correspond to each other, things will probably be weird).

    [procedure] (make-sprite-sheet TEX-WIDTH TEX-HEIGHT FRAME-WIDTH FRAME-HEIGHT [rows: ROWS] [columns: COLUMNS] [x-offset: X-OFFSET] [y-offset: Y-OFFSET] [centered?: CENTERED?] [texture-type: TEXTURE-TYPE])

Return a mesh, similar to a rectangular mesh, but with multiple rectangles with different texture coordinates. The rectangles will be sized `FRAME-WIDTH` by `FRAME-HEIGHT`. If `CENTERED?` is not `#f`, the centre of the rectangles with be at the origin, otherwise the bottom left corner will be (default is `#t`). The texture coordinates will be taken as though the rectangles were arranged in a grid over a texture of dimensions `TEX-WIDTH` by `TEX-HEIGHT`, with the number of rows and columns that can fit on these dimensions, unless `ROWS` or `COLUMNS` is specified. Offsets to the top left corner of the tiled area of the texture can be provided with `X-OFFSET` and `Y-OFFSET`. `TEXTURE-TYPE` controls the in-memory type of the texture attribute, and should be a valid argument to `type->gl`, defaulting to `#:ushort`. 

The meshes returned by `make-sprite-sheet` are intended as an argument to `make-animated-sprite` or `add-new-animated-sprite`. A single sprite sheet cannot be used with animated sprites with different textures. Consider using `mesh-copy` if you need an identical sprite sheet.

    [procedure] (make-animation TYPE FRAMES FRAME-RATE)

An animation is an opaque type that describes an animation sequence, as given in a sprite sheet. The `TYPE` may be one of `#:once` or `#:loop`, which respectively define animations that are only ever played once, or that loop. `FRAMES` is a list of integers, that define subsequent frames in the given animation, as ordered in the sprite sheet (or sheets) that is to be associated with the animation (index on the sprite sheet from left to right, top to bottom). `FRAME-RATE` is the desired speed that the animation should be played at, given in seconds between frames.

For a convenient way of defining multiple animations, see `make-animation-alist`.

    [procedure] (animation? X)

Return `#t` if `X` is an animation object.

    [procedure] (make-animation-alist ANIMATIONS [frame-rate: FRAME-RATE])

Takes an alist, `ANIMATIONS` of `(KEY . (TYPE FRAMES [ANIMATION-FRAME-RATE])`, where `TYPE` and `FRAMES` are as required for `make-animation`. `ANIMATION-FRAME-RATE` is an optional argument so long as the keyword argument `FRAME-RATE` is supplied. Returns an alist of `(KEY . ANIMATION)` pairs.

    [procedure] (make-animated-sprite NODE BASE-ANIMATION)

Create a new animated sprite, using the previously created `NODE`. `BASE-ANIMATION` is a (looping) animation that the sprite should use upon initialization. `NODE` should have been created with a sprite-sheet mesh, and with a corresponding texture. If the pipeline `sprite-pipeline` will be used for the animated sprite, consider using `add-new-animated-sprite`, which performs the node creation as well.

    [procedure] (animated-sprite? X)

Return `#t` if `X` is an animated sprite object.

    [procedure] (add-new-animated-sprite PARENT SPRITE-SHEET TEXTURE BASE-ANIMATION)

Returns a new animated sprite object that has been added to the Hyperscene node `PARENT`. The node for this sprite is created with `sprite-pipeline`. `SPRITE-SHEET` is a sprite sheet mesh that has been created with `make-sprite-sheet`. `TEXTURE` is the GL texture ID associated with the sprite sheet. `BASE-ANIMATION` is a (looping) animation that the sprite should use upon initialization.

    [procedure] (animated-sprite-node ANIMATED-SPRITE)

Return the node associated with `ANIMATED-SPRITE`.

    [procedure] (set-animation! ANIMATED-SPRITE ANIMATION)

Set the `ANIMATION` that the `ANIMATED-SPRITE` should be playing. `#:once` type animations are played once before the last looping animation resumes, while `#:loop` type animations will loop continuously until another `#:loop` animation is set. Setting an animation that is already the current animation of the `ANIMATED-SPRITE` has no effect.

    [procedure] (current-animation ANIMATED-SPRITE)

Return the animation that the `ANIMATED-SPRITE` is currently playing.

    [procedure] (update-animated-sprite! ANIMATED-SPRITE DELTA)

Update the `ANIMATED-SPRITE` given the time interval `DELTA`, changing the current frame of the sprite if enough time has elapsed since the last frame. This should be called every frame that the `ANIMATED-SPRITE` is to be animated.


### Inter-Quake Models
[Inter-Quake Models](http://sauerbraten.org/iqm/) are an open standard for rigged models, with support across a range of tools. Hypergiant supports their loading, and creating meshes and animated models with the resulting IQM objects.

    [procedure] (load-iqm IQM-FILE [BASE-IQM])

Load an IQM record from the given `IQM-FILE`. For IQM files that only provide an animation, a `BASE-IQM` is necessary in order to generate a Hypergiant animation object. When `load-iqm` is called with a `BASE-IQM`, any animations created are added to the `BASE-IQM`.

    [record] (iqm meshes vertex-arrays n-vertexes n-triangles triangles adjacencies joints animations flags comment)

The record created by `load-iqm`.

    [parameter] iqm-global-flags
An alist of flags that are recognized by `load-iqm`. Defaults to `()`.

    [parameter] vertex-array-flags
An alist of vertex-array flags that are recognized by `load-iqm`. Defaults to `()`.

    [parameter] animation-flags
An alist of animation flags that are recognized by `load-iqm`. Defaults to `((#:loop . 1))`.

    [procedure] (iqm->mesh IQM ATTRIBUTES)

Create a mesh from the given IQM object, with the provided list of ATTRIBUTES. This creates a mesh using all of the vertices of the IQM.

    [procedure] (iqm->meshes IQM ATTRIBUTES)

Create a list of mesh for each mesh defined in the given IQM object, with the provided list of ATTRIBUTES.

    [parameter] normalized-attributes
A list of attribute symbols which should be normalized when creating a mesh from an IQM (with `iqm->mesh(s)`). Defaults to `(blend-weights color)`.


### Animated models
Animated models are an opaque object similar to animated sprites, but for rigged models. Animated models are used with the same animation getting and setting functions as [animated sprites](http://wiki.call-cc.org/eggref/4/hypergiant#animated-sprites), i.e. `current-animation` and `set-animation!`. Animations suitable for animated models are created when loading IQM files containing animations (which can be referenced with `iqm-animations`).

    [procedure] (add-new-animated-model PARENT PIPELINE mesh: MESH base-animation: BASE-ANIMATION . ARGS)

Extends `add-node`, returning an animated model object. This function requires the following keyword arguments: `MESH`, a mesh corresponding to the IQM (made with e.g. `iqm->mesh`), `BASE-ANIMATION` the initial animation to use for the animated model.

    [procedure] (update-animated-model! ANIMATED-MODEL DELTA)

Update the `ANIMATED-MODEL` given the time interval `DELTA`, changing the current frame of the model if enough time has elapsed since the last frame, tweening between frames. This should be called every frame that the `ANIMATED-MODEL` is to be animated.


### Particle system
Hypergiant’s particle system is implemented as an extension to Hyperscene. It introduces two concepts: *emitters* and *particles*. Emitters are a record that is created when added to a scene (via `add-emitter`), similar to a node. These records contain a reference to the node that is used to move them around the scene. Emitters also share properties with [meshes](http://wiki.call-cc.org/eggref/4/gl-utils#gl-utils-mesh). A component of the emitter is essentially a mesh where each vertex corresponds to a particle, and the same attribute system is shared with meshes.

Emitters are used to “emit” a number of particles. These particles are created and updated via the `for-emitter` macro.


    [procedure] (particles)

Returns a pointer to the Hyperscene extension of the particle system. The extension must be activated for any scenes that will use the particle system. I.e. `(activate-extension SCENE (particles))`

    [record] emitter

The emitter record returned by `add-emitter`.

    [procedure] (emitter? X)

Return `#t` if `X` is a emitter record.

    [procedure] (emitter-node EMITTER)

Return the node of `EMITTER` that can be used to position the emitter in its scene. Do not use `delete-node` on this node: use `delete-emitter` on the emitter instead.

    [procedure] (emitter-n-particles EMITTER)

Return the current number of particles created by `EMITTER`.
    
    [procedure] (emitter-max-particles EMITTER)

Return the maximum number of particles that can be created by `EMITTER`.

    [procedure] (add-emitter PARENT PIPELINE [attributes: ATTRIBUTES] [n-particles: N-PARTICLES]  [position: POSITION] [radius: RADIUS] . UNIFORM-ARGS)

Add a new emitter to the node or scene `PARENT`, returning an emitter record. `PIPELINE` is the render-pipeline used to render the emitter.

`ATTRIBUTES` is a list of the kind that would be passed as the `ATTRIBUTES` argument in [make-mesh](http://api.call-cc.org/doc/gl-utils/make-mesh), a list in the form:

    (NAME TYPE N [normalized: NORMALIZED])

where `NAME` is the attribute name (as a symbol), `TYPE` is the type of the attribute as accepted by `type->gl`, `N` is the number of elements in the attribute, `NORMALIZED` is a boolean value indicating whether the attribute’s values should be normalized (defaulting to `#f`).

These attributes correspond to the properties of each particle. All emitters are given the attribute `(position #:float 3)`, so `ATTRIBUTES` is used to define any attributes beyond this default one.

`N-PARTICLES` is a required argument, which defines the maximum number of particles that the emitter may create. `POSITION` is used to define the initial position of the emitter, while `RADIUS` is used to define the radius of its bounding sphere. Unlike most nodes, the radius of the bounding sphere of an emitter cannot be meaningfully changed after it is created, so make sure to properly initialize it. As with `add-node`, `UNIFORM-ARGS` is the set of other keywords used to set the value for each uniform in the pipeline, as required by the [glls renderable](http://wiki.call-cc.org/eggref/4/glls#renderables) makers, specific to the `PIPELINE` that is used.

    [procedure] (delete-emitter EMITTER)

Delete the `EMITTER`, removing it from its scene.

    [macro] (for-emitter (PARTICLE-VAR EMITTER ATTRIBUTES) BEGIN UPDATE)

Iterates through the particles in `EMITTER`, assigning each one in sequence to the variable `PARTICLE-VAR` and executing `UPDATE`. The `BEGIN` form is used to perform any one-time actions that require the same scope, e.g. adding new particles. Particles added in `BEGIN` are not processed with `UPDATE`.

`ATTRIBUTES` is a list of the emitter’s attributes that should have accessors bound. Each attribute has a `(ATTRIBUTE-ref PARTICLE)` and `(ATTRIBUTE-set! PARTICLE VALUE)` function bound in the scope to the `BEGIN` and `UPDATE` forms, which are used to reference and set the attributes of the given particle. `ATTRIBUTE-ref` returns an srfi-4 vector of the type and number of the associated attribute, while `ATTRIBUTE-set!`’s `VALUE` should be an srfi-4 vector of the type and number of the attribute.

    [procedure] (add-particle EMITTER)

Return a pointer representing a new particle from `EMITTER`. This particle is not initialized in any way, and should be done so using the accessors that are bound in `for-emitter`.

    [procedure] (delete-particle EMITTER PARTICLE)

Delete the given `PARTICLE` from `EMITTER`.


### Text
Hypergiant reexports all of [gl-type](http://wiki.call-cc.org/eggref/4/gl-type) with no modifications. The following utilities are additionally provided:

    [procedure] (update-string-mesh! MESH NODE STRING FACE)

Used to modify an existing string mesh, this is similar to calling [`string-mesh`](http://api.call-cc.org/doc/gl-type/string-mesh) with the `mesh:` argument, but additionally updates the `NODE`’s renderable to properly display the new `STRING`. `MESH` should be a mesh that was created with `string-mesh`. When the mesh’s VAO is created, it must have a non-static usage (i.e. setting `add-node`’s `usage:` keyword to `#:dynamic` or `#:stream`). `NODE` is a node associated with that mesh, which must have been created with a render-pipeline. `FACE` is a font face created with [`load-face`](http://api.call-cc.org/doc/gl-type/load-face). The number of graphical characters (non-whitespace characters in the char-set of `FACE`) in `STRING` must be equal to or less than the number of graphical characters in the string used to create `MESH`.


### Colors
Some color functions are provided for convenience. These are nothing more than f32vectors, which are useful as uniform values.

    [procedure] (make-rgb-color R G B [NON-GC?])
    [procedure] (make-rgba-color R G B A [NON-GC?])

Create a three or four element f32vector with values `(R G B [A])`. If `NON-GC?` is set to `#t`, the color will be located in a non-garbage collected region of memory (so it will not be moved, although it will still be collected when non-accessible).

    [procedure] (color-r COLOR)
    [procedure] (color-g COLOR)
    [procedure] (color-b COLOR)
    [procedure] (color-a COLOR)
    [procedure] (set-color-r! COLOR)
    [procedure] (set-color-g! COLOR)
    [procedure] (set-color-b! COLOR)
    [procedure] (set-color-a! COLOR)

Getters and setters for colors. `color-a` and `set-color-a!` can only be used with 4 element (alpha) colors.

    [constant] black
    [constant] white

Two pre-defined RGB colors, with unambiguous names.


### Math
Hypergiant reexports all of [gl-math](http://wiki.call-cc.org/eggref/4/gl-math) with no modifications. The following math functions are additionally provided:

    [procedure] (random-normal [MEAN] [VARIANCE])

Return a semi-random floating point value. The values will be generated to fall in a a normal distribution around `MEAN`, defaulting to 0, with the given `VARIANCE`, defaulting to 1.

    [procedure] (random-float)

Return a random floating point value between -1 and 1.

    [procedure] (next-power-of-two N)

Returns the next power of two for the positive integer `n`.

    [procedure] (clamp X LOWER UPPER)

Given the number `X`, return a value that is the same, so long as it is not greater than `UPPER` or less than `LOWER`: in these cases, return the upper or lower bound, respectively.


#### Vector operations
The following operate on the 3 element vectors ([`point`s](http://wiki.call-cc.org/eggref/4/gl-math#vector-operations)) defined in gl-math.

    [procedure] (vclamp VECTOR LOWER UPPER [NON-GC?])
    [procedure] (vclamp! VECTOR LOWER UPPER)

Return a point with each element of `VECTOR` clamped to `LOWER` and `UPPER`. `vclamp!` destructively modifies the original vector. If `NON-GC?` is `#t`, the point is created in a non-garbage-collected area (the memory will still be freed when there are no more references to the vector).

    [procedure] (v/ VECTOR S [RESULT])

Return the result of the dividing each element of `VECTOR` with scalar `S`. If a vector `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (vround VECTOR [NON-GC?])
    [procedure] (vfloor VECTOR [NON-GC?])
    [procedure] (vceiling VECTOR [NON-GC?])
    [procedure] (vtruncate VECTOR [NON-GC?])

Return a point that has had the operation `round`, `floor`, `ceiling`, or `truncate` performed on each element of `VECTOR`. If `NON-GC?` is `#t`, the point is created in a non-garbage-collected area (the memory will still be freed when there are no more references to the vector).

## Examples
See the [examples directory](https://github.com/AlexCharlton/Hypergiant/tree/master/examples) for several examples that use Hypergiant.

The following example shows the creation and addition of a multi-coloured square to a scene:

``` scheme
(import chicken scheme)
(use hypergiant)

(define scene (make-parameter #f))
(define camera (make-parameter #f))

(define square (rectangle-mesh 1 1 color: (lambda (i)
                                            (list-ref '((1 0 0)
                                                        (0 1 0)
                                                        (0 0 1)
                                                        (1 0 1))
                                                      i))))

(define (init)
  (scene (make-scene))
  (camera (make-camera #:perspective #:orbit (scene)))
  (add-node (scene) color-pipeline-render-pipeline
            mesh: square))

(start 400 400 "Simple" init: init resizable: #f)
```

## Resources
[Prototype to polish: Making games in CHICKEN Scheme with Hypergiant](http://alex-charlton.com/posts/Prototype_to_polish_Making_games_in_CHICKEN_Scheme_with_Hypergiant/) is a blog post that details the creation of a simple game with Hypergiant.

## Version history
### Version 0.3.0
23 January 2015

Make sure Hyperscene, soil, glls, and gl-math are up to date before updating Hypergiant.

- Add pre & post-rendering keywords to `start`
- More control over camera viewports and projections (via Hyperscene)

### Version 0.2.0
18 January 2015

Make sure Hyperscene, soil, glls, gl-utils, noise, and gl-type are up to date before updating Hypergiant.

* Add particle system
* Add clipboard support
* Multisampling enabled by default
* OS X support

### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/hypergiant).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD-2-Clause

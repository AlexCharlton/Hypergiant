# Hypergiant
Hypergiant is an OpenGL-based library for games and other interactive media applications. Its philosophy is that it should be easy to (efficiently) perform common operations, but it shouldn’t be hard to do otherwise. Hypergiant is therefore not a framework, and doesn’t force you into any one mode of operation. Rather it’s based on acting as a glue between other graphics libraries.

The goal of Hypergiant is to make it as easy to perform simple tasks as something like LÖVE 2D or CHICKEN’s own doodle (except with Hypergiant, working in 3D is just as easy as working in 2D), while keeping the full power of OpenGL available – essentially making it possible to go from prototype to polished project with the same library.

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

## Documentation
Hypergiant is a glue library, intending to make the creation of real-time graphical applications easier. The main tasks that it supports are:

- Window opening and initialization, including a main loop
- Hypergiant acts as a glue between Hyperscene, glls. These two libraries were designed to work well together, making it possible to render an entire application in pure C, despite using Scheme to define the bulk of (if not all of) the rendering tasks. Hypergiant removes the boiler-plate required to use these libraries together.
- Intuitive control of input events, namely mouse and keyboard events (joystick support to come)
- Creation of geometric primitives as well as animated sprites
- Simple shaders to make simple visualization easy

Hypergiant reexports (and uses) the following libraries:

- [opengl-glew](http://wiki.call-cc.org/eggref/4/opengl-glew) (prefix `gl:`): Bindings to core-context OpenGL or OpenGL ES
- [glls](http://wiki.call-cc.org/eggref/4/glls) (some macros modified, as noted below): Creates OpenGL Shader Language shaders in Scheme, and compiles rendering functions in C for use with the shaders
- [Hyperscene](http://wiki.call-cc.org/eggref/4/hyperscene) (some functions modified, as noted below): Scene management with a scene-graph, cameras, frustum culling, and a lighting extension (extensible only in C)
- [gl-utils](http://wiki.call-cc.org/eggref/4/gl-utils) (gl-utils-core is prefiex with `gl:`, all other modules have no prefix): Extends OpenGL to help make common operations easier
- [gl-math](http://wiki.call-cc.org/eggref/4/gl-math): Provides fast matrix, quaternion, and vector manipulation functions, suitable for use with OpenGL
- [gl-type](http://wiki.call-cc.org/eggref/4/gl-type): Loads Truetype fonts and renders them as OpenGL objects
- [soil](http://wiki.call-cc.org/eggref/4/soil): Image loading for OpenGL
- [noise](http://wiki.call-cc.org/eggref/4/noise): Noise functions that run on the GPU in glls shaders

Because Hypergiant reexports from all of these eggs, when the import list of one of these eggs changes, Hypergiant must be reinstalled in order to reflect the change.

Take care with using any of the functions from these libraries that aren’t exported by Hypergiant (including glfw3). This probably means that you need to understand how those functions will interact with Hypergiant before you use them.

### Main loop and window
    [procedure] (start WIDTH HEIGHT TITLE [init: INIT] [update: UPDATE] [cleanup: CLEANUP] . WINDOW-HINTS)

Start the main body of the program, creating a new window with dimensions `WIDTH` and `HEIGHT`, and the title `TITLE`. `INIT` may be a function of zero arguments that will be called during the initialization sequence, after all libraries are initialized, but before the main loop. `UPDATE` may be a function of one argument (`delta`: the time that passed between the current update and the last one) that is called once per frame before the scene(s) is updated and rendered. `CLEANUP` may be a function of zero arguments which is called before the window is closed. `WINDOW-HINTS` accepts the same keyword arguments as [`make-window`](http://api.call-cc.org/doc/glfw3/make-window).

    [procedure] (stop)

Ends the main loop and closes the windowing, triggering any cleanup that was passed to `start`.

    [procedure] (get-window-size)

Return the size of the window as two values: width and height.

    [procedure] (get-time)

Return the time, in seconds, that has elapsed since `start` was called.

    [procedure] (frame-rate)

Return the current frame-rate, averaged over a number of frames. If rendering the frame rate, consider using `update-string-mesh!`.

    [procedure] (get-window-position)

Returns the `(X Y)` position (as values) of the upper left corner of the window, in screen coordinates.

    [procedure] (set-window-position X Y)

Sets the position of the upper left corner of the window, to `(X Y)` in screen coordinates.

### Input
Input is managed by /bindings/: sets of keys and the actions that they are supposed to trigger. Different input methods use separate stacks for tracking which bindings are current. Bindings are represented as lists of `binding` records.

Most standard English keyboard keys are named after a code that looks like `+key-***+`. Alpha and numeric keys are named based on their character, such as `key-a` and `key-1`, with special keys being named based on the following descriptors: up, down, left, right, delete, space, backspace, tab, enter, escape, slash, backslash, period, comma, apostrophe, minus, equal, semicolon, grave-accent, right-bracket, left-bracket, insert, end, home, page-down, page-up, right-super, right-alt, right-control, right-shift, left-super, left-alt, left-control, left-shift, pause, print-screen, num-lock, scroll-lock, caps-lock, last, menu, world-2, and world-1. F keys f1 through f25 are defined, as are keypad keys kp-0 through kp-9, kp-equal, kp-enter, kp-add, kp-subtract, kp-multiply, kp-divide, and kp-decimal.

Mouse buttons `+mouse-button-middle+`, `+mouse-button-right+`, `+mouse-button-left+`, `+mouse-button-last+`, and `+mouse-button-1+` through `+mouse-button-8+` are also defined.

    [record] (binding)

The mostly-opaque binding record, which can be created with `make-binding` or `make-bindings`. List of `binding` records, are collectively /bindings/.

    [procedure] (make-binding ID KEY [scancode?: SCANCODE?] [mods: MODS] [press: PRESS] [release: RELEASE] [toggle: TOGGLE] [reverse-toggle: REVERSE-TOGGLE])

Create a binding with the identifier `ID` for the key (or button) `KEY`. `ID` may be any sort of object that can be compared with `equal?`, that should be unique for a collection of bindings. `SCANCODE?` is a boolean (defaulting to `#f`) that indicates whether the key is a scancode or not (a scancode is a system&hardware-specific integer that corresponds to a particular key). `MODS` is a list of modifiers that must be held at the same time as the key for the action to take place, and may be a list containing any or all of `+mod-super+`, `+mod-alt+`, `+mod-control+`, and `+mod-shift+`. `PRESS` is a zero element function that is activated on a press event. `RELEASE` is a zero element function that is activated on a release event. `TOGGLE` and `REVERSE-TOGGLE` expect a parameter that must be an integer. `TOGGLE` indicates that when the key (plus mods) is pressed, the parameter should be incremented, and decremented when released. `REVERSE-TOGGLE` indicates the opposite. If `TOGGLE` or `REVERSE-TOGGLE` are specified, `PRESS` and `RELEASE` will be ignored, and only one of `TOGGLE` or `REVERSE-TOGGLE` may be used for a binding. 

`TOGGLE` and `REVERSE-TOGGLE` are useful when two keys are used to perform a continuous action (that may be negated). The game’s update loop should check the parameter passed to the toggle binding to see what action should be performed. For instance, if a parameter `move` is defined as `0`, and set as a toggle when the right key is pressed and reverse-toggle when the left key is pressed, the movement of a character for a given frame can be determined by multiplying `(move)` by the character’s movement speed.

    [procedure] (binding? X)

Return true when passed a binding record.

    [procedure] (binding-id BINDING)

Return the value of the `ID` of the given binding.

    [procedure] (make-bindings BINDING-LIST)

Create a list of bindings. `BINDING-LIST` should be a list of `BINDING` lists. Each `BINDING` list should be `apply`able to `make-binding`.

    [procedure] (add-binding BINDINGS BINDING)

Non-destructively modify the list `BINDINGS` with the new binding. `BINDING` should be a list that is `apply`able to `make-binding`.

    [procedure] (get-binding BINDINGS ID)

Return the binding with the given `ID` from the list of `BINDINGS`.

    [procedure] (remove-binding BINDINGS ID)
Non-destructively modify the list `BINDINGS`, removing the binding with the given `ID`.

    [procedure] (change-binding BINDINGS ID BINDING)

Non-destructively modify the list `BINDINGS`, replacing the binding with the given `ID` with the new binding. `BINDING` should be a list that is `apply`able to `make-binding`.

#### Key bindings
    [procedure] (push-key-bindings BINDINGS)

Set the currently active action that will occur when a key is pressed, pushing that action onto a stack. `BINDINGS` may be a list of bindings, in which case those bindings will be obeyed. `BINDINGS` may also be a function of four arguments – `(key scancode action mods)` – in which case this function is called when a key press occurs. `key` is a key code, whereas `scancode` is the corresponding scancode for the key that was pressed. `action` is one of `+press+`, `+release+`, or `+repeat+`, and `mods` is the integer formed by anding together the modifier keys – `+mod-super+`, `+mod-alt+`, `+mod-control+`, and `+mod-shift+` – that were pressed at the time of the key event.

    [procedure] (pop-key-bindings)

Return the state of the key bindings to before the last `push-key-bindings!`.

    [parameter] (char-callback)

This parameter may be set to be a one argument function that is called whenever a unicode character is generated. The function will be called with an integer representing the character. Use this rather than relying on key codes when text input is desired. Note that it is often desirable to have some form of key bindings present even when the char-callback function is set (e.g. pressing escape may exit a text field), but if no other key bindings are desired, an empty list may be passed to `push-key-bindings!`.

#### Mouse bindings
    [procedure] (push-mouse-bindings BINDINGS)

Set the currently active action that will occur when a mouse button is pressed, pushing that action onto a stack. `BINDINGS` must be a list of bindings.

    [procedure] (pop-mouse-bindings)

Return the state of the mouse bindings to before the last `push-mouse-bindings!`.

#### Cursor and scrolling
    [procedure] (get-cursor-position)

Return the (x y) coordinates of the cursor as values, in pixels, relative to the upper-left of the window (down is the direction of positive-y).

    [procedure] (set-cursor-position X Y)

Set the position of the cursor on the window to `(X Y)`, in pixels, relative to the upper-left of the window (down is the direction of positive-y).

    [procedure] (get-cursor-world-position)

Returns two values: the near and far coordinates (three element f32vectors) representing the cursor position projected onto the near and far planes of the `current-camera-view-projection` (see [Scenes](#scenes)).

    [parameter] (cursor-movement-callback)

A parameter that must be set to a two argument function, which is called when the cursor is moved. The two arguments for the function represent the new x and y coordinates of the cursor, in pixels, relative to the upper-left of the window (down is the direction of positive-y).

    [parameter] (scroll-callback)

A parameter that must be set to a two argument function, which is called when the mouse wheel or track-pad is scrolled. The two arguments for the function represent the x and y distance (in pixels) that has been scrolled, respectively.

### Scenes
Hypergiant reexports most of [Hyperscene](http://wiki.call-cc.org/eggref/4/hyperscene) except for `init`, `add-pipeline` `delete-pipeline`, and `resize-cameras`, as it manages this functionality. `init` and `resize-cameras` are handled by `start` while `add/delete-pipeline` are handled by Hypergiant’s `define-pipline`. `add-node` is modified as described below.

    [constant] ui

A Hyperscene scene that has one orthographic camera, included for UI elements. The camera is always sized to the window and positioned such that its upper left corner is `(0 0)`. The UI scene’s camera is always rendered last.

    [parameter] resize-hooks

A parameter that contains functions of two arguments, width and height, that are called every time the window is resized.

    [procedure] (add-node PARENT PIPELINE [mesh: MESH] [vao: VAO] [mode: MODE] [n-elements: N-ELEMENTS] [element-type: ELEMENT-TYPE] [offset: OFFSET] [usage: USAGE] [draw-arrays?: DRAW-ARRAYS?] [position: POSITION] [radius: RADIUS] . ARGS)

This extension of the Hyperscene function of the same name (along with Hypergiant’s extension of `define-pipline`) is where the majority of the magic of Hypergiant happens. Unlike its cousin, Hypergiant’s `add-node`’s `PIPELINE` argument accepts the special `*-render-pipeline` object defined by Hypergiant’s `define-pipeline` rather than a Hyperscene pipeline.  Because of this, Hyperscene pipelines never need to be manually created. When a non-render-pipeline (i.e. a Hyperscene pipeline) is passed to `add-node`, it acts identically to the Hyperscene version, except for the addition of `POSITION` and `RADIUS`.

`POSITION` expects a gl-math point. When `POSITION` is provided, `set-node-position!` is called with `POSITION` after the node is created. `RADIUS` expects a float. When `RADIUS` is provided, `set-node-bounding-sphere!` is called with `RADIUS` after the node is created.

When `PIPELINE` is a render-pipeline the node data that is created is a glls [renderable](http://wiki.call-cc.org/eggref/4/glls#renderables) object. `MESH`, `VAO`, `MODE`, `N-ELEMENTS`, `ELEMENT-TYPE`, and `OFFSET` all function as they do when making a renderable. Additionally, `MESH` may be passed to `add-node` when its VAO has not yet been created (i.e. with [`mesh-make-vao`](http://api.call-cc.org/doc/gl-utils/mesh-make-vao%21)), and `mesh-make-vao!` will be called automatically, influenced by the optional `USAGE` keyword (defaulting to `+static`). `DRAW-ARRAYS?` is a boolean that indicates whether or not the renderable’s array rendering function should be used (i.e. `draw-arrays` is used instead of `draw-elements`). `DRAW-ARRAYS?` defaults to `#t` if `MESH` has no index data, and `#f` otherwise. `add-node` accepts other keyword `ARGS`, which are used to set the value for each uniform in the pipeline, as required by glls renderable makers.

`add-node` appends a number of Hyperscene values to its renderable creation call, for convenience. Each variable is combined with the following keys (which may correspond to the names of uniforms in the renderable’s pipeline):

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

Accepts arguments identical to glls’ [`define-pipeline`](http://api.call-cc.org/doc/glls/define-pipeline), but additionally defines a render-pipeline object with the name `PIPELINE-NAME-render-pipeline`. This render-pipeline object should be passed to `add-node`, and contains all the functions necessary to render the pipeline’s renderables. In other words, this creates managed Hyperscene pipeline objects that you don’t need to worry about. Additional stuff is going on if you evaluate (i.e. don’t compile) the program.

The functions in the Hyperscene pipelines created by `define-pipeline` correspond to the begin render, render, and end render [“fast” functions](http://wiki.call-cc.org/eggref/4/glls#fast-render-functions) created by glls. Setting the `unique-textures?` parameter, to `#f` (for syntax), if a pipeline is known to use only one texture (for each sampler type), may improve speed.

`define-alpha-pipeline` works the same, but the creates a pipeline that is potentially transparent to some degree. This additional macro is necessary since Hyperscene renders alpha objects at a different stage, in a different order from opaque objects. 

    [macro] (export-pipeline . PIPELINES)

Since `define-pipeline` defines multiple objects, this macro exports everything related to each pipeline in `PIPELINES`, except for the `set-SHADER-NAME-renderable-UNIFORM!` setters. These must be exported individually.

### Pre-defined pipelines and shaders
For convenience, Hypergiant provides a number of pipelines and shaders to cover common operations. Don’t forget the each pipeline has a `NAME-render-pipeline` counterpart that should be used with `add-node`.

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

    [constant] Shader: text-pipeline

**Attributes**

- `position` – `#:vec2`
- `tex-coord` – `#:vec2`

**Uniforms**

- `mvp` – `#:mat4`
- `tex` – `#:sampler-2d`
- `color` – `#:vec3`

A pipeline for use with a mono-chrome alpha texture, such as a texture atlas. The opaque sections are shaded with the given colour.

    [constant] phong-lighting

**Uniforms**

- `camera-position` – `#:vec3`
- `ambient` – `#:vec3`
- `n-lights` – `#:int`
- `light-positions` – `(#:array #:vec3 N-LIGHTS)`
- `light-colors` – `(#:array #:vec3 N-LIGHTS)`
- `light-intensities` – `(#:array #:float N-LIGHTS)`
- `material` – `#:vec4`

Note that all of these uniforms are automatically provided by `add-node`, although they depend on Hyperscene’s lighting extension to be activated. E.g. `(activate-extension SCENE (lighting)`

**Exports**
    (light (SURFACE-COLOR #:vec4) (POSITION #:vec3) (NORMAL #:vec3)) -> #:vec4

This shader is designed to provided per-fragment Phong lighting, and provides a single function used for calculating the fragment colour: `light`. Given a base, `SURFACE-COLOR` the resulting colour after lighting is calculated given the fragments’ `POSITION` and `NORMAL` (and the uniforms listed above).

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
               uniform: ((camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (tex #:sampler-2d)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (light (texture tex t) p
                             (normalize (* (mat3 inverse-transpose-model) n)))))))
```

    [procedure] (set-max-lights! N)

This function replaces `set-max-lights!`, from Hyperscene. It should be used in the same way. The only difference is that this function selects an appropriate `phong-lighting` shader based on the number of lights set. The `phong-lighting` shaders differ in the array size of the uniforms, and an appropriate `N` value should be used in the uniforms of any pipeline using `phong-lighting`: the size of each uniform array should be greater than or equal to `N` and a power of 2 from 8 to 64.

### Geometry
Hypergiant provides numerous functions for generating [gl-utils meshe](http://api.call-cc.org/doc/gl-utils#sec:gl-utils-mesh) primitives. The attributes of these meshes are all named with the same convention as the [pre-defined pipelines](#pre-defined-pipelines-and-shaders): The vertex position, normal, three element colour, and texture coordinate attributes are named `position`, `normal`, `color`, and `tex-coord`, respectively. Positions and normals are always `vec3`s, represented as floats in memory, while the type of the other attributes can be controlled with arguments.

    [procedure] (line-mesh POINTS [mode: MODE])

Create a non-indexed mesh of `POINTS`. `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:line-strip`.

    [procedure] (rectangle-mesh WIDTH HEIGHT [centered?: CENTERED?] [texture-width: TEXTURE-WIDTH] [texture-height: TEXTURE-HEIGHT] [texture-offset: TEXTURE-OFFSET] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a rectangular mesh of `WIDTH` and `HEIGHT`. If `CENTERED?` is not `#f`, the centre of the rectangle with be at the origin, otherwise the bottom left corner will be (default is `#t`). When both `TEXTURE-WIDTH` and `TEXTURE-HEIGHT` are specified, two element texture coordinates are added to the mesh, with `TEXTURE-OFFSET` representing the upper left corner, defaulting to `(0 0)`. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The rectangle mesh vertices are ordered as follows: `(upper-left upper-right lower-left lower-right)`. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. These all default to `#:ushort`.

    [procedure] (circle-mesh RADIUS RESOLUTION [texture-radius: TEXTURE-RADIUS] [texture-offset: TEXTURE-OFFSET] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a circular mesh with `RADIUS` and `RESOLUTION` triangular slices, with the centre of the circle at the origin. When `TEXTURE-RADIUS` is specified, two element texture coordinates are added to the mesh, with `TEXTURE-OFFSET` representing the centre of the circle, defaulting to `(0.5 0.5)`. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The circle mesh vertices are ordered starting at `(RADIUS 0)`, then proceeding counter-clockwise for a total of `RESOLUTION` vertices. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. These all default to `#:ushort`.

    [procedure] (cube-mesh LENGTH [normals?: NORMALS?] [cube-map?: CUBE-MAP?] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a cube mesh with sides of `LENGTH` with the centre of the cube at the origin. Each face of the cube has a unique set of vertices – i.e. no face shares vertices. When `NORMALS?` is `#t`, normals are added to the mesh. When `CUBE-MAP?` is `#t`, three element texture coordinates are added to the mesh, corresponding to a unit-cube. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a three element list of the texture coordinate at that index. The cube mesh vertices are ordered as follows: each face has vertices `(upper-left upper-right lower-left lower-right)`, and the faces are ordered `(front right back left top bottom)`. The up direction for each face can be seen in the image below:

![Cube wrapping](https://github.com/AlexCharlton/hypergiant/raw/master/doc-images/cube-mapping.png)

Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. `TEXTURE-TYPE` defaults to `#:float`, while `COLOUR-TYPE` and `INDEX-TYPE` default to `#:ushort`.


    [procedure] (sphere-mesh RADIUS RESOLUTION [type: TYPE] [normals? NORMALS?] [texture-width: TEXTURE-WIDTH] [texture-height: TEXTURE-HEIGHT] [texture-offset: TEXTURE-OFFSET] [cube-map? CUBE-MAP?] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a spherical mesh of `RADIUS` with the centre of the sphere at the origin. Depending on `TYPE`, the way the sphere is created will differ: `#:uv` will create a UV sphere – with vertices positioned on longitude/latitude points – `#:cube` will create a cube sphere – with vertices positioned on a deformed cube. The former is useful for 2D texture mapping, but will necessarily have increasingly large deformation towards the poles, and lower resolution near the equator. The latter is useful for a cube-mapped texture, for minimal deformation. The images below illustrates a UV sphere and cube sphere, respectively.

![UV sphere](https://github.com/AlexCharlton/hypergiant/raw/master/doc-images/uv-sphere.gif) ![Cube sphere](https://github.com/AlexCharlton/hypergiant/raw/master/doc-images/uv-sphere.gif)

For the UV sphere, `RESOLUTION` sets the number of longitudinal subdivisions, half of which is the number of latitudinal subdivisions (`RESOLUTION` must be a factor of 2). Setting both `TEXTURE-WIDTH` and `TEXTURE-HEIGHT` for a UV sphere causes 2 element texture coordinates to be added to the mesh, with `TEXTURE-OFFSET` representing the upper left corner of the texture, defaulting to `(0 0)`. The texture’s upper left corner is mapped to the left side of the sphere, wrapping counter-clockwise such that the left half of the texture corresponds to the front half of the sphere. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The cube mesh vertices are ordered as a rectangular array with `RESOLUTION + 1` columns and `RESOLUTION/2 + 1` rows. The first row corresponds to the “north” pole, while the last corresponds to the “south” pole. The first column has the same position as the last. This array is wrapped counter-clockwise around the sphere, starting on the left. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. 

For the cube sphere, `RESOLUTION` sets the number of vertical and horizontal subdivision of each face of the cube. Like the cube mesh, each face of the cube has a unique set of vertices – i.e. no face shares vertices. When `CUBE-MAP?` is `#t`, three element texture coordinates are added to the mesh, corresponding to a unit-sphere. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a three element list of the texture coordinate at that index. The cube mesh vertices are ordered as follows: each face is a rectangular array of `RESOLUTION + 1` by `RESOLUTION + 1` points, and the faces are ordered `(front right back left top bottom)`, the same as the cube-mesh faces. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. 

For both types of spheres, when `NORMALS?` is `#t`, normals are added to the mesh. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. `TEXTURE-TYPE` defaults to `#:float` for cube spheres and `#:ushort` for UV spheres, while `COLOUR-TYPE` and `INDEX-TYPE` default to `#:ushort`.

    [procedure] (cylinder-mesh LENGTH RADIUS VERTICAL-SUBDIVISIONS RESOLUTION [normals? NORMALS?] [texture-width: TEXTURE-WIDTH] [texture-height: TEXTURE-HEIGHT] [texture-offset: TEXTURE-OFFSET] [texture: TEXTURE] [color: COLOR] [winding: WINDING] [mode: MODE] [texture-type: TEXTURE-TYPE] [color-type: COLOR-TYPE] [index-type: INDEX-TYPE])

Create a cylindrical mesh of `LENGTH` and `RADIUS`, with the length oriented along the Y-axis, and the centre of the bottom of the cylinder at the origin. `VERTICAL-SUBDIVISIONS` sets the number of subdivisions along the length of the mesh, while `RESOLUTION` sets the number of subdivisions along the circumference. Setting both `TEXTURE-WIDTH` and `TEXTURE-HEIGHT` causes 2 element texture coordinates to be added to the mesh, with `TEXTURE-OFFSET` representing the upper left corner of the texture, defaulting to `(0 0)`. The texture’s upper left corner is mapped to the left side of the cylinder, wrapping counter-clockwise such that the left half of the texture corresponds to the front half of the cylinder. Alternately, `TEXTURE` may be supplied, which expects a function of one argument: the index of a vertex. This texture function should return a two element list of the texture coordinate at that index. The cube mesh vertices are ordered as a rectangular array with `RESOLUTION + 1` columns and `VERTICAL-SUBDIVISIONS + 1` rows. The first row corresponds to the top of the cylinder pole, while the last corresponds to the bottom. The first column has the same position as the last. This array is wrapped counter-clockwise around the sphere, starting on the left. Likewise, `COLOR` expects a similar function that accepts one index as an argument, but should return a three element list of colour values. When `NORMALS?` is `#t`, normals are added to the mesh. `WINDING` controls the direction of the vertex winding, either counter-clockwise (`#:ccw`, the default), or clockwise (`#:cw`). `MODE` should be a valid argument to `mode->gl`,  defaulting to `#:triangles`. `TEXTURE-TYPE`, `COLOUR-TYPE`, and `INDEX-TYPE` control the in-memory type of the texture attribute, the color attribute, and the index, and should be a valid argument to `type->gl`. `TEXTURE-TYPE`, `COLOUR-TYPE` and `INDEX-TYPE` all default to `#:ushort`.

### Animated sprites
Hypergiant provides functions that handle the common needs for animated sprites. Such sprites use a sprite sheet – a texture that contains all of the frames of the animation, arranged in a grid. From this texture a `sprite-sheet` object is created, which is a mesh of rectangles and texture coordinates corresponding to the frames on the texture. Based on these sprite sheets, `animations` are created, which are essentially a list of frames, in the order that they should be played. A animation may be used with more than one sprite sheet (although, if the frames of each sprite sheet doesn’t correspond to each other, things will probably be weird).

    [procedure] (make-sprite-sheet TEX-WIDTH TEX-HEIGHT FRAME-WIDTH FRAME-HEIGHT [rows: ROWS] [columns: COLUMNS] [x-offset: X-OFFSET] [y-offset: Y-OFFSET] [centered?: CENTERED?] [texture-type: TEXTURE-TYPE])

Return a mesh, similar to a rectangular mesh, but with multiple rectangles with different texture coordinates. The rectangles will be sized `FRAME-WIDTH` by `FRAME-HEIGHT`. If `CENTERED?` is not `#f`, the centre of the rectangles with be at the origin, otherwise the bottom left corner will be (default is `#t`). The texture coordinates will be taken as though the rectangles were arranged in a grid over a texture of dimensions `TEX-WIDTH` by `TEX-HEIGHT`, with the number of rows and columns that can fit on these dimensions, unless `ROWS` or `COLUMNS` is specified. Offsets to the top left corner of the tiled area of the texture can be provided with `X-OFFSET` and `Y-OFFSET`. `TEXTURE-TYPE` controls the in-memory type of the texture attribute, and should be a valid argument to `type->gl`, defaulting to `#:ushort`. 

The meshes returned by `make-sprite-sheet` are intended as an argument to `add-new-animated-sprite`. A single sprite sheet cannot be used with animated sprites with different textures. Consider using `mesh-copy` if you need an identical sprite sheet.

    [procedure] (make-animation TYPE FRAMES FRAME-RATE)
    [procedure] (animation? X)

An animation is an opaque type that describes an animation sequence, as given in a sprite sheet. The `TYPE` may be one of `#:once` or `#:loop`, which respectively define animations that are only ever played once, or that loop. `FRAMES` is a list of integers, that define subsequent frames in the given animation, as ordered in the sprite sheet (or sheets) that is to be associated with the animation (from left to right, top to bottom). `FRAME-RATE` is the desired speed that the animation should be played at, given in seconds between frames.

For a convenient way of defining multiple animations, see `make-animation-alist`.

    [procedure] (make-animation-alist ANIMATIONS [frame-rate: FRAME-RATE])

Takes an alist, `ANIMATIONS` of `(KEY . (TYPE FRAMES [ANIMATION-FRAME-RATE])`, where `TYPE` and `FRAMES` are as required for `make-animation`. `ANIMATION-FRAME-RATE` is an optional argument so long as the keyword argument `FRAME-RATE` is supplied. Returns an alist of `(KEY . ANIMATION)` pairs.

    [procedure] (add-new-animated-sprite PARENT SPRITE-SHEET TEXTURE BASE-ANIMATION)
    [procedure] (animated-sprite? X)

Returns a new animated sprite object that has been added to the Hyperscene node `PARENT`. `SPRITE-SHEET` is a sprite sheet mesh that has been created with `make-sprite-sheet`. `TEXTURE` is the GL texture ID associated with the sprite sheet. `BASE-ANIMATION` is a (looping) animation that the sprite should use upon initialization.

    [procedure] (animated-sprite-node ANIMATED-SPRITE)

Return the node associated with `ANIMATED-SPRITE`.

    [procedure] (set-animation! ANIMATED-SPRITE ANIMATION)

Set the `ANIMATION` that the `ANIMATED-SPRITE` should be playing. `#:once` type animations are played once before the last looping animation resumes, while `#:loop` type animations will loop continuously until another `#:loop` animation is set. Setting an animation that is already set for the `ANIMATED-SPRITE` has no effect.

    [procedure] (current-animation ANIMATED-SPRITE)

Return the animation that the `ANIMATED-SPRITE` is currently playing.

    [procedure] (update-animated-sprite! ANIMATED-SPRITE DELTA)

Update the `ANIMATED-SPRITE` given the time interval `DELTA`. This should be called every frame that the `ANIMATED-SPRITE` is to be animated.

### Text
Hypergiant reexports all of [gl-type](http://wiki.call-cc.org/eggref/4/gl-type) with no modifications. The following utilities are additionally provided:

    [procedure] (update-string-mesh! MESH RENDERABLE STRING FACE)

Used to modify an existing string mesh, this is similar to calling [`string-mesh`](http://api.call-cc.org/doc/gl-type/string-mesh) with the `mesh:` argument, but additionally updates the `RENDERABLE` to properly display the new `STRING`. `MESH` should be a mesh that was created with `string-mesh`. When the mesh’s VAO is created, it must have a non-static usage (i.e. setting `add-node`’s `usage:` keyword). `RENDERABLE` is a renderable associated with that mesh. `FACE` is a font face created with [`load-face`](http://api.call-cc.org/doc/gl-type/load-face). The number of graphical characters (non-whitespace characters in the char-set of `FACE`) in `STRING` must be equal to or less than the number of graphical characters in the string used to create `MESH`.

### Math
Hypergiant reexports all of [gl-math](http://wiki.call-cc.org/eggref/4/gl-math) with no modifications. The following math functions are additionally provided:

    [procedure] (random-normal [MEAN] [VARIANCE])

Return a semi-random floating point value. The values will be generated to fall in a a normal distribution around `MEAN`, defaulting to 0, with the given `VARIANCE`, defaulting to 1.

    [procedure] (random-float)

Return a random floating point value between -1 and 1.

    [procedure] (clamp X LOWER UPPER)

Given the number `X`, return a value that is the same, so long as it is not greater than `UPPER` or less than `LOWER`: in these cases, return the upper or lower bound, respectively.

#### Vector operations
The following operate on the 3 element vectors (`point`s) defined in gl-math.

    [procedure] (vclamp VECTOR LOWER UPPER)
    [procedure] (vclamp! VECTOR LOWER UPPER)

Return a point with each element of `VECTOR` clamped to `LOWER` and `UPPER`. `vclamp!` destructively modifies the original vector.

    [procedure] (v/ VECTOR S [RESULT])

Return the result of the dividing each element of `VECTOR` with scalar `S`. If a vector `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (vround VECTOR)
    [procedure] (vfloor VECTOR)
    [procedure] (vceiling VECTOR)
    [procedure] (vtruncate VECTOR)

Return a point that has had the operation `round`, `floor`, `ceiling`, or `truncate` performed on each element of `VECTOR`.

## Examples

## Version history
### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/hypergiant).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD-2-Clause

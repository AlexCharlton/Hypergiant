# Hypergiant
Hypergiant is a library for games and other interactive media applications. Its philosophy is that it should be easy to (efficiently) perform common operations, but it shouldn’t be hard to do otherwise. Hypergiant is therefore not a framework, and doesn’t force you into any one mode of operation. Rather it’s based on acting as a glue between other graphics libraries.

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
Reexports: opengl-glew (prefix `gl:`), gl-utils (gl-utils-core is prefiex with `gl:`, all other modules have no prefix), gl-math, gl-type, noise, and soil. glls and most of Hyperscene are rexported with several enhanced functions, as noted below.

Take care with using any of the functions from these libraries that aren’t exported by Hypergiant (including glfw3). This probably means that you need to know how those functions will interact with Hypergiant before you doing anything. Or it might mean that I forgot to export something ;)

### Main loop
    [procedure] (start WIDTH HEIGHT TITLE [init: INIT] [update: UPDATE] [cleanup: CLEANUP] . WINDOW-HINTS)

Start the main body of the program, creating a new window with dimensions `WIDTH` and `HEIGHT`, and the title `TITLE`. `INIT` may be a function of zero arguments that will be called during the initialization sequence, after all libraries are initialized, but before the main loop. `UPDATE` may be a function of one argument (`delta`: the time that passed between the current update and the last one) that is called once per frame before the scene(s) is updated and rendered. `CLEANUP` may be a function of zero arguments which is called before the window is closed. `WINDOW-HINTS` accepts the same keyword arguments as [`make-window`](http://api.call-cc.org/doc/glfw3/make-window).

    [procedure] (stop)

Ends the main loop and closes the windowing, triggering any cleanup that was passed to `start`.

    [procedure] (get-window-size)

Return the size of the window as two values: width and height.

    [procedure] (get-time)

Return the time, in seconds, that has elapsed since `start` was called.

### Input
Input is managed by /bindings/: sets of keys and the actions that they are supposed to trigger. Different input methods use separate stacks for tracking which bindings are current. Bindings are represented as lists of `binding` records.

Most standard English keyboard keys are named after a code that looks like `+key-***+`. Alpha and numeric keys are named based on their character, such as `key-a`, with special keys being named based on the following descriptors: up, down, left, right, delete, space, backspace, tab, enter, escape, slash, backslash, period, comma, apostrophe, minus, equal, semicolon, grave-accent, right-bracket, left-bracket, insert, end, home, page-down, page-up, right-super, right-alt, right-control, right-shift, left-super, left-alt, left-control, left-shift, pause, print-screen, num-lock, scroll-lock, caps-lock, last, menu, world-2, and world-1. F keys f1 through f25 are defined, as are keypad keys kp-0 through kp-9, kp-equal, kp-enter, kp-add, kp-subtract, kp-multiply, kp-divide, and kp-decimal.

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
Hypergiant reexports most of Hyperscene except for `init`, `add-pipeline` `delete-pipeline`, and `resize-cameras`, as it manages this functionality. `init` and `resize-cameras` are handled by `start` while `add/delete-pipeline` are handled by Hypergiant’s `define-pipline`. `add-node` is modified as described below.

    [procedure] (add-node PARENT RENDER-PIPELINE [mesh: MESH] [vao: VAO] [mode: MODE] [n-elements: N-ELEMENTS] [element-type: ELEMENT-TYPE] [offset: OFFSET] [usage: USAGE] [draw-arrays?: DRAW-ARRAYS?])

This extension of the Hyperscene function of the same name (along with Hypergiant’s extension of `define-pipline`) is where the majority of the magic of Hypergiant happens. Unlike its cousin, Hypergiant’s `add-node`’s `RENDER-PIPELINE` argument accepts the special `*-render-pipeline` object defined by Hypergiant’s `define-pipeline` rather than a Hyperscene pipeline.  Because of this, Hyperscene pipelines never need to be manually created.

The node data that is created is therefore a glls [renderable](http://wiki.call-cc.org/eggref/4/glls#renderables) object. `MESH`, `VAO`, `MODE`, `N-ELEMENTS`, `ELEMENT-TYPE`, and `OFFSET` all function as they do when making a renderable. Additionally, `MESH` may be passed to `add-node` when its VAO has not yet been created (i.e. with [`mesh-make-vao`](http://api.call-cc.org/doc/gl-utils/mesh-make-vao%21)), and `mesh-make-vao!` will be called automatically, influenced by the optional `USAGE` keyword (defaulting to `+static`). `DRAW-ARRAYS?` is a boolean that indicates whether or not the renderable’s array rendering function should be used (i.e. `draw-arrays` is used instead of `draw-elements`). `DRAW-ARRAYS?` defaults to `#t` if `MESH` has no index data, and `f` otherwise.

`add-node`, along with `define-pipeline` also does some magic so that things keep working when the program is evaluated (as opposed to compiled), but that’s all we’re going to say about that.

It’s worth noting that when `add-node` is called with a mesh, no references to that node are kept. Make sure you keep your meshes live, lest they become garbage.

### Render-pipelines
    [macro] (define-pipeline PIPELINE-NAME . SHADERS)
    [macro] (define-alpha-pipeline PIPELINE-NAME . SHADERS)

Accepts arguments identical to glls’ [`define-pipeline`](http://api.call-cc.org/doc/glls/define-pipeline), but additionally defines a render-pipeline object with the name `PIPELINE-NAME-render-pipeline`. This render-pipeline object should be passed to `add-node`, and contains all the functions necessary to render the pipeline’s renderables. In other words, this creates managed Hyperscene pipeline objects that you don’t need to worry about. Additional stuff is going if you evaluate (i.e. don’t compile) the program.

`define-alpha-pipeline` works the same, but the creates a pipeline that is potentially transparent to some degree. This additional macro is necessary since Hyperscene renders alpha objects at a different stage, in a different order from opaque objects. 

    [macro] (export-pipeline . PIPELINES)

Since `define-pipeline` defines multiple objects, this macro exports everything related to each pipeline in `PIPELINES`.

### Pre-defined pipelines and shaders

### Geometry

### Math
gl-math

(random-normal [MEAN] [VARIANCE])
(random-float)
(clamp X LOWER UPPER)

#### Vectors
The following operate on the 3 element vectors defined in gl-math.

(vclamp VECTOR LOWER UPPER)
(vclamp! VECTOR LOWER UPPER)
(v/ VECTOR S)
(vround VECTOR)
(vfloor VECTOR)
(vceiling VECTOR)
(vtruncate VECTOR)

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
BSD

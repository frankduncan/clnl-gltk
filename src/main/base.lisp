(in-package #:clnl-gltk)

(defgeneric render (item)
 (:documentation
  "RENDER ITEM => RESULT

ARGUMENTS AND VALUES:

  ITEM: item to be rendered
  RESULT: undefined

DESCRIPTION:

  RENDER is the entry point for rendering different items in the
  CLNL-GLTK package.

  RENDER will return the opengl world to the way it found it after
  finishing, usually via just popping the matrix."))

(defgeneric resize (item width height)
 (:documentation
  "RESIZE ITEM WIDTH HEIGHT => RESULT

ARGUMENTS AND VALUES:

  ITEM: item to be rendered
  WIDTH: an integer
  HEIGHT: an integer
  RESULT: undefined

DESCRIPTION:

  RESIZE is the general purpose resizing entry point for all widgets.

  WIDTH and HEIGHT are contextual to the actual item being resized, and
  may be even be ignored."))

(defgeneric reposition (item x y)
 (:documentation
  "REPOSITION ITEM X Y => RESULT

ARGUMENTS AND VALUES:

  ITEM: item to be rendered
  X: an integer
  Y: an integer
  RESULT: undefined

DESCRIPTION:

  REPOSITION is the general purpose resizing entry point for all widgets.

  X and Y are contextual to the actual item being repositioned, and
  may be even be ignored."))

(defgeneric mousemove (item x y)
 (:documentation
  "MOUSEMOVE ITEM X Y => RESULT

ARGUMENTS AND VALUES:

  ITEM: item handling event
  X: an integer
  Y: an integer
  RESULT: undefined

DESCRIPTION:

  MOUSEMOVE is the general purpose mousemove entry point for all widgets.  It
  is used to alert widgets to movements of the mouse, regardless of button state.

  X and Y are absolute coordinates, and assumed to be opengl coordinates,
  not window coordinates (meaning they match the render and setup functions
  of widgets).

  A catchall method that does nothing is also defined so that mouse functions
  can loop over all available widgets and let them decide what they want to do."))

(defmethod mousemove (item x y))

(defgeneric mousedown (item x y)
 (:documentation
  "MOUSEDOWN ITEM X Y => RESULT

ARGUMENTS AND VALUES:

  ITEM: item handling event
  X: an integer
  Y: an integer
  RESULT: undefined

DESCRIPTION:

  MOUSEDOWN is the general purpose mousedown entry point for all widgets.  It
  is used to alert widgets that a mouse button has been pressed, and where.
  There's no information on which button has been pressed.

  X and Y are absolute coordinates, and assumed to be opengl coordinates,
  not window coordinates (meaning they match the render and setup functions
  of widgets).

  A catchall method that does nothing is also defined so that mouse functions
  can loop over all available widgets and let them decide what they want to do."))

(defmethod mousedown (item x y))

(defgeneric mouseup (item x y)
 (:documentation
  "MOUSEUP ITEM X Y => RESULT

ARGUMENTS AND VALUES:

  ITEM: item handling event
  X: an integer
  Y: an integer
  RESULT: undefined

DESCRIPTION:

  MOUSEUP is the general purpose mouseup entry point for all widgets.  It
  is used to alert widgets to that a mouse button has been released, and where.
  There's no information on which button has been released, and it is
  up to the widget to decide if a click was triggered.

  X and Y are absolute coordinates, and assumed to be opengl coordinates,
  not window coordinates (meaning they match the render and setup functions
  of widgets).

  A catchall method that does nothing is also defined so that mouse functions
  can loop over all available widgets and let them decide what they want to do."))

(defmethod mouseup (item x y))

(defgeneric toggle (item &optional state)
 (:documentation
  "TOGGLE ITEM &optional STATE => NEW-STATE

ARGUMENTS AND VALUES:

  ITEM: an item
  STATE: a boolean, the state to set to
  NEW-STATE: a boolean, the state after being set

DESCRIPTION:

  TOGGLE toggles an ITEM.

  Inverts the current toggle status of the ITEM if no STATE
  passed in, otherwise sets to STATE."))

; Stick utilities here for now
(defun draw-border (x1 y1 x2 y2 &optional (line-width 1f0))
 (let
  ((offset (/ (1- line-width) 2)))
  (gl:line-width line-width)
  (gl:begin :lines)
  (gl:vertex x1 y1)
  (gl:vertex (+ x2 offset) y1)
  (gl:vertex x2 y1)
  (gl:vertex x2 (+ y2 offset))
  (gl:vertex x2 y2)
  (gl:vertex (- x1 offset) y2)
  (gl:vertex x1 y2)
  (gl:vertex x1 (- y1 offset))
  (gl:end)))

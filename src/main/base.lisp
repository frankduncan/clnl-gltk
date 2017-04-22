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

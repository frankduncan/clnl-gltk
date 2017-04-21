(in-package #:clnl-gltk)

(defstruct textbox x y width height text)

(setf (documentation 'textbox-text 'function)
 "FUNCTION TEXTBOX-TEXT TB => TEXT

ARGUMENTS AND VALUES:

  TB: a textbox
  TEXT: string currently being displayed

DESCRIPTION:

  TEXTBOX-TEXT allows for the retrieving and setting of the internal text
  of textbox TB.")

(defun textbox (x y width height &optional text)
 "FUNCTION TEXTBOX X Y WIDTH HEIGHT &optional TEXT => TB

ARGUMENTS AND VALUES:

  X: x offset, in pixels
  Y: y offset, in pixels
  WIDTH: width, in characters
  HEIGHT: height, in characters
  TEXT: optional string for the textual display
  TB: a textbox that can later be rendered

DESCRIPTION:

  TEXTBOX creates a textbox widget.

  The widget is defined in terms of characters, rather than pixels.  In this
  way, it will never clip a portion of a character off.  It will also display
  whatever it can of its text, clipping off characters that are outside.

  Multiline strings are supported, and each one appears on a new line."
 (make-textbox :x x :y y :width width :height height :text text))

(defmethod render ((tb textbox))
 (gl:color 1f0 1f0 1f0)
 (with-slots (x y width height text) tb
  (gl:with-pushed-matrix
   (let
    ((px-width (+ (* width *font-width*) 6))
     (px-height (+ (* height *font-height*) 6)))
    (gl:translate x y 0)
    (gl:line-width 1f0)
    (gl:begin :lines)
    (gl:vertex 0 0)
    (gl:vertex px-width 0)
    (gl:vertex px-width 0)
    (gl:vertex px-width px-height)
    (gl:vertex px-width px-height)
    (gl:vertex 0 px-height)
    (gl:vertex 0 px-height)
    (gl:vertex 0 0)
    (gl:end)
    (gl:translate 2 (- px-height 4 *font-height*) 0)
    (when text
     (let
      ((lines (cl-ppcre:split "\\n" text)))
      (loop
       :for line :in lines
       :for i :from 0
       :do
       (when (< i height)
        (gl:with-pushed-matrix
         (font-print (subseq line 0 (min (length line) width))))
        (gl:translate 0 (- *font-height*) 0)))))))))

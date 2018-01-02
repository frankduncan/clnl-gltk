(in-package #:clnl-gltk)

; add word wrpaping, add border optional, so can use for text boxes for the TEXTBOX widget

(defstruct textbox x y width height text word-wrap border)

(setf (documentation 'textbox-text 'function)
 "TEXTBOX-TEXT TB => TEXT

ARGUMENTS AND VALUES:

  TB: a textbox
  TEXT: string currently being displayed

DESCRIPTION:

  TEXTBOX-TEXT allows for the retrieving and setting of the internal text
  of textbox TB.")

(defun textbox (x y width height &key text (border t) word-wrap)
 "TEXTBOX X Y WIDTH HEIGHT &key TEXT BORDER WORD-WRAP => TB

ARGUMENTS AND VALUES:

  X: x offset, in pixels
  Y: y offset, in pixels
  WIDTH: width, in characters
  HEIGHT: height, in characters
  TEXT: optional string for the textual display
  BORDER: boolean, whether we draw a border, defaults to t
  WORD-WRAP: boolean, whether we attempt to wrap the text
  TB: a textbox that can later be rendered

DESCRIPTION:

  TEXTBOX creates a textbox widget.

  The widget is defined in terms of characters, rather than pixels.  In this
  way, it will never clip a portion of a character off.  It will also display
  whatever it can of its text, clipping off characters that are outside.

  Multiline strings are supported, and each one appears on a new line.

  When BORDER is NIL, no border is drawn and the text box floats, which can be
  useful for labels.

  When WORD-WRAP is non NIL, the text is attempted to wrap by the following rules.
  The wrapping is done at the line if possible, at a breaking character if possible,
  or just fits as many letters as it can befoer wrapping.  It then only clips off
  on the bottom.  The only breaking character currently is #\Space."
 (make-textbox :x x :y y :width width :height height :text text :border border :word-wrap word-wrap))

(defmethod resize ((tb textbox) width height)
 (setf (textbox-width tb) width)
 (setf (textbox-height tb) height))

(defmethod reposition ((tb textbox) x y)
 (setf (textbox-x tb) x)
 (setf (textbox-y tb) y))

(defun break-lines (text width)
 (cond
  ((zerop (length text)) nil)
  ((char= #\Space (aref text 0)) (break-lines (subseq text 1) width))
  ((char= #\Newline (aref text 0)) (break-lines (subseq text 1) width))
  ((let*
    ((pos-space
      (and
       (> (length text) width)
       (position #\Space (subseq text 0 width) :from-end t)))
     (pos-nl (position #\Newline text))
     (pos
      (cond
       ((and pos-nl (< pos-nl width)) pos-nl)
       ((and pos-space (< pos-space width)) pos-space)
       ((min width (length text))))))
    (cons
     (subseq text 0 pos)
     (break-lines (subseq text pos) width))))))

(defmethod render ((tb textbox))
 (gl:color 1f0 1f0 1f0)
 (with-slots (x y width height text border word-wrap) tb
  (gl:with-pushed-matrix
   (let
    ((px-width (+ (* width *font-width*) 6))
     (px-height (+ (* height *font-height*) 6)))
    (gl:translate x y 0)
    (when border (draw-border 0 0 px-width px-height))
    (gl:translate 2 (- px-height 4 *font-height*) 0)
    (when text
     (let
      ((lines (if word-wrap (break-lines text width) (cl-ppcre:split "\\n" text))))
      (loop
       :for line :in lines
       :for i :from 0
       :do
       (when (< i height)
        (gl:with-pushed-matrix
         (font-print (subseq line 0 (min (length line) width))))
        (gl:translate 0 (- *font-height*) 0)))))))))

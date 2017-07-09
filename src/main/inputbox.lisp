(in-package #:clnl-gltk)

(defstruct inputbox x y width first-drawn-char cursor text)

(defun inputbox (x y width)
 "INPUTBOX X Y WIDTH => IB

ARGUMENTS AND VALUES:

  X: x offset, in pixels
  Y: y offset, in pixels
  WIDTH: width, in characters
  IB: an inputbox that can later be rendered

DESCRIPTION:

  INPUTBOX creates an inputbox widget.

  The inputbox is a simple, single lined, box that can hold a mutating string.
  Use the various INPUTBOX-* functions to add to it and modify it.  As a string
  is added to it that is too large, it will scroll the characters automatically.

  The widget is defined in terms of characters, rather than pixels.  In this
  way, it will never clip a portion of a character off."
 (make-inputbox :x x :y y :width width :text "" :first-drawn-char 0 :cursor 0))

(defmethod resize ((ib inputbox) width height)
 (declare (ignore y))
 (setf (inputbox-width ib) width))

(defmethod render ((ib inputbox))
 (gl:color 1f0 1f0 1f0)
 (with-slots (x y width first-drawn-char cursor text) ib
  (gl:with-pushed-matrix
   (let
    ((px-width (+ (* width *font-width*) 6))
     (px-height (+ (* 1 *font-height*) 6)))
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

    (font-print (subseq text first-drawn-char cursor))

    (gl:color 1f0 1f0 1f0 1f0)
    (gl:begin :quads)
    (gl:vertex 0 0)
    (gl:vertex *font-width* 0)
    (gl:vertex *font-width* (1+ *font-height*))
    (gl:vertex 0 (1+ *font-height*))
    (gl:end)
    (when (< cursor (length text))
     (gl:color 0.0f0 0.0f0 0.0f0 1f0)
     (font-print (subseq text cursor (1+ cursor))))

    (gl:color 1f0 1f0 1f0 1f0)

    (when (< cursor (1- (length text)))
     (font-print (subseq text (1+ cursor) (min (length text) (+ first-drawn-char width)))))))))

(defun add-char (ib c)
 (with-slots (width first-drawn-char cursor text) ib
  (setf (inputbox-text ib) (format nil "~A~A~A" (subseq text 0 cursor) (code-char c) (subseq text cursor)))
  (incf cursor)
  (when (<= (+ first-drawn-char width) cursor) (incf first-drawn-char))))

(defun delete-char (ib)
 (with-slots (width first-drawn-char cursor text) ib
  (when (and (< 0 cursor) (< 0 (length text)))
   (setf (inputbox-text ib) (concatenate 'string (subseq text 0 (1- cursor)) (subseq text cursor)) )))
 (left ib))

(defun empty (ib) ; for control u
 (with-slots (first-drawn-char cursor text) ib
  (setf text (subseq text cursor))
  (setf cursor 0)
  (setf first-drawn-char 0)))

; May someday make this generic
(defun clear (ib)
 "FUNCTION CLEAR IB => RESULT

ARGUMENTS AND VALUES:

  IB: an inputbox
  RESULT: undefined

DESCRIPTION:

  Rests the inputbox IB to empty."
 (with-slots (first-drawn-char cursor text) ib
  (setf text "")
  (setf cursor 0)
  (setf first-drawn-char 0)
  nil))

; May someday make this generic
(defun value (ib)
 "FUNCTION VALUE IB => TEXT

ARGUMENTS AND VALUES:

  IB: an inputbox
  TEXT: a string, the text currently in IB

DESCRIPTION:

  Returns the TEXT that currently resides in the inputbox IB."
 (inputbox-text ib))

(defun left (ib)
 (with-slots (first-drawn-char cursor) ib
  (when (< 0 cursor)
   (decf cursor)
   (when (< cursor first-drawn-char)
    (decf first-drawn-char)))))

(defun right (ib)
 (with-slots (first-drawn-char width cursor text) ib
  (when (< cursor (length text))
   (incf cursor)
   (when (<= (+ first-drawn-char width) cursor)
    (incf first-drawn-char)))))

; Generic this later if we add other keyboard widgets
(defun key-pressed (ib key)
 "KEY-PRESSED IB KEY => RESULT

ARGUMENTS AND VALUES:

  IB: An inputbox
  KEY: Key pressed, an integer or a symbol
  RESULT: Undefined

DESCRIPTION:

  KEY-PRESSED will do the appropriate thing in the case of a key being pressed.

  When an integer, will insert the text into the appropriate location, if it's
  an ascii character less than 256.

  The other values acceptable are:

  :key-left moves the cursor one to the left
  :key-right moves the cursor one to the right"
 (cond
  ((and (integerp key) (< 31 key 256)) (add-char ib key))
  ((equal key 8) (delete-char ib))
  ((equal key 21) (empty ib))
  ((equal key :key-left) (left ib))
  ((equal key :key-right) (right ib))))

(in-package #:clnl-gltk)

(defstruct button x y width height text callback hover click)

; minimalist button idea
; - white line border
; - hover, lighten inside slightly
; - click/mousedown, faded outside border, one line outside at 80% opacity, one more at 50% opacity
;   - from amazon
; - toggled on, inside is 80% white, lettering black

(defun in-button (m-x m-y but)
 (with-slots (x y width height) but
  (and (< x m-x (+ x width)) (< y m-y (+ y height)))))

(defun button (x y width height text callback)
 "BUTTON X Y WIDTH HEIGHT TEXT CALLBACK => BUTTON

ARGUMENTS AND VALUES:

  X: x offset, in pixels
  Y: y offset, in pixels
  WIDTH: width, in pixels
  HEIGHT: height, in pixels
  TEXT: string for the textual display
  CALLBACK: a function
  BUTTON: a button that can later be rendered

DESCRIPTION:

  BUTTON creates a button widget.

  The widget will center the viewable TEXT inside itself, replacing the
  last three characters with an ellipses if the text is too large for the
  given dimensions.  It will never clip a character.

  BUTTON objects also work with mouse movement functions.  When it identifies
  that a click has happened, CALLBACK will be called."
 (make-button :x x :y y :width width :height height :text text :callback callback))

(defmethod render ((but button))
 (gl:color 1f0 1f0 1f0)
 (with-slots (x y width height text hover click) but
  (gl:with-pushed-matrix
   (gl:translate x y 0)

   (when hover
    (gl:color .25f0 .25f0 .25f0)
    (gl:begin :quads)
    (gl:vertex 0 0)
    (gl:vertex width 0)
    (gl:vertex width height)
    (gl:vertex 0 height)
    (gl:end))

   (when click
    (gl:color .5f0 .5f0 .5f0)
    (draw-border 0 0 width height 5f0)

    (gl:color .8f0 .8f0 .8f0)
    (draw-border 0 0 width height 3f0))

   (gl:color 1f0 1f0 1f0)
   (draw-border 0 0 width height)

   ; NetLogo doesn't allow buttons shorter than a letter, so we can assume that we get that height.
   ; It also clips off text if too long, and replaces with elipses, so we can do similar
   (gl:color 1f0 1f0 1f0)
   (let
    ((text
      (if (< (* *font-width* (length text)) (- width 4))
       text
       (format nil "~A..." (subseq text 0 (- (truncate width *font-width*) 3))))))
    (gl:translate
     (truncate (- width 4 (* *font-width* (length text))) 2)
     (truncate (- height 4 *font-height*) 2)
     0)
    (font-print text)))))

(defmethod reposition ((but button) x y)
 (setf (button-x but) x)
 (setf (button-y but) y))

(defmethod resize ((but button) width height)
 (setf (button-width but) width)
 (setf (button-height but) height))

(defmethod mousemove ((but button) m-x m-y)
 (setf (button-hover but) (in-button m-x m-y but)))

(defmethod mousedown ((but button) m-x m-y)
 (setf (button-click but) (in-button m-x m-y but)))

(defmethod mouseup ((but button) m-x m-y)
 (when (and (button-hover but) (button-click but) (button-callback but)) (funcall (button-callback but)))
 (setf (button-click but) nil))

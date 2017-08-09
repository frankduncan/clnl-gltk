(in-package #:clnl-gltk)

(defstruct switch x y width text callback hover on)

; Height is constant for switches in netlogo
(defvar *switch-height* 33
 "*SWITCH-HEIGHT*

VALUE TYPE:

  an integer

INITIAL VALUE:

  33

DESCRIPTION:

  The default switch height.")

; minimalist switch idea
; - white line border
; - hover, lighten inside slightly
; - mousedown, just change the state immediately
; - toggled on, placement of switch

(defun in-switch (m-x m-y switch)
 (with-slots (x y width) switch
  (and (< x m-x (+ x width)) (< y m-y (+ y *switch-height*)))))

(defun switch (x y width text callback &optional initial-state)
 "SWITCH X Y WIDTH TEXT CALLBACK &optional INITIAL-STATE => SWITCH

ARGUMENTS AND VALUES:

  X: x offset, in pixels
  Y: y offset, in pixels
  WIDTH: width, in pixels
  TEXT: string for the textual display
  CALLBACK: a function
  INITIAL-STATE: a boolean, defaulting to nil
  SWITCH: a switch that can later be rendered

DESCRIPTION:

  SWITCH creates a switch widget.

  The widget will center the viewable TEXT inside itself, replacing the
  last three characters with an ellipses if the text is too large for the
  given dimensions.  It will never clip a character.

  SWITCH objects also work with mouse movement functions.  When it identifies
  that a mousedown has happened, the state of the SWITCH will be changed,
  and CALLBACK will be called with the new state.

  The INITIAL-STATE defines whether the switch starts on or off."
 (make-switch :x x :y y :width width :text text :callback callback :on initial-state))

(defmethod render ((switch switch))
 (gl:color 1f0 1f0 1f0)
 (with-slots (x y width text hover on) switch
  (gl:with-pushed-matrix
   (gl:translate x y 0)

   (when hover
    (gl:color .25f0 .25f0 .25f0)
    (gl:begin :quads)
    (gl:vertex 0 0) (gl:vertex width 0) (gl:vertex width *switch-height*) (gl:vertex 0 *switch-height*)
    (gl:end))

   (gl:color 1f0 1f0 1f0)
   (gl:begin :quads)
   (gl:vertex 6 4) (gl:vertex 8 4) (gl:vertex 8 28) (gl:vertex 6 28)
   (gl:end)

   (let
    ((bottom (if on 21 8)))
    (gl:color 0f0 0f0 0f0)
    (gl:begin :quads)
    (gl:vertex 2 bottom) (gl:vertex 12 bottom) (gl:vertex 12 (+ bottom 4)) (gl:vertex 2 (+ bottom 4))
    (gl:end)
    (gl:color 1f0 1f0 1f0)
    (draw-border 2 bottom 12 (+ bottom 4)))

   (gl:color 1f0 1f0 1f0)
   (gl:with-pushed-matrix (gl:translate 14 15 0) (font-print "On"))
   (gl:with-pushed-matrix (gl:translate 14 1 0) (font-print "Off"))

   (draw-border 0 0 width *switch-height*)

   ; It also clips off text if too long, and replaces with elipses, so we can do similar
   (let
    ((text
      (if (< (* *font-width* (length text)) (- width 46))
       text
       (format nil "~A..." (subseq text 0 (- (truncate (- width 46) *font-width*) 3))))))
    (gl:translate
     (truncate (- width -28 (* *font-width* (length text))) 2)
     (truncate (- *switch-height* 4 *font-height*) 2)
     0)
    (font-print text)))))

(defmethod toggle ((switch switch) &optional (state :unused))
 (setf (switch-on switch) (if (eql state :unused) (not (switch-on switch)) state)))

(defmethod reposition ((switch switch) x y)
 (setf (switch-x switch) x)
 (setf (switch-y switch) y))

(defmethod resize ((switch switch) width height)
 (declare (ignore height))
 (setf (switch-width switch) width))

(defmethod mousemove ((switch switch) m-x m-y)
 (setf (switch-hover switch) (in-switch m-x m-y switch)))

(defmethod mousedown ((switch switch) m-x m-y)
 (when (in-switch m-x m-y switch) (toggle switch) (funcall (switch-callback switch) (switch-on switch))))

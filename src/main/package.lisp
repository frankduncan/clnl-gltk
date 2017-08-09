(defpackage #:clnl-gltk (:use :cl)
 (:export
  #:render #:resize #:reposition #:mousemove #:mousedown #:mouseup #:toggle
  #:font-print *font-width* *font-height*
  #:textbox #:textbox-text
  #:inputbox #:key-pressed #:value #:clear
  #:button
  #:switch #:*switch-height*
  #:setup)
 (:documentation "Main clnl-gltk package.

Use widgets available in clnl-gltk to create NetLogo user interface widgets in
pure opengl.  This project doesn't have much usefulness outside of CLNL."))

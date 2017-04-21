(defpackage #:clnl-gltk (:use :cl)
 (:export
  #:render
  #:font-print
  #:textbox #:textbox-text
  #:inputbox #:key-pressed #:value #:clear
  #:setup)
 (:documentation "Main clnl-gltk package.

Use widgets available in clnl-gltk to create NetLogo user interface widgets in
pure opengl.  This project doesn't have much usefulness outside of CLNL."))

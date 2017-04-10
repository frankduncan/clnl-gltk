(asdf:defsystem clnl-gltk-test
 :name "OpenGL Window Toolkit for CLNL Tests"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :components ((:file "package") (:file "main"))
 :depends-on (:clnl-gltk #-travis :cl-glu #-travis :cl-glut))

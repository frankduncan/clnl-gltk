(asdf:defsystem clnl-gltk
 :name "OpenGL Window Toolkit for CLNL"
 :version "0.0"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :components ((:file "package"))
 :depends-on (#-travis :cl-opengl)) ; Don't load libraries in travis
(in-package #:clnl-gltk-test)

(deftest "Simple Font"
 '("EF9A5638F61D00895D8917C2B1C3AE5C3597698B" "122BF8CBDB3578C01903B4E0D535380B7FA85FA7")
 (gl:color 1f0 1f0 1f0)
 (gl:translate 10 10 0)
 (clnl-gltk:font-print "Hello World"))

(let
 ((tb (clnl-gltk:textbox 5 5 5 1 "Hello")))
 (deftest "Text Box 1"
  '("AAE3BF09109DF261EAAA6FBEA82A1E9044E5E184" "3929C31E199A896456CABF9CDA1BAE694CDD1733")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 7 2 "Hello")))
 (deftest "Text Box 2"
  '("2799D09A639901F185FEE26E95EC241938D53738" "7E279C409FD0F12115F2E43ABC756DFEA953276B")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 5 2 (format nil "Hello~%World"))))
 (deftest "Text Box Multiline"
  '("D8078D7EC6C179EAC0C19AAD10A01262F8C55684" "498E4F14AB2B1410FABA2BB826962E724A705E5E")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 5 2 (format nil "Helloare~%Worlding~%fdsa"))))
 (deftest "Text Box Multiline Clip"
  '("D8078D7EC6C179EAC0C19AAD10A01262F8C55684" "498E4F14AB2B1410FABA2BB826962E724A705E5E")
  (clnl-gltk:render tb)))

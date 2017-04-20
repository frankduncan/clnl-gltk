(in-package #:clnl-gltk-test)

(deftest "Simple Font"
 '("C186403CAF13F5E2A260D84F7D3555C89C1B18B3" "0933C804D3AE2D3314ED490458777695FCFDF460")
 (gl:color 1f0 1f0 1f0)
 (gl:translate 10 10 0)
 (clnl-gltk:font-print "Hello World"))

(let
 ((tb (clnl-gltk:textbox 5 5 5 1 "Hello")))
 (deftest "Text Box 1"
  '("9D13BA3FEE0C23D451C0364491C39D3A50B35C92" "A00DE5E0B21B3C1BD57137C5C0E5F9C54A7E44FB")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 7 2 "Hello")))
 (deftest "Text Box 2"
  '("DDC8524798C4A0560758494E39FB34CCC4A5C55B" "D64787E4123F9A12938469FFD480A10B421E138D")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 5 2 (format nil "Hello~%World"))))
 (deftest "Text Box Multiline"
  '("33BD8696E4E0E59BA15C090F92B63D7717F21BC2" "5DD186A64C522B09BA5A3BF35E8937DB12C0D984")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 5 2 (format nil "Helloare~%Worlding~%fdsa"))))
 (deftest "Text Box Multiline Clip"
  '("33BD8696E4E0E59BA15C090F92B63D7717F21BC2" "5DD186A64C522B09BA5A3BF35E8937DB12C0D984")
  (clnl-gltk:render tb)))

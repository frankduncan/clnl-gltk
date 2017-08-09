(in-package #:clnl-gltk-test)

(deftest "Simple Font"
 '("8A20F6ABDEB91C20EDA85C78AED49767AB3A70EB" "80DD444F064D2D60669D072BDE0C6CE25B48E367")
 (gl:color 1f0 1f0 1f0)
 (gl:translate 10 10 0)
 (clnl-gltk:font-print "Hello World"))

(let
 ((tb (clnl-gltk:textbox 5 5 5 1 "Hello")))
 (deftest "Text Box 1"
  '("21A6A03947D811EF70F45DAB3CF86FC3F5155F57" "002B9E00C55CB467C2BA6DB7004AEB3E37EB7DC0")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 7 2 "Hello")))
 (deftest "Text Box 2"
  '("6CC1E2734C274C193B003A544A7E3A5923F25C5C" "76251F34BA5626AA846C7700B5D78A15C5D3FEE5")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 5 2 (format nil "Hello~%World"))))
 (deftest "Text Box Multiline"
  '("A90B6F28500F34FEA8DB7C766C775EC9DE89B9FD" "AEE3421D70811D6989348374251B2F9E00019B46")
  (clnl-gltk:render tb)))

(let
 ((tb (clnl-gltk:textbox 5 5 5 2 (format nil "Helloare~%Worlding~%fdsa"))))
 (deftest "Text Box Multiline Clip"
  '("A90B6F28500F34FEA8DB7C766C775EC9DE89B9FD" "AEE3421D70811D6989348374251B2F9E00019B46")
  (clnl-gltk:render tb)))

(let
 ((ib (clnl-gltk:inputbox 5 5 5)))
 (deftest "Input Box 1"
  '("12C410CF23BC494CB6C61A69452BF9D66E2F535B")
  (setf *inputbox* ib)
  (clnl-gltk:render ib)))

(let
 ((ib (clnl-gltk:inputbox 5 5 5)))
 (clnl-gltk:key-pressed ib 65)
 (clnl-gltk:key-pressed ib 66)
 (clnl-gltk:key-pressed ib 67)
 (clnl-gltk:key-pressed ib :key-left)
 (clnl-gltk:key-pressed ib :key-left)
 (clnl-gltk:key-pressed ib 8)
 (clnl-gltk:key-pressed ib :key-right)
 (deftest "Input Box 2"
  '("28ABF4CF2164E810E2068B3FEB8E7AEE356D809D" "FC65485ADF1D8082B3477388E16E407DBA2055D9")
  (setf *inputbox* ib)
  (clnl-gltk:render ib)))

(let
 ((ib (clnl-gltk:inputbox 5 5 5)))
 (clnl-gltk:key-pressed ib 65)
 (clnl-gltk:key-pressed ib 66)
 (clnl-gltk:key-pressed ib 67)
 (clnl-gltk:key-pressed ib 68)
 (clnl-gltk:key-pressed ib :key-left)
 (clnl-gltk:key-pressed ib 21)
 (deftest "Input Box 3"
  '("39BBC5FBF12191BA83EB29477D8C96BA6B09790D" "A3B65B6C4AA44881EC8A5CBF38ED1E78FDBBFF00")
  (setf *inputbox* ib)
  (clnl-gltk:render ib)))

(let
 ((ib (clnl-gltk:inputbox 5 5 5))
  (tb (clnl-gltk:textbox 5 28 5 1)))
 (clnl-gltk:key-pressed ib 65)
 (clnl-gltk:key-pressed ib 66)
 (clnl-gltk:key-pressed ib 67)
 (clnl-gltk:key-pressed ib 68)
 (clnl-gltk:key-pressed ib 8)
 (setf (clnl-gltk:textbox-text tb) (clnl-gltk:value ib))
 (clnl-gltk:clear ib)
 (deftest "Input Box / Text Box"
  '("FFE0866958A8EAC1B04FCFB1C03BD0CD0250FE9D" "357A70D811786FF4ACAB6EBFA489E6FF32F08BB2")
  (setf *inputbox* ib)
  (clnl-gltk:render tb)
  (clnl-gltk:render ib)))

(let
 ((ib (clnl-gltk:inputbox 5 5 1))
  (tb (clnl-gltk:textbox 5 28 1 1)))
 (clnl-gltk:resize ib (truncate (- 100 12) clnl-gltk:*font-width*) 1)
 (clnl-gltk:resize tb (truncate (- 100 12) clnl-gltk:*font-width*) (truncate (- 100 40) clnl-gltk:*font-height*))
 (deftest "Resize Input Box / Text Box"
  "14DFCF6AA7954142C5B37DB6DB70EE1426891A8F"
  (setf *inputbox* ib)
  (clnl-gltk:render tb)
  (clnl-gltk:render ib)))

(defvar *button-base-sum* "246FF03A0BF9C0ADEBBE6D399450DF4B09951E00")
(defvar *button-hover-sum* "CBAAA3334D3529AF002764EC4043DB39B0DA42C2")

(let
 ((but (clnl-gltk:button 5 5 40 30 "test" (lambda () (format t "This button was pressed~%")))))
 (deftest "Button 1"
  *button-base-sum*
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let
 ((but (clnl-gltk:button 5 5 40 30 "te" (lambda () (format t "This button was pressed~%")))))
 (deftest "Button 2"
  "77E9810A560991D0420F7E9AACBF7661C71CDBFB"
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let
 ((checksum "E0F7EE964C574B5A9360A154C7C1FD2F64E9FFD7"))
 (let
  ((but (clnl-gltk:button 5 5 40 30 "te..." (lambda () (format t "This button was pressed~%")))))
  (deftest "Button 3a"
   checksum
   (setf *mouse-reactor* but)
   (clnl-gltk:render but)))

 (let
  ((but (clnl-gltk:button 5 5 40 30 "testing" (lambda () (format t "This button was pressed~%")))))
  (deftest "Button 3b"
   checksum
   (setf *mouse-reactor* but)
   (clnl-gltk:render but))))

(let
 ((but (clnl-gltk:button 5 5 40 30 "test" nil)))
 (clnl-gltk:mousemove but 10 10)
 (deftest "Button Hover 1"
  *button-hover-sum*
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let
 ((but (clnl-gltk:button 5 5 40 30 "test" nil)))
 (clnl-gltk:mousemove but 10 10)
 (clnl-gltk:mousemove but 50 50)
 (deftest "Button Hover 2"
  *button-base-sum*
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let
 ((but (clnl-gltk:button 5 5 40 30 "test" nil)))
 (clnl-gltk:mousemove but 10 10)
 (clnl-gltk:mousedown but 10 10)
 (deftest "Button Down"
  "006E9FE5F641E446EB322FC5977FA3643D80A67E"
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let
 ((but (clnl-gltk:button 5 5 40 30 "test" nil)))
 (clnl-gltk:mousemove but 10 10)
 (clnl-gltk:mousedown but 10 10)
 (clnl-gltk:mousemove but 50 50)
 (deftest "Button Down Moved"
  "EB826464B9A6A1CB4554FA083F87D7D8EC1A071A"
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let*
 ((clicked nil)
  (but (clnl-gltk:button 5 5 40 30 "test" (lambda () (setf clicked t)))))
 (clnl-gltk:mousemove but 10 10)
 (clnl-gltk:mousedown but 10 10)
 (clnl-gltk:mouseup but 10 10)
 (deftest "Button Clicked"
  *button-hover-sum*
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)
  (when (not clicked) (fail-test))))

(let*
 ((clicked nil)
  (but (clnl-gltk:button 5 5 40 30 "test" (lambda () (setf clicked t)))))
 (clnl-gltk:mousemove but 10 10)
 (clnl-gltk:mousedown but 10 10)
 (clnl-gltk:mousemove but 50 50)
 (clnl-gltk:mouseup but 50 50)
 (deftest "Button Clicked Abort"
  *button-base-sum*
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)
  (when clicked (fail-test))))

(let
 ((but (clnl-gltk:button 20 20 50 40 "test" nil)))
 (clnl-gltk:resize but 40 30)
 (clnl-gltk:reposition but 5 5)
 (deftest "Button Reize/Reposition"
  *button-base-sum*
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let*
 ((but nil))
 (setf but (clnl-gltk:button 20 20 60 40 "test" (lambda () (clnl-gltk:toggle but)) :forever t))
 (deftest "Button Forever 1"
  '("177FDA47EF4BF0A9C17594EE3FCC46810BF95EDE" "7825FBB56097D7F74ECE6D09FD864CB7D9A166F6")
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let*
 ((but nil))
 (setf but (clnl-gltk:button 20 20 60 40 "test" (lambda () (clnl-gltk:toggle but)) :forever t))
 (clnl-gltk:mousemove but 30 30)
 (clnl-gltk:mousedown but 30 30)
 (clnl-gltk:mouseup but 30 30)
 (deftest "Button Forever - Toggle mouse on"
  '("6838E917A88FECEFE4DF081B072699F30FC869F6" "AA8B2CB66C2B24006F65F800B4782AE6085F5191")
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let*
 ((but nil))
 (setf but (clnl-gltk:button 20 20 60 40 "test" (lambda () (clnl-gltk:toggle but)) :forever t))
 (clnl-gltk:mousemove but 30 30)
 (clnl-gltk:mousedown but 30 30)
 (clnl-gltk:mouseup but 30 30)
 (clnl-gltk:mousemove but 10 10)
 (deftest "Button Forever - Toggle mouse off"
  '("3A3B731359CBDADE2818D555B086CBC93EA29017" "1A3588244F03415EBFD74763309A7E9FDA32EFE4")
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let*
 ((but nil))
 (setf but (clnl-gltk:button 20 20 60 40 "test" (lambda () (clnl-gltk:toggle but)) :forever t))
 (clnl-gltk:mousemove but 30 30)
 (clnl-gltk:mousedown but 30 30)
 (clnl-gltk:mouseup but 30 30)
 (clnl-gltk:mousedown but 30 30)
 (clnl-gltk:mouseup but 30 30)
 (clnl-gltk:mousemove but 10 10)
 (deftest "Button Forever - Toggle twice"
  '("177FDA47EF4BF0A9C17594EE3FCC46810BF95EDE" "7825FBB56097D7F74ECE6D09FD864CB7D9A166F6")
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(let*
 ((but nil))
 (setf but (clnl-gltk:button 20 20 60 40 "test" (lambda () (clnl-gltk:toggle but t)) :forever t))
 (clnl-gltk:mousemove but 30 30)
 (clnl-gltk:mousedown but 30 30)
 (clnl-gltk:mouseup but 30 30)
 (clnl-gltk:mousedown but 30 30)
 (clnl-gltk:mouseup but 30 30)
 (clnl-gltk:mousemove but 10 10)
 (deftest "Button Forever - Toggle state T"
  '("3A3B731359CBDADE2818D555B086CBC93EA29017" "1A3588244F03415EBFD74763309A7E9FDA32EFE4")
  (setf *mouse-reactor* but)
  (clnl-gltk:render but)))

(defvar *switch-on* '("066088122E0633F6F1C309373BF7E02EA0EDF6B4" "3B8285565BCB400AAF89B180F7C3D4BC6A34761A"))
(defvar *switch-off* '("1D4029F33A353A9EAA7DB7038AFC5C3CF6640C3D" "47CFEB0F5E5841DAAA4DB5CC543E1E2EFE8E037D"))
(defvar *switch-hover* '("056E4C64078F6EA45A49E0F342D3B7CD912DE996" "B1C3FC0B3D232674E9DF26B53D3A5A12F22756B2"))

(let
 ((switch (clnl-gltk:switch 5 5 80 "test" (lambda (state) (format t "The new state is: ~A~%" state)) t)))
 (deftest "Switch 1"
  *switch-on*
  (setf *mouse-reactor* switch)
  (clnl-gltk:render switch)))

(let
 ((switch (clnl-gltk:switch 5 5 80 "test" (lambda (state) (format t "The new state is: ~A~%" state)))))
 (deftest "Switch 2"
  *switch-off*
  (setf *mouse-reactor* switch)
  (clnl-gltk:render switch)))

(let
 ((switch (clnl-gltk:switch 5 5 80 "testing switch" (lambda (state) (format t "The new state is: ~A~%" state)))))
 (deftest "Switch 3"
  '("35332B4D8BD5E26FADFB9CE21466EC6016C26887" "751D71C3B77DE8956325EDC2294FEDB68A12643E")
  (setf *mouse-reactor* switch)
  (clnl-gltk:render switch)))

(let
 ((switch (clnl-gltk:switch 5 5 80 "test" (lambda (state) (format t "The new state is: ~A~%" state)))))
 (clnl-gltk:mousemove switch 30 30)
 (deftest "Switch Hover"
  *switch-hover*
  (setf *mouse-reactor* switch)
  (clnl-gltk:render switch)))

(let
 ((switch (clnl-gltk:switch 5 5 80 "test" (lambda (state) (format t "The new state is: ~A~%" state)))))
 (clnl-gltk:mousemove switch 30 30)
 (clnl-gltk:mousemove switch 50 50)
 (deftest "Switch Hover 2"
  *switch-off*
  (setf *mouse-reactor* switch)
  (clnl-gltk:render switch)))

(let*
 ((on nil)
  (switch (clnl-gltk:switch 5 5 80 "test" (lambda (state) (setf on state)))))
 (clnl-gltk:mousedown switch 30 30)
 (deftest "Switch Click"
  *switch-on*
  (setf *mouse-reactor* switch)
  (clnl-gltk:render switch)
  (when (not on) (fail-test))))

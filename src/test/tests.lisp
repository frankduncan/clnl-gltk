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

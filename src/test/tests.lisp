(in-package #:clnl-gltk-test)

(deftest "Simple Font"
 '("C186403CAF13F5E2A260D84F7D3555C89C1B18B3" "0933C804D3AE2D3314ED490458777695FCFDF460")
 (gl:color 1f0 1f0 1f0)
 (gl:translate 10 10 0)
 (clnl-gltk:font-print "Hello World"))

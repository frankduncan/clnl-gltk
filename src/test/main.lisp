(in-package #:clnl-gltk-test)

(defvar *tests* nil)
(defvar *glut-window-opened* nil)
(defvar *commands* nil)

(defun find-test (name)
 (or
  (find name *tests* :test #'string= :key #'car)
  (error "Couldn't find test with name: ~A" name)))

(defun run-and-print-test (test)
 (let
  ((green (format nil "~c[1;32m" #\Esc))
   (red (format nil "~c[1;31m" #\Esc))
   (result (funcall (cadr test))))
  (format t "~A- ~S ~A~c[0m~%" (if result green red) (car test) (if result "passed" "failed") #\Esc)
  result))

(defun run-tests (tests)
 (let
  ((final-result t))
  (loop
   :for test :in tests
   :for result := (run-and-print-test test)
   :do (setf final-result (and final-result result)))
  final-result))

(defun run-all-tests ()
 (run-tests (reverse *tests*)))

(defmacro defsimpletest (name test-fn debug-fn scala-prog scala-input)
 `(progn
   (push
    (list ,name ,test-fn ,debug-fn ,scala-prog ,scala-input)
    *tests*)))

(defun test-debug (name) (format t "----~%~A~%" (funcall (third (find-test name)))))

(defun checksum= (expected got)
 (if (stringp expected)
  (string= got expected)
  (find got expected :test #'string=)))

(defmacro deftest (name sum &body commands)
 `(push
   (list
    ,name
    (lambda ()
     (setup)
     (setf *commands* (lambda () ,@commands))
     (let
      ((result-sum (checksum-view)))
      (when (not (checksum= ,sum result-sum))
       (format t "~c[1;35m-- For ~A, got ~A but expected ~A~c[0m~%" #\Esc ,name result-sum ,sum #\Esc))
      (checksum= ,sum result-sum)))
    (lambda ()
     (setup)
     (setf *commands* (lambda () ,@commands))
     (save-view-to-ppm)
     (format nil "~A" (checksum-view)))
    (lambda ()
     (setup)
     (setf *commands* (lambda () ,@commands))
     (run)))
   *tests*))

(defun render-scene ()
 (gl:clear :color-buffer-bit :depth-buffer-bit)
 (gl:enable :blend)
 (gl:matrix-mode :projection)
 (gl:with-pushed-matrix
  (gl:load-identity)
  (gl:ortho 0 100 0 100 -100 100)
  (gl:matrix-mode :modelview)
  (gl:with-pushed-matrix
   (funcall *commands*))
  (gl:matrix-mode :projection)))

(defun setup ()
 (sb-int:with-float-traps-masked (:invalid :inexact :divide-by-zero :overflow :underflow)
  (when (not *glut-window-opened*)
   (cl-glut:init)
   (cl-glut:init-window-size 100 100)
   (cl-glut:init-display-mode :double :rgba)
   (cl-glut:create-window "CLNL-GLTK Test Window")
   (gl:clear-color 0 0 0 1)
   (cl-glut:display-func (cffi:get-callback 'display))
   (cl-glut:reshape-func (cffi:callback reshape))
   (cl-glut:idle-func (cffi:get-callback 'idle))
   (cl-glut:close-func (cffi:get-callback 'close-func))
   (clnl-gltk:setup)
   (setf *glut-window-opened* t))))

(defun checksum-view ()
 (format nil "~{~2,'0X~}"
  (map 'list #'identity
   (ironclad:digest-sequence :sha1 (coerce (export-view) '(vector (unsigned-byte 8)))))))

(defun save-view-to-ppm ()
 (let
  ((height 100) (width 100)) ; hardcoded in interface, hardcoded here, cry for me
  (with-open-file (str "cl.ppm"
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
   (write-sequence (map 'vector #'char-code (format nil "P6~%")) str)
   (write-sequence (map 'vector #'char-code (format nil "~A ~A~%" width height)) str)
   (write-sequence (map 'vector #'char-code (format nil "255~%")) str)
   (let
    ((image-data (export-view)))
    (dotimes (i width)
     (dotimes (j height)
      (write-byte (aref image-data (+ 0 (* 4 (+ (* (- (1- height) i) width) j)))) str)
      (write-byte (aref image-data (+ 1 (* 4 (+ (* (- (1- height) i) width) j)))) str)
      (write-byte (aref image-data (+ 2 (* 4 (+ (* (- (1- height) i) width) j)))) str)))))))

(defun export-view ()
 (sb-int:with-float-traps-masked (:invalid)
  (let
   ((fbo (first (gl:gen-framebuffers 1)))
    (render-buf (first (gl:gen-renderbuffers 1)))
    (width 100)  ; Hard coded for now, yay v1 (if you see this comment in a year, please cry for me)
    (height 100))
   (gl:bind-framebuffer :framebuffer fbo)
   (gl:bind-renderbuffer :renderbuffer render-buf)
   (gl:renderbuffer-storage :renderbuffer :rgba8 width height)
   (gl:framebuffer-renderbuffer :draw-framebuffer :color-attachment0 :renderbuffer render-buf)
   (gl:viewport 0 0 width height)
   (render-scene)
   (gl:read-pixels 0 0 width height :rgba :unsigned-byte))))

(defun close-func ()
 (sb-ext:exit :abort t))

(defun reshape (width height)
 (when (and (/= 0 width) (/= 0 height))
  (gl:viewport 0 0 width height)))

(defun idle ()
 (cl-glut:post-redisplay))

(defun display ()
 (render-scene)
 (cl-glut:swap-buffers))

(cffi:defcallback display :void () (display))
(cffi:defcallback idle :void () (idle))
(cffi:defcallback close-func :void () (close-func))
(cffi:defcallback reshape :void ((width :int) (height :int)) (reshape width height))

(defun run ()
 ; I do this because I don't know who or what in the many layers
 ; is causing the floating point errors, but I definitely don't
 ; want to investigate until simply ignoring them becomes a problem.
 (sb-int:with-float-traps-masked (:invalid :inexact :divide-by-zero :overflow :underflow)
  (cl-glut:main-loop)))

(defun run-test (test-name)
 (funcall (fourth (find-test test-name))))

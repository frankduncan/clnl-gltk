(in-package #:clnl-gltk-test)

(defvar *checksum-location* nil)
(defvar *checksums* nil)
(defvar *tests* nil)
(defvar *test-success* nil)
(defvar *glut-window-opened* nil)
(defvar *commands* nil)
(defvar *inputbox* nil) ; this can be more generalized later when there's multiple keyboard input widgets
(defvar *mouse-reactor* nil) ; like similar, but at least a little more generalized

(defvar *height* 100) ; window height

(defun fail-test () (setf *test-success* nil))

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
(defun test-run (name) (funcall (fourth (find-test name))))

(defun checksum= (name sum got)
 (or
  (string= got sum)
  (progn
   (when (and (not *checksums*) *checksum-location* (probe-file *checksum-location*))
    (setf *checksums* (with-open-file (str *checksum-location*) (read str nil))))
   (string= got (cdr (assoc name *checksums* :test #'string=))))))

(defmacro deftest (name sum &body commands)
 `(push
   (list
    ,name
    (lambda ()
     (let
      ((*test-success* t))
      (setup)
      (setf *commands* (lambda () ,@commands))
      (let
       ((result-sum (checksum-view)))
       (when (not (checksum= ,name ,sum result-sum))
        (format t "~c[1;35m-- For ~A, got ~A but expected ~A~c[0m~%" #\Esc ,name result-sum ,sum #\Esc)
        (format t "To verify and update, run:~%~S~%"
         `(clnl-gltk-test:verify-and-update ,,name ,result-sum ,*checksum-location* ',(output-view-as-bzip2))))
       (and *test-success* (checksum= ,name ,sum result-sum)))))
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
   (funcall *commands*)
   (gl:matrix-mode :modelview))
  (gl:matrix-mode :projection)))

(defun setup ()
 (sb-int:with-float-traps-masked (:invalid :inexact :divide-by-zero :overflow :underflow)
  (when (not *glut-window-opened*)
   (cl-glut:init)
   (cl-glut:init-window-size 100 100)
   (cl-glut:init-display-mode :double :rgba)
   (cl-glut:create-window "CLNL-GLTK Test Window")
   (gl:clear-depth 1.0f0)
   (gl:depth-func :lequal)
   (gl:blend-func :src-alpha :one-minus-src-alpha)
   (gl:shade-model :smooth)
   (gl:clear-color 0 0 0 0)
   (cl-glut:display-func (cffi:get-callback 'display))
   (cl-glut:reshape-func (cffi:callback reshape))
   (cl-glut:idle-func (cffi:get-callback 'idle))
   (cl-glut:close-func (cffi:get-callback 'close-func))
   (cl-glut:keyboard-func (cffi:get-callback 'key-pressed))
   (cl-glut:motion-func (cffi:get-callback 'motion))
   (cl-glut:passive-motion-func (cffi:get-callback 'motion))
   (cl-glut:mouse-func (cffi:get-callback 'mouse))
   (cl-glut:special-func (cffi:get-callback 'special-key-pressed))
   (clnl-gltk:setup)
   (setf *glut-window-opened* t))
  (setf *inputbox* nil)))

(defun checksum-view ()
 (format nil "~{~2,'0X~}"
  (map 'list #'identity
   (ironclad:digest-sequence :sha1 (coerce (export-view) '(vector (unsigned-byte 8)))))))

(defun verify-and-update (test-name sum filename img-data)
 (format t "Verifying ~S, you can probably see how it should look via bin/run-test ~S~%" test-name test-name)
 (save-bzip2-to-ppm img-data)
 (if (probe-file "/usr/bin/display" )
  (sb-ext:run-program "/usr/bin/display" (list "cl.ppm"))
  (progn
   (format t "/usr/bin/display not found, so you need to check cl.ppm manually.  Hit enter when done.")
   (force-output)
   (read-char)))
 (delete-file "cl.ppm")
 (when (and filename (probe-file filename))
  (format t "If that looks good, enter y: ")
  (force-output)
  (when (char= #\y (read-char))
   (format t "Updating ~S~%" filename)
   (let*
    ((assoc-list (with-open-file (str filename) (read str nil)))
     (assoc-pair (assoc test-name assoc-list :test #'string=)))
    (if assoc-pair
     (setf (cdr assoc-pair) sum)
     (setf assoc-list (sort (cons (cons test-name sum) assoc-list) #'string< :key #'car)))
    (with-open-file (str filename :direction :output :if-exists :supersede) (prin1 assoc-list str)))))
 nil)

(defun output-view-as-bzip2 ()
 (let
  ((proc (sb-ext:run-program "/bin/bzip2" nil :input :stream :output :stream :wait nil)))
  (save-view-to-stream (sb-ext:process-input proc))
  (close (sb-ext:process-input proc))
  (loop
   :for seq = (make-array 80 :element-type '(unsigned-byte 8))
   :for pos = (read-sequence seq (sb-ext:process-output proc))
   :collect (subseq seq 0 pos)
   :while (= pos 80))))

; You can really only use what cames out of output-view-as-bzip2
(defun save-bzip2-to-ppm (bzip2-data)
 (with-open-file (str "cl.ppm"
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create
                  :element-type '(unsigned-byte 8))
  (let
   ((proc (sb-ext:run-program "/bin/bzip2" (list "-d") :input :stream :output :stream :wait nil)))
   (loop :for c :in bzip2-data :do (write-sequence c (sb-ext:process-input proc)))
   (close (sb-ext:process-input proc))
   (apply #'concatenate 'vector
    (loop
     :for seq = (make-array 1024 :element-type '(unsigned-byte 8))
     :for pos = (read-sequence seq (sb-ext:process-output proc))
     :do (write-sequence (subseq seq 0 pos) str)
     :while (= pos 1024))))))

(defun save-view-to-ppm ()
 (with-open-file (str "cl.ppm"
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create
                  :element-type '(unsigned-byte 8))
  (save-view-to-stream str)))

(defun save-view-to-stream (str)
 (let
  ((width 100)) ; hardcoded in interface, hardcoded here, cry for me
  (write-sequence (map 'vector #'char-code (format nil "P6~%")) str)
  (write-sequence (map 'vector #'char-code (format nil "~A ~A~%" width *height*)) str)
  (write-sequence (map 'vector #'char-code (format nil "255~%")) str)
  (let
   ((image-data (export-view)))
   (dotimes (i width)
    (dotimes (j *height*)
     (write-byte (aref image-data (+ 0 (* 4 (+ (* (- (1- *height*) i) width) j)))) str)
     (write-byte (aref image-data (+ 1 (* 4 (+ (* (- (1- *height*) i) width) j)))) str)
     (write-byte (aref image-data (+ 2 (* 4 (+ (* (- (1- *height*) i) width) j)))) str))))))

(defun export-view ()
 (sb-int:with-float-traps-masked (:invalid)
  (let
   ((fbo (first (gl:gen-framebuffers 1)))
    (render-buf (first (gl:gen-renderbuffers 1)))
    (width 100))  ; Hard coded for now, yay v1 (if you see this comment in a year, please cry for me)
   (gl:bind-framebuffer :framebuffer fbo)
   (gl:bind-renderbuffer :renderbuffer render-buf)
   (gl:renderbuffer-storage :renderbuffer :rgba8 width *height*)
   (gl:framebuffer-renderbuffer :draw-framebuffer :color-attachment0 :renderbuffer render-buf)
   (gl:viewport 0 0 width *height*)
   (render-scene)
   (gl:read-pixels 0 0 width *height* :rgba :unsigned-byte))))

(defun close-func ()
 (sb-ext:exit :code 0 :abort t))

(defun reshape (width height)
 (when (and (/= 0 width) (/= 0 height))
  (setf *height* height)
  (gl:viewport 0 0 width *height*)))

(defun key-pressed (key x y)
 (declare (ignore x y))
 (when (eql 27 key) (close-func))
 (when *inputbox* (clnl-gltk:key-pressed *inputbox* key)))

(defun mouse (button state x y)
 (declare (ignore button))
 (when (eql state :down) (clnl-gltk:mousedown *mouse-reactor* x (- *height* y)))
 (when (eql state :up) (clnl-gltk:mouseup *mouse-reactor* x (- *height* y))))

(defun motion (x y)
 (clnl-gltk:mousemove *mouse-reactor* x (- *height* y)))

(defun idle ()
 (cl-glut:post-redisplay))

(defun display ()
 (render-scene)
 (cl-glut:swap-buffers))

(cffi:defcallback display :void () (display))
(cffi:defcallback key-pressed :void ((key :uchar) (x :int) (y :int)) (key-pressed key x y))
(cffi:defcallback mouse :void ((button cl-glut:mouse-button) (state cl-glut:mouse-button-state) (x :int) (y :int))
 (mouse button state x y))

(cffi:defcallback motion :void ((x :int) (y :int)) (motion x y))
(cffi:defcallback special-key-pressed :void ((key glut:special-keys) (x :int) (y :int)) (key-pressed key x y))
(cffi:defcallback idle :void () (idle))
(cffi:defcallback close-func :void () (close-func))
(cffi:defcallback reshape :void ((width :int) (height :int)) (reshape width height))

(defun run ()
 ; I do this because I don't know who or what in the many layers
 ; is causing the floating point errors, but I definitely don't
 ; want to investigate until simply ignoring them becomes a problem.
 (sb-int:with-float-traps-masked (:invalid :inexact :divide-by-zero :overflow :underflow)
  (cl-glut:main-loop)))

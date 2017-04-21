(in-package #:clnl-gltk)

(defvar *texture*)
(defvar *base*)

(defvar *font-data*
 (with-open-file
  (str "resources/font.dat" :element-type 'unsigned-byte)
  (let
   ((seq (make-sequence 'vector (/ (* 4 (file-length str)) 3))))
   (loop
    :for idx :from 0
    :for r := (read-byte str nil)
    :for g := (read-byte str nil)
    :for b := (read-byte str nil)
    :while r
    :do
    (progn
     (setf (aref seq (* idx 4)) r)
     (setf (aref seq (+ (* idx 4) 1)) r)
     (setf (aref seq (+ (* idx 4) 2)) r)
     (setf (aref seq (+ (* idx 4) 3)) r)))
   seq)))

(defvar *font-width* 7)
(defvar *font-height* 14)

(defun font-print (str)
 "FONT-PRINT STR => RESULT

ARGUMENTS AND VALUES:

  STR: a string to be printed to screen
  RESULT: undefined

DESCRIPTION:

  FONT-PRINT prints STR to the screen.

  It affirms no assumptions that are required for it to run, in the
  interest of speed.  Those assumptions include that an opengl window
  has been opened, that all matrices are correct, and that SETUP-FONT
  has been run.

EXAMPLES:

  (font-print #P\"Hello World\" t) => nil"
 (gl:enable :texture-2d)
 (gl:bind-texture :texture-2d *texture*)
 (gl:list-base *base*)
 (gl:call-lists (map 'vector (lambda (c) (- (char-code c) 32)) str))
 (gl:disable :texture-2d))

(defun setup-font ()
 (setf *texture* (first (gl:gen-textures 1)))
 (gl:bind-texture :texture-2d *texture*)
 (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
 (gl:tex-parameter :texture-2d :texture-min-filter :linear)
 (gl:tex-image-2d :texture-2d 0 :rgba8 (* *font-width* 224) *font-height* 0 :rgba :unsigned-byte *font-data*)
 (setf *base* (gl:gen-lists 224))
 (dotimes (l 224)
  (gl:with-new-list ((+ *base* l) :compile)
   (gl:begin :quads)
   (gl:tex-coord (/ l 224d0) 1d0)
   (gl:vertex 0 0)
   (gl:tex-coord (/ (1+ l) 224d0) 1d0)
   (gl:vertex *font-width* 0)
   (gl:tex-coord (/ (1+ l) 224d0) 0d0)
   (gl:vertex *font-width* *font-height*)
   (gl:tex-coord (/ l 224d0) 0d0)
   (gl:vertex 0 *font-height*)
   (gl:end)
   (gl:translate *font-width* 0 0))))

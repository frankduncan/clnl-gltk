#!/bin/bash

tmp=$(mktemp)
# This creates an image 113 wide by 226 tall.  We need to lop off the rightmost column and topmost row
for ((x=32 ; x < 256 ; x++)) ; do
  #if ! ((x % 16)) ; then printf "\n" ; fi
  printf "\x$(printf "%x" $x)"
done | convert -font resources/Inconsolata.otf -background "#000000" -fill "#FFFFFF" -antialias -pointsize 14 -depth 8 "label:@-" rgb:$tmp

# to display the rgb (for future debugging, we use the following line):
# display -size 1569x16 -depth 8 $tmp

#the ending data is 15 pixels tall, and 7 pixels wide
sbcl --script << EOF
(with-open-file (out "resources/font.dat" :element-type 'unsigned-byte :if-exists :supersede :direction :output)
 (with-open-file (in "$tmp" :element-type 'unsigned-byte)
  (loop
   :for i :from 0 :to (1- (file-length in))
   :for c := (read-byte in)
   :do
   (when
    (and
     (<= (* 1569 3) i) ; remove top row
     (> (* 1568 3) (mod i (* 1569 3)))) ; remove right column
    (write-byte c out)))))
EOF

rm $tmp

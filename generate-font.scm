(import (chicken io))


(define inport (open-input-file "font/Segment7Standard.otf" #:binary))
(define outport (open-output-file "font.scm"))

(write-string "(import (chicken blob) (chicken format))" #f outport)
(newline outport)
(write-string "(define fontblob (string->blob (string" #f outport)

(do ((c (read-char inport) (read-char inport))
     (cnt 0 (+ cnt 1)))
    ((eq? c #!eof))
  (if (= (remainder cnt 8) 0)
      (newline outport)
      (write-char #\space outport))
  (write c outport))
(write-string ")))" #f outport)

(close-input-port inport)
(close-output-port outport)


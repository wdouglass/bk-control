(import
  scheme
  stty
  (chicken port)
  (chicken file posix)
  (chicken string)
  (chicken base)
  (chicken format)
  (chicken io))

(define (open-serial-port . args)
  (let* ((portname (car args))
	  (baudrate (if (null? (cdr args)) 9600 (cadr args)))
	  (fd (file-open portname (+ open/rdwr open/excl))))
     (let ((in (open-input-file* fd))
	   (out (open-output-file* fd)))
       (stty in `((ispeed ,baudrate) (ospeed ,baudrate) (not echo) (not icanon) (not isig) (not echoe) (not echoctl) (not echoke) (not onlcr) (not opost) (not icrnl) ignbrk))
       (make-bidirectional-port in out))))

(define (list-eqv? a b)
  (if (and (null? a) (null? b))
      #t
      (if (eqv? (car a) (car b))
	  (list-eqv? (cdr a) (cdr b))
	  #f)))

(define (field->4sig in start)
  (let ((field (substring in start (+ start 4))))
    (/ (string->number field) 100.0)))

(define (real->field in)
  (pad-string (number->string (inexact->exact (floor (* 100 in)))) #\0 4))

(define (read-until-cr p)
  (let loop ((c (read-char p))) 
    (if (or
	 (eof-object? c)
	 (eqv? c #\xd))
      '()
      (cons c (loop (read-char p))))))

(define (pad-string s char width)
  (if (< (string-length s) width)
      (let ((p (make-string (- width (string-length s)) char)))
	(string-append p s))
      s))

(define (bk-ok p)
  (let ((ok (read-until-cr p)))
    (unless (list-eqv? ok (list #\O #\K))
      (error "Bad response from BK"))))

(define (bk-write p s)
  (display s p)
  (write-char #\return p)
  (flush-output p))

(define (bk-invalid-integer-range i min max)
  (or (not (exact? i))
      (not (integer? i))
      (< i min)
      (> i max)))

(define (bk-check-valid-preset preset)
  (when (bk-invalid-integer-range preset 0 3)
      (error "Bad preset number"))
  #t)

(define (bk-set-output p on)
  (bk-write p (format #f "SOUT~A" (if on 1 0)))
  (bk-ok p))

(define (bk-set-voltage p voltage . args)
  (let ((preset (if (null? args)
		    3
		    (car args))))
    (bk-check-valid-preset preset)
    (bk-write p (format #f "VOLT~A~A" preset (real->field voltage)))
    (bk-ok p)))

(define (bk-set-current p current . args)
  (let ((preset (if (null? args)
		    3
		    (car args))))
    (bk-check-valid-preset preset)
    (bk-write p (format #f "CURR~A~A" preset (real->field current)))
    (bk-ok p)))

(define (bk-set-voltage-current p voltage current . args)
  (let ((preset (if (null? args)
		    3
		    (car args))))
    (bk-check-valid-preset preset)
    (bk-write p (format #f "SETD~A~A~A" preset (real->field voltage) (real->field current)))
    (bk-ok p)))

(define (bk-get-preset p)
  (bk-write p "GABC")
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (string->number response)))

(define (bk-set-preset p preset)
  (bk-check-valid-preset preset)
  (bk-write p (format #f "SABC~A" preset))
  (bk-ok p))

(define (bk-set-over-voltage p voltage)
  (bk-write p (format #f "SOVP~A" (real->field voltage)))
  (bk-ok p))

(define (bk-get-over-voltage p)
  (bk-write p "GOVP")
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (field->4sig response 0)))

(define (bk-set-over-current p current)
  (bk-write p (format #f "SOCP~A" (real->field current)))
  (bk-ok p))

(define (bk-get-delta-time p idx)
  (when (bk-invalid-integer-range idx 0 5)
    (error "Invalid time index"))
  (bk-write p (format #f "GDLT~A" idx))
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (string->number response)))

(define (bk-set-delta-time p idx time)
  (when (bk-invalid-integer-range idx 0 5)
    (error "Invalid time index"))
  (when (bk-invalid-integer-range time 0 20)
    (error "Invalid time index"))
  (bk-write p (format #f "SDLT~A~A" idx (pad-string (number->string time) #\0 2)))
  (bk-ok p))


(define (bk-get-sw-time p idx)
  (when (bk-invalid-integer-range idx 0 2)
    (error "Invalid time index"))
  (bk-write p (format #f "GSWT~A" idx))
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (string->number response)))

(define (bk-set-sw-time p idx time)
  (when (bk-invalid-integer-range idx 0 2)
    (error "Invalid time index"))
  (when (bk-invalid-integer-range time 0 600)
    (error "Invalid time value"))
  (bk-write p (format #f "SDLT~A~A" idx (pad-string (number->string time) #\0 3)))
  (bk-ok p))

(define (bk-run-sw p first end)
  (when (bk-invalid-integer-range first 0 2)
    (error "Invalid first index"))
  (when (bk-invalid-integer-range end 0 2)
    (error "Invalid end index"))
  (bk-write p (format #f "SDLT~A~A" first end))
  (bk-ok p))

(define (bk-stop-sw p)
  (bk-write p "STOP")
  (bk-ok p))

(define (bk-enable-keyboard p en)
  (bk-write p (if en "ENDS" "SESS"))
  (bk-ok p))

(define (bk-get-over-current p)
  (bk-write p "GOCP")
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (field->4sig response 0)))

(define (bk-configure-presets p
			      v1 i1 swtime1
			      v2 i2 swtime2
			      v3 i3 swtime3)
  (when (or
	 (bk-invalid-integer-range swtime1 0 600)
	 (bk-invalid-integer-range swtime2 0 600)
	 (bk-invalid-integer-range swtime3 0 600))
    (error "invalid time value"))
  (bk-write p
	    (format #f "SETM~A~A~A~A~A~A~A~A~A"
		    (real->field v1) (real->field i1)
		    (pad-string (number->string swtime1) #\0 3)
		    (real->field v2) (real->field i2)
		    (pad-string (number->string swtime2) #\0 3)
		    (real->field v3) (real->field i3)
		    (pad-string (number->string swtime3) #\0 3)))
  (bk-ok p))

(define (bk-get-output p)
  (bk-write p "GOUT")
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (> (string->number response) 0)))

(define (bk-get-measurements p)
  (bk-write p "GETD")
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (list (cons 'voltage (field->4sig response 0))
	  (cons 'current (field->4sig response 4))
	  (cons 'mode (if (> (string->number (substring response 8 9)) 0) 'current 'voltage)))))

(define (bk-get-all p)
  (bk-write p "GALL")
  (let ((response (apply string (read-until-cr p))))
    (bk-ok p)
    (list (cons 'abc-sele (substring response 0 1))
	  (cons 'channel (substring response 1 2))
	  (cons 'uvl (field->4sig response 2))
	  (cons 'ucl (field->4sig response 6))
	  (cons 'output (substring response 10 11))
	  (cons 'swtime1 (substring response 11 14))
	  (cons 'swtime2 (substring response 14 17))
	  (cons 'swtime3 (substring response 17 20))
	  (cons 'deltatime (map string->number (string-chop (substring response 20 32) 2)))
	  
	  (cons 'mode (substring response 32 36))
	  (cons 'setv1 (field->4sig response 36))
	  (cons 'seti1 (field->4sig response 40))
	  (cons 'setv2 (field->4sig response 44))
	  (cons 'seti2 (field->4sig response 48))
	  (cons 'setv3 (field->4sig response 52))
	  (cons 'seti3 (field->4sig response 56))
	  (cons 'setv4 (field->4sig response 60))
	  (cons 'seti4 (field->4sig response 64)))))
	  
;(let ((p (open-serial-port "/dev/ttyUSB0")))
;  (format #t "OUTPUT ~A~%" (bk-get-output p))
;  (format #t "ALL ~A~%" (bk-get-all p))
;  (format #t "MEASUREMENTS ~A~%" (bk-get-measurements p)))

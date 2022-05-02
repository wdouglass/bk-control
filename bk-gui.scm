(import
  scheme
  stty
  (srfi 18)
  (prefix bk-control "bk:")
  (prefix sdl2 "sdl2:")
  (prefix sdl2-ttf "ttf:")
  (chicken platform)
  (chicken port)
  (chicken file posix)
  (chicken string)
  (chicken base)
  (prefix (srfi 48) "fourtyeight:")
  (chicken io))

(include "font.scm")

(sdl2:set-main-ready!)
(sdl2:init! '(video events timer))
(ttf:init!)
(define window (sdl2:create-window! "BK Precision" 0 0 640 480))
(sdl2:fill-rect! (sdl2:window-surface window)
                 #f
                 (sdl2:make-color 0 128 255))
(sdl2:update-window-surface! window)

(sdl2:register-events! '(tick-event))

(define bk (bk:open-serial-port "/dev/ttyUSB0"))

(define ticker (make-thread (lambda ()
			      (let ((start (time->seconds (current-time))))
				(let loop ((x 0))
				  (thread-sleep! (seconds->time (+ x start)))
				  (let ((ev (sdl2:make-event 'tick-event)))
				    (set! (sdl2:user-event-code ev) x)
				    (sdl2:push-event! ev))
				  (loop (+ x 1)))))))

(define fontrw (sdl2:rw-from-blob fontblob))
(define font (ttf:open-font-rw fontrw #f 60))

(thread-start! ticker)

(define (label text x y)
  (let ((text-surf 
	 (ttf:render-utf8-shaded
	  font text
	  (sdl2:make-color 0 0 0)
	  (sdl2:make-color 255 255 255))))
    (sdl2:blit-surface! text-surf #f
			(sdl2:window-surface window) (sdl2:make-rect x y 0 0))))

(do ((ev (sdl2:wait-event-timeout! 2000) (sdl2:wait-event-timeout! 2000))
     (frame-cnt 0 (+ frame-cnt 1)))
    ((sdl2:quit-event? ev))
  (let* ((measurements (bk:bk-get-measurements bk))
	 (vlabel (fourtyeight:format "Voltage ~5,2F" (cdr (assoc 'voltage measurements))))
	 (ilabel (fourtyeight:format "Current ~5,2F" (cdr (assoc 'current measurements)))))
    (label vlabel 0 0)
    (let-values (((width height) (ttf:size-text font vlabel))) 
      (label ilabel 0 height)))
    
  (sdl2:update-window-surface! window))
(sdl2:rw-close! fontrw)
(sdl2:quit!)

(defpackage :repl
  (:use :cl)
  (:export
   #:start
   #:stop
   #:framebuffer
   #:load-example
   #:save))
(in-package :repl)

(defun mez-event-fifo ()
  (clim-mezzano::mezzano-mez-fifo clim-mezzano::*port*))
(defun mez-window ()
  (clim-mezzano::mezzano-port-window clim-mezzano::*port*))


(defparameter *compositor* nil)
(defparameter *quit* nil)
(defun start-compositor ()
  (stop-compositor)
  (setf *quit* nil)
  (when (not *compositor*)
    (setf *compositor* (mezzano.supervisor:make-thread
			(lambda ()
			  (loop :until *quit* :do (per-frame)))
			:name "Compositor"))))

(defun mouse ()
  (make-instance 'mezzano.gui.compositor:mouse-event
		 :x-position (random 100);; window::*mouse-x*
		 :y-position (random 100);; window::*mouse-y*
		 :x-motion (random 100)
		 :y-motion (random 100)
		 :button-state
		 (logior 
		  (if (window:button :mouse :down :left)
		      #b1
		      0)
		  (if (window:button :mouse :down :right)
		      #b10
		      0)
		  (if (window:button :mouse :down :middle)
		      #b100
		      0)
		  )
		 :window (mez-window)
		 ;;:button-change

		 ))
(defun per-frame ()
  (mezzano.gui.compositor::compositor-thread-body)
  (test5:change-image (pixels))
  (mezzano.supervisor:fifo-push (mouse) (mez-event-fifo))
  (sleep 0.3))

(defun stop-compositor ()
  (when (and
	 (bt:threadp *compositor*)
	 (bt:thread-alive-p *compositor*))
    (bt:destroy-thread *compositor*))
  (setf *compositor* nil))

(defun pixels ()
  (mezzano.supervisor::framebuffer-pixels
   (MEZZANO.SUPERVISOR:CURRENT-FRAMEBUFFER)))

(defun framebuffer ()
  (remove-duplicates
   (coerce
    (mezzano.gui::array-flatten
     (pixels))
    'list)))

(defun load-example ()
  #+quicklisp
  (ql:quickload "clim-examples"))

(defparameter *clim* nil)
(defparameter *quit-clim* nil)
(defun start-clim ()
  (stop-clim)
  (setf *quit-clim* nil)
  (when (not *clim*)
    (ql:quickload :clim-examples)
    (setf *clim* (mezzano.supervisor:make-thread
		  (lambda ()
		    (loop :until *quit-clim* :do (uiop:symbol-call
						  :clim-demo
						  :demodemo)))
		  :name "Clim"))))

(defun stop-clim ()
  (when (and
	 (bt:threadp *clim*)
	 (bt:thread-alive-p *clim*))
    (bt:destroy-thread *clim*))
  (setf *clim* nil))

(defun save ()
  (let* ((output (asdf:system-relative-pathname :mcclim-mezzano "alive.png"))
	 (save-p (y-or-n-p "Save to: ~s" output)))
    (when save-p
      (opticl:write-png-file
       output
       (pixels)))))

(defun stop ()
  (stop-compositor)
  (stop-clim))

(defun start ()
  (start-compositor)
  (start-clim)
  (test5:start))

#+nil
(defparameter +button-to-clim-alist+
  `((,(byte 1 0) . ,+pointer-left-button+)
    (,(byte 1 1) . ,+pointer-right-button+)
    (,(byte 1 2) . ,+pointer-middle-button+)
    (,(byte 1 3) . ,+pointer-wheel-up+)
    (,(byte 1 4) . ,+pointer-wheel-down+)
    ;; (,(byte 1 ???) . ,+pointer-wheel-left+)
    ;; (,(byte 1 ???) . , +pointer-wheel-right+)
    ))

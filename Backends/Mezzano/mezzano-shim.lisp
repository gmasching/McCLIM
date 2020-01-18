(in-package #:mezzano.sync)
(defun MEZZANO.SYNC:MAILBOX-RECEIVE (queue &key (wait-p nil))
  ;;[MCCLIM FIXME]
  (cond
    ((eq wait-p nil) (lparallel.queue:try-pop-queue queue))
    ((eq wait-p t) (values (lparallel.queue:pop-queue queue) t))
    (t (error "non-boolean wait-p not implemented"))))
(defun MEZZANO.SYNC:MAILBOX-SEND (object queue &key (wait-p t))
  ;;[MCCLIM FIXME]
  (declare (ignore wait-p))
  (lparallel.queue:push-queue object queue))
(defun MEZZANO.SYNC:MAKE-MAILBOX (&key name capacity)
  ;;[MCCLIM FIXME]
  (declare (ignore name capacity))
  (lparallel.queue:make-queue))

(in-package #:mezzano.supervisor)
(defun MEZZANO.SUPERVISOR:CURRENT-THREAD ()
  (bt:current-thread))
(defun MEZZANO.SUPERVISOR:DEBUG-PRINT-LINE (string)
  ;;[MCCLIM FIXME]
  (print string))
(defun MEZZANO.SUPERVISOR:PANIC (string)
  (error "***PANIC***~s" string))
(defun MEZZANO.SUPERVISOR:MAKE-THREAD (function &key name)
  (bt:make-thread function :name name))
(defun MEZZANO.SUPERVISOR:MAKE-FIFO (size &key (element-type 't))
  (declare (ignore element-type))
  (mezzano.sync:make-mailbox :capacity size))
(defun MEZZANO.SUPERVISOR:FIFO-POP (fifo &optional (wait-p t))
  (mezzano.sync:mailbox-receive fifo :wait-p wait-p))
(defun MEZZANO.SUPERVISOR:FIFO-PUSH (value fifo &optional (wait-p t))
  (mezzano.sync:mailbox-send value fifo :wait-p wait-p))

(defparameter *data* nil)
(defun framebuffer-blit (fb nrows ncols from-array from-row from-col to-row to-col)
  "Update a region of the system framebuffer.
Returns false if the framebuffer is invalid, true otherwise.
If the framebuffer is invalid, the caller should fetch the current framebuffer and discard the old one."
  (when (not (eql (array-rank from-array) 2))
    (error 'type-error
           :expected-type '(array (unsigned-byte 32) (* *))
           :datum from-array))
  ;; Don't write to the top row of pixels, that's where the lights are.
  #+nil
  (case *light-position*
    (:top
     (when (<= to-row 0)
       (incf from-row (- 1 to-row))
       (decf nrows (- 1 to-row))
       (setf to-row 1)))
    (:bottom
     (when (>= (+ to-row nrows) (1- (framebuffer-height fb)))
       (setf nrows (- (framebuffer-height fb) to-row 1)))))
  ;; Dismember the from-array.
  (let (;;(from-offset 0)
        ;;(from-storage (sys.int::%complex-array-storage from-array))
        (from-width (array-dimension from-array 1))
        (from-height (array-dimension from-array 0)))
    ;; Check for displaced arrays.
    #+nil
    (when (sys.int::%complex-array-info from-array)
      (when (integerp from-storage)
        ;; Memory array
        (error "Can't copy from a memory array"))
      (setf from-offset (sys.int::%complex-array-info from-array)
            from-storage (sys.int::%complex-array-storage from-storage)))
    ;; Storage must be a simple ub32 array.
    #+nil
    (when (not (sys.int::%object-of-type-p from-storage sys.int::+object-tag-array-unsigned-byte-32+))
      (error 'type-error
             :expected-type (array (unsigned-byte 32) (* *))
             :datum from-array))
    ;; Clamp parameters.
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (decf from-row to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (decf from-col to-col)
      (setf to-col 0))
    ;; Clamp from row/column.
    (when (< from-row 0)
      (incf nrows from-row)
      (decf to-row from-row)
      (setf from-row 0))
    (when (< from-col 0)
      (incf ncols from-col)
      (decf to-col from-col)
      (setf from-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (max 0 (min nrows (- (framebuffer-height fb) to-row) (- from-height from-row))))
    (setf ncols (max 0 (min ncols (- (framebuffer-width fb) to-col) (- from-width from-col))))
    ;; Disable snapshotting and the GC while this is in progress, as we're touching physical memory.
    #+nil
    (with-pseudo-atomic
      (when (not (eql fb *current-framebuffer*))
        (return-from framebuffer-blit nil))
      (let ((to-base (convert-to-pmap-address
                      (+ (framebuffer-base-address fb)
                         (* to-row (framebuffer-pitch fb))
                         (* to-col (framebuffer-bytes-per-pixel fb)))))
            (to-pitch (framebuffer-pitch fb))
            (from-base (+ (sys.int::lisp-object-address from-storage)
                          (- sys.int::+tag-object+)
                          8
                          (* from-row from-width 4)
                          (* from-col 4)))
            (from-pitch (* from-width 4))
            (blit-fn (framebuffer-blit-fn fb)))
        (dotimes (i nrows)
          (funcall blit-fn to-base from-base ncols)
          (incf to-base to-pitch)
          (incf from-base from-pitch))))
    #+nil
    (funcall (framebuffer-damage-fn fb)
             to-col to-row
             ncols nrows
             nil)
    ;;[MCCLIM]
    (let ((fb-array (framebuffer-pixels fb)))
      (dotimes (x nrows)
	(dotimes (y ncols)
	  (let ((value (aref from-array (+ y from-row) (+ x from-col))))
	    (multiple-value-bind (a r g b) (mezzano.gui:unpack-color value)
	      (setf (aref fb-array (+ y to-row) (+ x to-col) 0) r
		    (aref fb-array (+ y to-row) (+ x to-col) 1) g
		    (aref fb-array (+ y to-row) (+ x to-col) 2) b
		    (aref fb-array (+ y to-row) (+ x to-col) 3) a))))))
    t))

(defclass framebuffer ()
  ((width :initarg :width :accessor framebuffer-width)
   (height :initarg :height :accessor framebuffer-height)
   (pixels :initarg :pixels :accessor framebuffer-pixels)))

(defun make-framebuffer (width height)
  (make-instance 'framebuffer
		 :width width
		 :height height
		 :pixels (make-array (list height width 4) :element-type '(unsigned-byte 8))))
(defparameter *framebuffer* (make-framebuffer 512 512))
(defun MEZZANO.SUPERVISOR:CURRENT-FRAMEBUFFER () *framebuffer*)
;;(defun MEZZANO.SUPERVISOR:FRAMEBUFFER-WIDTH (fb) (framebuffer-width fb)) 
;;(defun MEZZANO.SUPERVISOR:FRAMEBUFFER-HEIGHT (fb) (framebuffer-height fb)) 

(in-package #:mezzano.internals)
(defparameter *log-p* t)
(defmacro MEZZANO.INTERNALS:LOG-AND-IGNORE-ERRORS (&body body)
  `(block out
     (handler-case (progn ,@body)
       (error (c)
	 (when *log-p* (print c))
	 (return-from out)))))

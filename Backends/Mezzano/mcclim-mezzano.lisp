;;;;************************************************************************;;;;
;;;;<EVENTS>
(in-package :clim-mezzano)

;; minimum mezzano frame size for resize events
(defparameter *minimum-width* 100)
(defparameter *minimum-height* 100)

;; These x y variables are always in mcclim "units", that is they
;; always apply to the *last-mouse-sheet*, not the mezzano frame
(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)
(defvar *last-graft-x* 0)
(defvar *last-graft-y* 0)
(defvar *last-mouse-sheet* nil)

(defvar *last-modifier-state* 0)

(defvar *char->name* (make-hash-table :test #'eql))

;;;======================================================================
;;;
;;; mez-event->mcclim-event - converts mezzano events to mcclim events
;;;
;;;======================================================================

(defgeneric mez-event->mcclim-event (mcclim-fifo event))

(defmethod mez-event->mcclim-event (mcclim-fifo event)
  ;; Default case - log event and ignore
  (debug-format "mcclim backend unexpected event")
  (debug-format "    ~S" event))

;;;======================================================================
;;; Keyboard Events
;;;======================================================================

(defun get-name (char)
  (let ((name (gethash char *char->name*)))
    (if name
        name
        (setf (gethash char *char->name*) (intern (string char) :keyword)))))

(defparameter +modifier-to-clim-alist+
  `((:shift    . ,+shift-key+)
    (:control  . ,+control-key+)
    (:meta     . ,+meta-key+)
    (:super    . ,+super-key+)))

(defun compute-modifier-state (modifier-keys)
  (let ((modifier 0))
    (dolist (key modifier-keys)
      (let ((modifier-info (assoc key +modifier-to-clim-alist+)))
        (if modifier-info
            (setf modifier (logior modifier (cdr modifier-info)))
            (debug-format "Unknown modifier key ~S" key))))
    (setf *last-modifier-state* modifier)))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:key-event))
  (let* ((releasep (mos:key-releasep event))
         (char (mos:key-key event))
         (name (get-name char))
         (modifier-state (compute-modifier-state (mos:key-modifier-state event)))
         (mez-window (mos:window event))
         (sheet (port-lookup-sheet *port* mez-window)))
    (when sheet
      (mos:fifo-push
       (make-instance (if releasep 'key-release-event 'key-press-event)
                      :key-name name
                      :key-character char
                      :x *last-mouse-x*
                      :y *last-mouse-y*
                      :graft-x *last-graft-x*
                      :graft-y *last-graft-y*
                      :sheet (or (frame-properties (pane-frame sheet) 'focus)
                                 sheet)
                      :modifier-state modifier-state)
       mcclim-fifo
       nil))))

;;;======================================================================
;;; Pointer Events
;;;======================================================================

(defparameter +button-to-clim-alist+
  `((,(byte 1 0) . ,+pointer-left-button+)
    (,(byte 1 1) . ,+pointer-right-button+)
    (,(byte 1 2) . ,+pointer-middle-button+)
    (,(byte 1 3) . ,+pointer-wheel-up+)
    (,(byte 1 4) . ,+pointer-wheel-down+)
    ;; (,(byte 1 ???) . ,+pointer-wheel-left+)
    ;; (,(byte 1 ???) . , +pointer-wheel-right+)
    ))

(defun compute-mouse-buttons (buttons)
  (let ((result 0))
    (dolist (tr +button-to-clim-alist+)
      (when (ldb-test (car tr) buttons)
        (setf result (logior result (cdr tr)))))
    result))

(defun pointer-motion-event (mcclim-fifo sheet event)
  (let ((time 0))
    (mos:fifo-push
     (make-instance 'pointer-motion-event
                    :pointer 0
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x *last-graft-x*
                    :graft-y *last-graft-y*
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun pointer-button-event (mcclim-fifo sheet event)
  (let* ((buttons (compute-mouse-buttons (mos:mouse-button-state event)))
         (change (compute-mouse-buttons (mos:mouse-button-change event)))
         (time 0))
    (mos:fifo-push
     (make-instance (if (= (logand buttons change) 0)
                        'pointer-button-release-event
                        'pointer-button-press-event)
                    :pointer 0
                    :button change
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x *last-graft-x*
                    :graft-y *last-graft-y*
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun mouse-exit-event (mcclim-fifo sheet event)
  (let ((time 0))
    (mos:fifo-push
    (make-instance 'pointer-exit-event
                   :pointer 0
                   :x *last-mouse-x*
                   :y *last-mouse-y*
                   :graft-x *last-graft-x*
                   :graft-y *last-graft-y*
                   :sheet sheet
                   :modifier-state *last-modifier-state*
                   :timestamp time)
    mcclim-fifo
    nil)))

(defun mouse-enter-event (mcclim-fifo sheet event)
  (let ((time 0))
    (mos:fifo-push
     (make-instance 'pointer-enter-event
                    :pointer 0
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x *last-graft-x*
                    :graft-y *last-graft-y*
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun frame-mouse-event (mcclim-fifo sheet mez-frame event)
  (handler-case
      (mos:frame-mouse-event mez-frame event)
    (mos:close-button-clicked ()
      (mos:fifo-push
       (make-instance 'window-manager-delete-event :sheet sheet)
       mcclim-fifo
       nil))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:mouse-event))
  (let* ((mez-window (mos:window event))
         (mouse-x    (mos:mouse-x-position event))
         (mouse-y    (mos:mouse-y-position event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (sheet      (port-lookup-sheet *port* mez-window)))
    (when mez-mirror
      (with-slots (mez-frame dx dy width height) mez-mirror
        (setf *last-mouse-x* (- mouse-x dx)
              *last-mouse-y* (- mouse-y dy)
              *last-graft-x* (+ mouse-x (mos:window-x mez-window))
              *last-graft-y* (+ mouse-y (mos:window-y mez-window)))
        (cond ((and mez-frame
                    (or (mos:in-frame-header-p mez-frame mouse-x mouse-y)
                        (mos:in-frame-border-p mez-frame mouse-x mouse-y)))
               (when *last-mouse-sheet*
                 (mouse-exit-event mcclim-fifo *last-mouse-sheet* event)
                 (setf *last-mouse-sheet* nil))
               (frame-mouse-event mcclim-fifo sheet mez-frame event))

              ((= (mos:mouse-button-change event) 0)
               (funcall (mos:set-cursor-function mez-frame)
                        :default)
               (cond ((eq sheet *last-mouse-sheet*)
                      (pointer-motion-event mcclim-fifo sheet event))
                     (T
                      (when *last-mouse-sheet*
                        (mouse-exit-event mcclim-fifo *last-mouse-sheet* event))
                      (mouse-enter-event mcclim-fifo sheet event)
                      (setf *last-mouse-sheet* sheet))))
              (T
               (unless (eq sheet *last-mouse-sheet*)
                 (when *last-mouse-sheet*
                   (mouse-exit-event mcclim-fifo *last-mouse-sheet* event))
                 (mouse-enter-event mcclim-fifo sheet event)
                 (setf *last-mouse-sheet* sheet))
               (pointer-button-event mcclim-fifo sheet event)))))))

;;;======================================================================
;;; Activation Events
;;;======================================================================

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:window-activation-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (port-lookup-sheet *port* mez-window))
         (focus (and sheet (frame-query-io (pane-frame sheet)))))
    (when mez-frame
      (setf (mos:activep mez-frame)
            (mos:state event))
      (mos:draw-frame mez-frame)
      ;; HACK: This "fixes" the initial rendering issue in the listener.
      (sleep 0.1)
      (mos:fifo-push
       (with-slots (width height) mez-mirror
         (make-instance 'window-repaint-event
                        :timestamp 0
                        :sheet sheet
                        :region (make-rectangle* 0 0 width height)))
       mcclim-fifo))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:quit-event))
  (let* ((mez-window (mos:window event))
         (sheet (port-lookup-sheet *port* mez-window)))
    (when sheet
      (mos:fifo-push
       (make-instance 'window-manager-delete-event
                      :sheet sheet)
       mcclim-fifo
       nil))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:window-close-event))
  ;;; TODO - what needs to happen here anything?
  )

;;;======================================================================
;;; Resize events
;;;======================================================================

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:resize-request-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (port-lookup-sheet *port* mez-window))
         (fwidth (max *minimum-width* (mos:width event)))
         (fheight (max *minimum-height* (mos:height event))))
    (multiple-value-bind (dw dh) (size-deltas mez-mirror)
      (when (and mez-frame
                 (or (/= fwidth (mos:width mez-window))
                     (/= fheight (mos:height mez-window))))
        (let* ((surface (mos:make-surface fwidth fheight))
               (pixels (mos:surface-pixels surface))
               (width (- fwidth dw))
               (height(- fheight dh)))
          (mos:resize-frame mez-frame surface)
          (mos:resize-window
           mez-window surface
           :origin (mos:resize-origin event))

          (setf (slot-value mez-mirror 'mez-pixels) pixels
                (slot-value mez-mirror 'fwidth) fwidth
                (slot-value mez-mirror 'fheight) fheight
                (slot-value mez-mirror 'width) width
                (slot-value mez-mirror 'height) height)
          (mos:fifo-push
           (make-instance 'window-configuration-event
                          :sheet sheet
                          :region nil
                          :width width
                          :height height
                          :x (mos:window-x mez-window)
                          :y (mos:window-y mez-window))
           mcclim-fifo
           nil)
          )))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:resize-event))
  ;;; TODO - what needs to happen here anything?
  )

;;;;</EVENTS>
;;;;************************************************************************;;;;
;;;;<GRAFT>
;;;  (c) copyright 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :clim-mezzano)

(defparameter +width-dots-per-inch+ 96)
(defparameter +height-dots-per-inch+ 96)

(defclass mezzano-graft (graft)
  (width
   height))

(defmethod sheet-direct-mirror ((sheet mezzano-graft))
  (port-lookup-mirror (port sheet) sheet))

(defmethod (setf sheet-direct-mirror) (mirror (sheet mezzano-graft))
  (port-register-mirror (port sheet) sheet mirror))

(defmethod initialize-instance :after ((graft mezzano-graft) &rest args)
  (declare (ignore args))
  (debug-format "make-instance mezzano-graft")
  (let ((framebuffer (mos:current-framebuffer)))
    (setf (slot-value graft 'width) (mos:framebuffer-width framebuffer)
          (slot-value graft 'height) (mos:framebuffer-height framebuffer))))

(defmethod graft-width ((graft mezzano-graft) &key (units :device))
  (ecase units
    (:device (slot-value graft 'width))
    (:inches (/ (slot-value graft 'width) +width-dots-per-inch+))
    (:millimeters (/ (slot-value graft 'width) +width-dots-per-inch+ 2.54))
    (:screen-sized 1)))

(defmethod graft-height ((graft mezzano-graft) &key (units :device))
  (ecase units
    (:device (slot-value graft 'height))
    (:inches (/ (slot-value graft 'height) +width-dots-per-inch+))
    (:millimeters (/ (slot-value graft 'height) +width-dots-per-inch+ 2.54))
    (:screen-sized 1)))

;;;;</GRAFT>
;;;;************************************************************************;;;;
;;;;<MEDIUM>
(in-package :clim-mezzano)

(defvar *debug-format-control* nil)
(defvar *debug-format-messages* NIL)

(defun debug-format (string &rest args)
  (cond ((eq *debug-format-control* :console)
         (mos:debug-print-line (apply #'format nil string args)))
        ((eq *debug-format-control* :list)
         (push (apply #'format nil string args) *debug-format-messages*))
        ((streamp *debug-format-control*)
         (apply #'format *debug-format-control* string args))))

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2.
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))

;;; MEZZANO-MEDIUM class

(defclass mezzano-medium (render-medium-mixin basic-medium)
  ())

(defmethod text-style-fixed-width-p (text-style (medium mezzano-medium))
  (eql (text-style-family text-style) :fixed))

;;;;</MEDIUM>
;;;;************************************************************************;;;;
;;;;<PORT>
(in-package :clim-mezzano)

(defvar *port* NIL)

;;======================================================================
;; Define pointer class
;;======================================================================

(defclass mezzano-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defmethod synthesize-pointer-motion-event ((pointer mezzano-pointer))
  ;; TODO - write this function
  )

(defmethod pointer-position ((pointer mezzano-pointer))
  (values *last-mouse-x* *last-mouse-y*))

;;======================================================================
;; Define port class
;;======================================================================

;;
;; All mezzano events are piped through a single fifo (mez-fifo) which
;; is read by get-next-event. get-next-event translates mezzano events
;; to mcclim events using mez-window->sheet to figure out which mcclim
;; sheet corresponds to the mezzano window which received the
;; event. The mcclim events are placed in mcclim-fifo because a single
;; mezzano event can generate multiple mcclim events. get-next-event
;; then returns the events from mcclim-fifo one at a time.
;;

(defclass mezzano-port (render-port-mixin
                        standard-event-port-mixin
                        standard-port)
  ((pointer            :reader   port-pointer)
   (window             :accessor mezzano-port-window)
   (display-thread     :accessor mezzano-display-thread)
   (event-thread       :accessor mezzano-event-thread)
   (cursor-table       :accessor mezzano-cursor-table)
   (mez-window->sheet  :initform (make-hash-table :test #'eq))
   (mez-window->mirror :initform (make-hash-table :test #'eq))
   (mez-fifo           :initform (mos:make-fifo 50)
                       :reader   mezzano-mez-fifo)
   (mcclim-fifo        :initform (mos:make-fifo 10)
                       :reader   mezzano-mcclim-fifo)))

(defmethod port-lookup-sheet ((port mezzano-port) (mez-window mos:window))
  (gethash mez-window (slot-value port 'mez-window->sheet)))

(defmethod port-lookup-mirror ((port mezzano-port) (mez-window mos:window))
  (gethash mez-window (slot-value port 'mez-window->mirror)))

(defun parse-mezzano-server-path (path)
  (let ((mirroring (mirror-factory (getf path :mirroring)))
        (result (list :mezzano
                      :host       :mezzano
                      :display-id 0
                      :screen-id  0
                      :protocol   :native)))
    (if mirroring
        (nconc result (list :mirroring  mirroring))
        result)))

(setf (get :mezzano :port-type) 'mezzano-port)
(setf (get :mezzano :server-path-parser) 'parse-mezzano-server-path)

(defun initialize-display-thread (port)
  (mos:make-thread
   (lambda ()
     (loop
        (handler-case
            (maphash #'(lambda (key val)
                         (when (typep key 'mezzano-mirrored-sheet-mixin)
                           (image-mirror-to-mezzano (sheet-mirror key))))
                     (slot-value port 'climi::sheet->mirror))
          (condition (condition)
            (format *debug-io* "~A~%" condition)))
        (sleep 0.01)))
   :name "McCLIM Display"))

(defun initialize-event-thread (port)
  (when clim-sys:*multiprocessing-p*
    (mos:make-thread
     (lambda ()
       (let (#+nil ;;[MCCLIM]
	     (*terminal-io* (make-instance 'mos:popup-io-stream
                                           :title "McCLIM event loop console")))
         (loop
            (with-simple-restart
                (restart-event-loop
                 "Restart CLIM's event loop.")
              (loop
                 (process-next-event port))))))
     :name "McCLIM Events")))

(defmethod initialize-instance :after ((port mezzano-port) &rest args)
  (declare (ignore args))
  (setf *port* port
        (slot-value port 'pointer) (make-instance 'mezzano-pointer :port port)
        (mezzano-port-window port) (mos:current-framebuffer))
  (push (make-instance 'mezzano-frame-manager :port port)
	(slot-value port 'frame-managers))
  (make-graft port)
  (clim-extensions:port-all-font-families port)
  (setf (mezzano-display-thread port) (initialize-display-thread port)
        (mezzano-event-thread port) (initialize-event-thread port)))

(defmethod destroy-port :before ((port mezzano-port))
  ;; TODO - there's more to clean up here:
  ;; close any mez-windows/mez-frames
  ;; destroy the associated frame-manager
  (bt:destroy-thread (mezzano-display-thread port))
  (bt:destroy-thread (mezzano-event-thread port)))

(defmethod port-enable-sheet ((port mezzano-port) (mirror mirrored-sheet-mixin))
  )

(defmethod port-frame-keyboard-input-focus ((port mezzano-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port mezzano-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defun create-mezzano-mirror (port sheet title width height top-border
                              &key (close-button-p t) (resizablep t))
  ;; Assumes left, right and bottom borders are 1, which is the
  ;; default when creating a frame
  (setf width (max 5 width))
  (setf height (max 5 height))
  (let* ((fwidth (+ width 2))
         (fheight (+ height 1 top-border))
         (mirror (make-instance 'mezzano-mirror))
         (fifo (mezzano-mez-fifo port))
         (window (mos:make-window fifo fwidth fheight))
         (surface (mos:window-buffer window))
         (frame (make-instance 'mos:frame
                               :top top-border
                               :framebuffer surface
                               :title title
                               :close-button-p close-button-p
                               :resizablep resizablep
                               :damage-function (mos:default-damage-function window)
                               :set-cursor-function (mos:default-cursor-function window))))
    (setf (slot-value mirror 'mcclim-render-internals::dirty-region) nil
          (slot-value mirror 'fwidth) fwidth
          (slot-value mirror 'fheight) fheight
          (slot-value mirror 'dx) 1
          (slot-value mirror 'dy) top-border
          (slot-value mirror 'width) width
          (slot-value mirror 'height) height
          (slot-value mirror 'mez-pixels) (mos:surface-pixels surface)
          (slot-value mirror 'mez-window) window
          (slot-value mirror 'mez-frame) frame)
    (port-register-mirror port sheet mirror)
    (setf (gethash window (slot-value port 'mez-window->sheet)) sheet
          (gethash window (slot-value port 'mez-window->mirror)) mirror)
    mirror))

(defmethod realize-mirror ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (%realize-mirror port sheet)
  (port-lookup-mirror port sheet)
  )

(defmethod realize-mirror ((port mezzano-port) (pixmap pixmap))
  )

(defmethod %realize-mirror ((port mezzano-port) (sheet basic-sheet))
  (debug-format "%realize-mirror ((port mezzano-port) (sheet basic-sheet))")
  (debug-format "    ~S ~S" port sheet)
  (break)
  )

(defmethod %realize-mirror ((port mezzano-port) (sheet top-level-sheet-pane))
  (let* ((q (compose-space sheet))
         (frame (pane-frame sheet))
         (mirror (create-mezzano-mirror
                  port sheet
                  (frame-pretty-name frame)
                  (round-coordinate (space-requirement-width q))
                  (round-coordinate (space-requirement-height q))
                  19)))
    (setf (slot-value mirror 'top-levelp) t)
    mirror))

(defmethod %realize-mirror ((port mezzano-port) (sheet unmanaged-top-level-sheet-pane))
  (create-mezzano-mirror port sheet "" 300 300 2
                         :close-button-p NIL
                         :resizablep NIL))

(defmethod make-medium ((port mezzano-port) sheet)
  (make-instance 'mezzano-medium
		 ;; :port port
		 ;; :graft (find-graft :port port)
		 :sheet sheet))


(defmethod make-graft ((port mezzano-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'mezzano-graft
                              :port port
                              :mirror (mezzano-port-window port)
                              :orientation orientation
                              :units units)))
    (climi::%%set-sheet-region
     (make-bounding-rectangle 0 0 (graft-width graft) (graft-height graft))
     graft)
    (push graft (port-grafts port))
    graft))

(defmethod graft ((port mezzano-port))
  (first (port-grafts port)))


(defmethod port-force-output ((port mezzano-port))
  (maphash
   #'(lambda (key val)
       (when (typep key 'mezzano-mirrored-sheet-mixin)
         (mcclim-render-internals::%mirror-force-output (sheet-mirror key))))
   (slot-value port 'climi::sheet->mirror))
  )

;;
;; Polling for events every 10ms
;; TODO would be better if we could set a timer and wait on the timer
;; or a new fifo entry instead.
;;

;; return :timeout on timeout
;;
(defmethod get-next-event ((port mezzano-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let ((mez-fifo (mezzano-mez-fifo port))
        (mcclim-fifo (mezzano-mcclim-fifo port)))
    (if (null timeout)
        ;; check for a mcclim event - if one is available return
        ;; it. If none available wait for a mezzano event, which may
        ;; or may not generate a mcclim event, so check for a mcclim
        ;; event again.  Don't have to worry about a race condition of
        ;; a mcclim event arriving after checking while waiting on a
        ;; mezzano event because only this thread puts events in the
        ;; mcclim event fifo.

        ;; TODO - this loop needs to read from the mcclim-fifo until
        ;; it's empty before going to the mezzano-fifo. This doesn't
        ;; happen anymore because it sometimes ignores a mcclim
        ;; event. It works accidently because the only mcclim event it
        ;; ignores is a keyboard-event which only occurs singlely in
        ;; the mcclim-fifo.
        (loop
           (multiple-value-bind (event validp)
               (mos:fifo-pop mcclim-fifo nil)
             (when validp
               ;; ignore keyboard events if there's no event sheet to
               ;; handle them
               (unless (and (typep event 'keyboard-event)
                            (null (event-sheet event)))
                 (return event))))
           (mez-event->mcclim-event
            mcclim-fifo (mos:fifo-pop mez-fifo t)))
        (mos:panic "timeout not supported")
        ;; (loop
        ;;    (multiple-value-bind (event validp)
        ;;        (mos:fifo-pop fifo NIL)
        ;;      (cond (validp (return (convert-event event)))
        ;;            ((< timeout 0.005) (return :timeout))
        ;;            (T (sleep 0.01)
        ;;               (decf timeout 0.01)))))
        )))

;;; Pixmap

(defmethod destroy-mirror ((port mezzano-port) (pixmap mcclim-render-internals:image-pixmap-mixin))
  (when (port-lookup-mirror port pixmap)
    (port-unregister-mirror port pixmap (port-lookup-mirror port pixmap))))

(defmethod realize-mirror ((port mezzano-port) (pixmap image-pixmap-mixin))
  (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port pixmap mirror)
    (mcclim-render-internals::%make-image mirror pixmap)))

(defmethod port-allocate-pixmap ((port mezzano-port) sheet width height)
  (let ((pixmap (make-instance 'mezzano-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port mezzano-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod set-sheet-pointer-cursor
    ((port mezzano-port) (sheet mirrored-sheet-mixin) cursor)
  ;; TODO - do we need to do anything here - or is the pointer cursor
  ;; completely managed by compositor?
  (unless (eq cursor :default)
    (debug-format "set-sheet-pointer-cursor ((port mezzano-port) (sheet mirrored-sheet-mixin) cursor)")
    (debug-format "    ~S ~S ~S" port sheet cursor)
    (break))
  )

;; TODO: Theses should show/hide the window, but that needs compositor support.
;; They're stubbed out because the listener requires them.

(defmethod port-enable-sheet ((port mezzano-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port mezzano-port) (mirror mirrored-sheet-mixin))
  nil)

;;;;</PORT>
;;;;************************************************************************;;;;
;;;;<MIRROR>
(in-package :clim-mezzano)

;;;
;;; fwidth/fheight are width and height including frame
;;; width and height are the interior width and height available to mcclim
;;; dx/dy are the x and y offsets to the interior available to mcclim
;;;
(defclass mezzano-mirror (image-mirror-mixin)
  ((top-levelp :initform nil)
   (fwidth     :initform 0)
   (fheight    :initform 0)
   (width      :initform 0)
   (height     :initform 0)
   (dx         :initform 0)
   (dy         :initform 0)
   (mez-pixels :initform nil)
   (mez-window :initform nil)
   (mez-frame  :initform nil)
   (mez-dirty-region :initform +nowhere+)
   (skip-count :initform 0)))

(defun size-deltas (mez-mirror)
  (with-slots (fwidth fheight width height) mez-mirror
    (values (- fwidth width) (- fheight height))))

(defun resize-mirror (mirror new-width new-height)
  (setf new-width (max 5 new-width))
  (setf new-height (max 5 new-height))
  (with-slots (fwidth fheight width height mez-frame mez-window) mirror
    (when (or (/= width new-width) (/= height new-height))
      (setf fwidth (+ new-width (- fwidth width))
            fheight (+ new-height (- fheight height))
            width new-width
            height new-height)
      (let* ((surface (mos:make-surface fwidth fheight))
             (pixels (mos:surface-pixels surface)))
        (mos:resize-frame mez-frame surface)
        (mos:resize-window mez-window surface)
        (setf (slot-value mirror 'mez-pixels) pixels)
        (mos:draw-frame mez-frame)))))

(defmethod %create-mirror-image :after ((mirror mezzano-mirror) width height)
  (resize-mirror mirror width height))

(defgeneric image-mirror-to-mezzano (sheet))

(defmethod image-mirror-to-mezzano ((sheet image-mirror-mixin))
  )

(defun image-mirror-put (mez-window dx dy width height dirty-r)
  (when mez-window
    ;; (debug-format "image-mirror-put ~S ~S ~S ~S ~S"
    ;;               mez-window dx dy width height)
    #+nil (mos:damage-window
     mez-window
     dx
     dy
     width
     height)
    (map-over-region-set-regions
     #'(lambda (region)
         (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
             (region-intersection region (make-rectangle* 0 0 width height))
           (let ((width (round (- max-x min-x)))
                 (height (round (- max-y min-y))))
             ;; (debug-format "image-mirror-put ~S ~S ~S ~S ~S"
             ;;               mez-window
             ;;               (max 0 min-x)
             ;;               (max 0 min-y)
             ;;               width height)
             (mos:damage-window
              mez-window
              (+ dx (round (max 0 min-x)))
              (+ dy (round (max 0 min-y)))
              width
              height))))
     dirty-r)
    ))

(defun image-mirror-pre-put (mirror mez-pixels dx dy width height dirty-r)
  (let ((pixels (climi::pattern-array (image-mirror-image mirror))))
    (declare (type opticl-rgb-image-pixels pixels)
             (optimize speed (safety 0) (debug 0)))
    (map-over-region-set-regions
     #'(lambda (region)
         (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
           (region-intersection region (make-rectangle* 0 0
                                                        (1- width) (1- height)))
           (when mez-pixels
             (do ((x min-x)
                  (y min-y (1+ y)))
                 ((> x max-x))
               (setf (aref mez-pixels (+ dy y) (+ dx x))
                     (logior #xFF000000 (ash (aref pixels y x) -8)))
               (when (= y max-y)
                 (incf x)
                 (setf y min-y))))))
     dirty-r)))

(defmethod image-mirror-to-mezzano ((sheet mezzano-mirror))
  (declare (optimize speed))
  (with-slots (mcclim-render-internals::image-lock
               mcclim-render-internals::dirty-region
               mcclim-render-internals::finished-output
               MCCLIM-RENDER-INTERNALS::updating-p
               dx dy
               width height
               mez-window
               mez-dirty-region skip-count) sheet
    (when (not (region-equal mez-dirty-region +nowhere+))
      (let ((reg))
        (climi::with-lock-held (mcclim-render-internals::image-lock)
          (setf reg mez-dirty-region)
          (setf mez-dirty-region +nowhere+))
        (image-mirror-put mez-window dx dy width height reg)))))

(defmethod port-set-mirror-region
    ((port mezzano-port) (mirror mezzano-mirror) mirror-region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) mirror-region
    (resize-mirror mirror
                   (1+ (ceiling (- max-x min-x)))
                   (1+ (ceiling (- max-y min-y)))))
  (mos:draw-frame (slot-value mirror 'mez-frame)))

(defmethod port-set-mirror-transformation
    ((port mezzano-port) (mirror mezzano-mirror) mirror-transformation)
  (unless (slot-value mirror 'top-levelp)
    (multiple-value-bind (x y) (transform-position mirror-transformation 0 0)
      (mos:move-window (slot-value mirror 'mez-window) (floor x) (floor y)))))

(defmethod destroy-mirror ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-mirror sheet)))
    (when (typep mirror 'mezzano-mirror)
      (let ((mez-window (slot-value mirror 'mez-window)))
        (remhash mez-window (slot-value port 'mez-window->sheet))
        (remhash mez-window (slot-value port 'mez-window->mirror))
        (mos:close-window mez-window)))
    (when (port-lookup-mirror port sheet)
      (port-unregister-mirror port sheet (sheet-mirror sheet)))))

(defmethod port-disable-sheet ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-mirror sheet)))
    (when (and (typep mirror 'mezzano-mirror)
               (eq sheet (port-lookup-sheet port mirror)))
      ;; disabling a top level sheet - close the window and delete mappings
      (let ((mez-window (slot-value mirror 'mez-window)))
        (remhash mez-window (slot-value port 'mez-window->sheet))
        (remhash mez-window (slot-value port 'mez-window->mirror))
        (mos:close-window mez-window)))))

(defmethod mcclim-render-internals::%mirror-force-output ((mirror mezzano-mirror))
  (with-slots (mcclim-render-internals::image-lock
               mcclim-render-internals::dirty-region
               dx dy
               width height
               mez-pixels
               mez-dirty-region) mirror
    (when mcclim-render-internals::dirty-region
      (climi::with-lock-held (mcclim-render-internals::image-lock)
        (when mcclim-render-internals::dirty-region
          (setf mez-dirty-region
                (region-union mez-dirty-region
                              mcclim-render-internals::dirty-region))
;;          (debug-format "dirty regin ~S" mez-dirty-region)
          (image-mirror-pre-put mirror mez-pixels dx dy width height mez-dirty-region)
          (setf mcclim-render-internals::dirty-region nil))))))

;;;;</MIRROR>
;;;;************************************************************************;;;;
;;;;<MIRRORED-SHEETS>
(in-package :clim-mezzano)

(defclass mezzano-mirrored-sheet-mixin (image-sheet-mixin mirrored-sheet-mixin)
  ())

;;;
;;; Updating
;;;

;;;;; this is evil.
(defmethod allocate-space :after ((sheet mezzano-mirrored-sheet-mixin) width height)
;; TODO - does mezzano need these hints?
  ;; (when (sheet-direct-xmirror sheet)
  ;;   (with-slots (space-requirement) sheet
  ;;     '(setf (xlib:wm-normal-hints (sheet-direct-xmirror sheet))
  ;;           (xlib:make-wm-size-hints
  ;;            :width (round width)
  ;;            :height (round height)
  ;;            :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
  ;;            :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
  ;;            :min-width (round (space-requirement-min-width space-requirement))
  ;;            :min-height (round (space-requirement-min-height space-requirement))))))
)

;;;
;;;

(defmethod destroy-mirror :before ((port render-port-mixin) (sheet mezzano-mirrored-sheet-mixin))
  (declare (ignore port))
  ;; TODO destroy surface, fifo, window, frame
  ;; set mez-pixels mez-window mez-frame to NIL - do I need handle to surface?
  )

(defclass mezzano-pixmap (image-pixmap-mixin basic-pane)
  ())

;;;;</MIRRORED-SHEETS>
;;;;************************************************************************;;;;
;;;;<FRAME-MANAGER>
(in-package :clim-mezzano)

(defclass mezzano-frame-manager (frame-manager)
  ((mirroring-fn :initarg :mirroring
                 :initform (clim-mezzano::mirror-factory :single)
                 :reader mirroring-p)
   (class-gensym :initarg :class-gensym
                 :initform (gensym "MEZZANO-")
                 :reader class-gensym)))

;;; Default mirroring predicates
(defun mirror-factory (kind)
  (etypecase kind
    (null nil)
    (function kind)
    ((eql :single)
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (subtypep class 'top-level-sheet-pane))))
    ((eql :full)
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (subtypep class 'basic-pane))))
    ((eql :random) ;; for testing
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (or (subtypep class 'top-level-sheet-pane)
                  (zerop (random 2))))))))

;;; if the pane is a subclass of basic-pane and it is not mirrored we
;;; create a new class.

(defun maybe-mirroring (fm concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (funcall (mirroring-p fm) concrete-pane-class))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                           (class-name concrete-pane-class)
                                           concrete-pane-class)))
      (multiple-value-bind (class-symbol foundp)
          (alexandria:ensure-symbol
           (alexandria:symbolicate (class-gensym fm) "-"
                                   (symbol-name concrete-pane-class-symbol))
           :clim-mezzano)
	(unless foundp
          (let ((superclasses
                 (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
                     (list 'mezzano-mirrored-sheet-mixin
                           'climi::always-repaint-background-mixin
                           concrete-pane-class-symbol)
                     (list 'mezzano-mirrored-sheet-mixin
                           'climi::always-repaint-background-mixin
                           ;;'temporary-medium-sheet-output-mixin
                           'permanent-medium-sheet-output-mixin
                           concrete-pane-class-symbol))))
            (eval
             `(defclass ,class-symbol
                  ,superclasses
                ()
                (:metaclass
                 ,(type-of (find-class concrete-pane-class-symbol)))))))
        (setf concrete-pane-class (find-class class-symbol)))))
  concrete-pane-class)

(defun find-first-defined-class (types)
  (first
   (remove-if #'null
              (mapcar (lambda (class-name)
                        (find-class class-name nil))
                      types))))

(defun find-symbol-from-spec (package-spec name-components)
  (flet ((coerce-name-element (name-elt)
           (typecase name-elt
             (symbol (symbol-name name-elt))
             (sequence (coerce name-elt 'string))
             (t (princ-to-string name-elt)))))
  (find-symbol
   (apply #'concatenate 'string (mapcar #'coerce-name-element name-components))
   package-spec)))

(defun find-symbols (name-specs)
  (remove-if #'null
             (mapcar #'(lambda (x)
                         (find-symbol-from-spec (first x) (rest x)))
                     name-specs)))


(defun generate-standard-pane-specs (type)
  (let ((mapping (get type 'climi::concrete-pane-class-name)))
    `((,(symbol-package mapping) ,mapping)
      (:climi ,mapping)
      (:climi ,type #:-pane)
      (:climi ,type))))

(defun generate-mezzano-pane-specs (type)
  (append
   `((:clim-mezzano #:mezzano- ,type #:-pane)
     (:clim-mezzano #:mezzano- ,type)
     (:climi #:mezzano- ,type #:-pane)
     (:climi #:mezzano- ,type))
   (generate-standard-pane-specs type)))

(defun find-concrete-pane-class (type)
  (if (or (eql (symbol-package type)
               (find-package '#:clim))
          (eql (symbol-package type)
               (find-package '#:climi))
          (eql (symbol-package type)
               (find-package '#:keyword))
	  (get type 'climi::concrete-pane-class-name))
      (find-first-defined-class
       (find-symbols (generate-mezzano-pane-specs type)))
      type))

(defmethod make-pane-1 ((fm mezzano-frame-manager)
                        (frame application-frame) type &rest args)
  (apply #'make-instance
	 (maybe-mirroring fm (find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

(defmethod adopt-frame :before ((fm mezzano-frame-manager) (frame menu-frame))
  (multiple-value-bind (buttons mouse-x mouse-y)
      (mos:global-mouse-state)
    (declare (ignore buttons))
    (setf (slot-value frame 'climi::left) (+ mouse-x 5)
          (slot-value frame 'climi::top) (+ mouse-y 5))))

  ;; CLX code for adopt-frame :before
  ;; Temporary kludge.
  ;; (when (eq (slot-value frame 'climi::top) nil)
  ;;   (multiple-value-bind (x y)
  ;;       (xlib:query-pointer (clx-port-window (port fm)))
  ;;     (incf x 10)
  ;;     (setf (slot-value frame 'climi::left) x
  ;;           (slot-value frame 'climi::top) y)))


(defmethod adopt-frame :after ((fm mezzano-frame-manager) (frame menu-frame))
  ;; TODO not sure what to do here - maybe draw frame should be moved
  ;; here from create-mezzano-mirror? Then need additional cases:
  ;; application-frame
  ;; others?

  ;; (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
  ;;   (xlib:map-window (sheet-direct-xmirror (slot-value frame 'top-level-sheet))))
  )

;;;;</FRAME-MANAGER>
;;;;************************************************************************;;;;
;;;;<TEXT-SELECTION>
(in-package :clim-mezzano)

(defmethod clim-backend:bind-selection
    ((port mezzano-port) (sheet basic-sheet) &optional time)
  (declare (ignore time)))

(defmethod clim-backend:release-selection ((port mezzano-port) &optional time)
  (declare (ignore time)))

(defmethod request-selection :around ((port mezzano-port) requestor time)
  (declare (ignore requestor time)))

(defmethod clim-backend:selection-owner ((port mezzano-port))
  (climi::port-selection-owner port))

(defmethod (setf clim-backend:selection-owner) (owner (port mezzano-port))
  (setf (climi::port-selection-owner port) owner))

(defmethod clim-backend::selection-requester ((port mezzano-port))
  (climi::port-selection-requester port))

(defmethod (setf clim-backend::selection-requester)
    (requester (port mezzano-port))
  (setf (climi::port-selection-requester port) requester))

;;;;</TEXT-SELECTION>
;;;;************************************************************************;;;;


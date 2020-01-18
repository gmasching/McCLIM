(defpackage #:mezzano.supervisor
  (:use :cl))
(defpackage #:mezzano.gui
  (:use :cl))
(defpackage #:mezzano.gui.compositor
  (:use :cl))
(defpackage #:mezzano.gui.widgets
  (:use :cl))
(defpackage #:mezzano.gui.popup-io-stream
  (:use :cl))

;;;;************************************************************************;;;;
;;;;/gui/surface.lisp
(in-package :mezzano.gui)

(defstruct (surface
             (:constructor %make-surface))
  (pixels (error "Pixel data not specified."))
  (format (error "Format not specified.")))

(defmethod print-object ((object surface) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (surface-format object))))

(defun make-surface (width height &key (format :argb32) (initial-colour 0))
  "Create a new surface of the specified WIDTH, HEIGHT and FORMAT.
The surface will be filled with INITIAL-COLOUR, which defaults to fully transparent."
  (check-type format (member :argb32 :a8 :a1))
  (%make-surface :pixels (make-array (list height width)
                                     :element-type (ecase format
                                                     (:argb32 '(unsigned-byte 32))
                                                     (:a8 '(unsigned-byte 8))
                                                     (:a1 'bit))
                                     :initial-element initial-colour)
                 :format format))

;;;;....

(defun surface-width (surface)
  "Return the width of SURFACE in pixels."
  (array-dimension (surface-pixels surface) 1))

(defun surface-height (surface)
  "Return the height of SURFACE in pixels."
  (array-dimension (surface-pixels surface) 0))

(defun surface-pixel (surface x y)
  "Get the pixel in SURFACE at (X,Y)."
  (aref (surface-pixels surface) y x))

(defun (setf surface-pixel) (value surface x y)
  "Set the pixel in SURFACE at (X,Y)."
  (setf (aref (surface-pixels surface) y x) value))

;;;;************************************************************************;;;;
;;;;/gui/compositor.lisp
(in-package :mezzano.gui.compositor)
;;;;...
(defclass window ()
  ((%x :initarg :x :accessor window-x)
   (%y :initarg :y :accessor window-y)
   (%thread :initarg :thread :accessor window-thread)
   (%fifo :initarg :fifo :reader fifo)
   (%buffer :initarg :buffer :reader window-buffer)
   (%layer :initarg :layer :reader layer)
   (%subscribed-notifications :initarg :notifications :reader subscribed-notifications)
   (%unresponsive :initarg :unresponsive :accessor window-unresponsive)
   (%kind :initarg :kind :reader kind)
   (%cursor :initarg :cursor :accessor cursor)
   (%grabp :initarg :grabp :accessor grabp)
   (%grab-x1 :initarg :grab-x1 :accessor grab-x1)
   (%grab-y1 :initarg :grab-y1 :accessor grab-y1)
   (%grab-x2 :initarg :grab-x2 :accessor grab-x2)
   (%grab-y2 :initarg :grab-y2 :accessor grab-y2))
  (:default-initargs :layer nil
                     :notifications '()
                     :unresponsive nil
                     :thread nil
                     :cursor :default
                     :grabp nil))

(defclass event ()
  ((%window :initarg :window :reader window))
  (:default-initargs :window nil))

(defgeneric width (thing))
(defgeneric height (thing))

(defmethod width ((window window))
  (mezzano.gui:surface-width (window-buffer window)))

(defmethod height ((window window))
  (mezzano.gui:surface-height (window-buffer window)))

;;;;...

;;;; Keyboard events, including translation from HID scancode.

(defclass key-event (event)
  ((%scancode :initarg :scancode :reader key-scancode)
   (%releasep :initarg :releasep :reader key-releasep)
   (%key :initarg :key :reader key-key)
   (%modifier-state :initarg :modifier-state :reader key-modifier-state))
  (:default-initargs :scancode nil
                     :releasep nil
                     :key nil
                     :modifier-state '()))

;;;;...

;;;; Mouse events

(defclass mouse-event (event)
  ((%button-state :initarg :button-state :reader mouse-button-state)
   (%button-change :initarg :button-change :reader mouse-button-change)
   (%x-position :initarg :x-position :reader mouse-x-position)
   (%y-position :initarg :y-position :reader mouse-y-position)
   (%x-motion :initarg :x-motion :reader mouse-x-motion)
   (%y-motion :initarg :y-motion :reader mouse-y-motion))
  (:default-initargs :button-state nil
                     :button-change 0
                     :x-position nil
                     :y-position nil
                     :x-motion 0
                     :y-motion 0))

;;;;...

(defun make-window (fifo width height &key layer initial-z-order kind)
  (let ((window (make-instance 'window
                               :fifo fifo
                               :thread (mezzano.supervisor:current-thread)
                               :buffer (mezzano.gui:make-surface width height)
                               :layer layer
                               :kind kind)))
    (submit-compositor-event (make-instance 'window-create-event
                                            :window window
                                            :initial-z-order (or initial-z-order :top)))
    window))

;;;;...
;;;; Window close event.

(defclass window-close-event (event)
  ())
;;;;...
(defun close-window (window)
  (submit-compositor-event (make-instance 'window-close-event
                                          :window window)))

;;;;...
;;;; Window activation changed.

(defclass window-activation-event (event)
  ((%state :initarg :state :reader state)))

;;;;...
;;;; Window resizing.

(defclass resize-request-event (event)
  ((%origin :initarg :origin :reader resize-origin)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

(defclass resize-event (event)
  ((%origin :initarg :origin :reader resize-origin)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)
   (%new-fb :initarg :new-fb :reader resize-new-fb)))

;;;;....
;;;; Window move event.

(defclass move-event ()
  ((%window :initarg :window :reader window)
   (%x      :initarg :x      :reader new-x)
   (%y      :initarg :y      :reader new-y)))

(defmethod process-event ((event move-event))
  (let ((window (window event)))
    (setf (window-x window) (new-x event)
          (window-y window) (new-y event))))

(defun move-window (window new-x new-y)
  (submit-compositor-event (make-instance 'move-event
                                          :window window
                                          :x new-x
                                          :y new-y)))

;;;;....

;;;; Quit event, sent by the compositor when the user wants to close the window.

(defclass quit-event (event)
  ())

;;;;************************************************************************;;;;
;;;;/home/imac/Downloads/mezzano/gui/widgets.lisp
;;;;....
(in-package :mezzano.gui.widgets)

(defun default-damage-function (window)
  (lambda (&rest args)
    (apply #'mezzano.gui.compositor:damage-window window args)))

(defun default-cursor-function (window)
  (lambda (cursor)
    (mezzano.gui.compositor:set-window-data window :cursor cursor)))

;;;;...
(defclass frame ()
  ((%framebuffer :initarg :framebuffer :initform NIL :reader framebuffer)
   (%damage-function :initarg :damage-function :reader damage-function)
   (%set-cursor-function :initarg :set-cursor-function :reader set-cursor-function)
   (%title :initarg :title :accessor frame-title)
   (%title-width           :accessor title-width)
   (%title-origin          :accessor title-origin)
   (%title-vert            :accessor title-vert)
   (%close-button-p :initarg :close-button-p :accessor close-button-p)
   (%close-button-hover :initarg :close-button-hover :accessor close-button-hover)
   (%close-vert            :accessor close-vert)
   (%activep :initarg :activep :accessor activep)
   (%resizablep :initarg :resizablep :accessor resizablep)
   (%left   :initarg :left   :initform  1 :accessor left-border)
   (%right  :initarg :right  :initform  1 :accessor right-border)
   (%top    :initarg :top    :initform 19 :accessor top-border)
   (%bottom :initarg :bottom :initform  1 :accessor bottom-border)
   (%draw-corners-p                       :accessor draw-corners-p)
   (%draw-close-button-p                  :accessor draw-close-button-p)
   (%draw-title-p                         :accessor draw-title-p))
  (:default-initargs
    :title ""
    :close-button-p nil
    :close-button-hover nil
    :activep nil
    :resizablep nil))
;;;;************************************************************************;;;;
;;;;************************************************************************;;;;
;;;;************************************************************************;;;;

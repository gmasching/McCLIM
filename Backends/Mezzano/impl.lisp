;;;;************************************************************************;;;;
;;;;<PORTING SHIM>
;; (in-package :clim-mezzano)
;; (defun stub (&optional string)
;;   (error "Not implemented: ~s" string))
;; (defclass mos-window () ())
;; (defun mos:window-x ()
;;   (stub "window-x"))
;; (defun mos:window-y ()
;;   (stub "window-y"))

;; (defclass mos-event () ())
;; (defclass mos:key-event (mos-event) ())
;; (defclass mos:mouse-event (mos-event) ())
;; (defclass mos:window-activation-event (mos-event) ())
;; (defclass mos:quit-event (mos-event) ())
;; (defclass mos:window-close-event (mos-event) ())
;; (defclass mos:resize-request-event (mos-event) ())
;; (defclass mos:resize-event (mos-event) ())

;; (defmethod mos:key-releasep ((event mos:key-event))
;;   (stub "key-releasep")
;;   )
;; (defmethod mos:key-key ((event mos:key-event))
;;   (stub "key-key")
;;   )
;; (defmethod mos:key-modifier-state ((event mos:key-event))
;;   (stub "key-modifier-state")
;;   )
;; (defmethod mos:window ((event mos-event))
;;   ;;(or mos-event mos:mouse-event mos:key-event)
;;   (stub "window")
;;   )

;; (defmethod mos:mouse-x-position ((event mos:mouse-event))
;;   (stub "mouse-x-position")
;;   )
;; (defmethod mos:mouse-y-position ((event mos:mouse-event))
;;   (stub "mouse-y-position")
;;   )

;; (defmethod  mos:mouse-button-change ((event mos:mouse-event))
;;   (stub "mouse-button-change"))

;; (defclass mos:window () ())

;;(defmethod )
;;;;<PORTING SHIM>

;;;; Window content updates.
#+nil
(defclass damage-event (event)
  ((%x :initarg :x :reader x)
   (%y :initarg :y :reader y)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

;;;;...
#+nil
(defun damage-window (window x y width height)
  "Send a window damage event to the compositor."
  (submit-compositor-event (make-instance 'damage-event
                                          :window window
                                          :x x
                                          :y y
                                          :width width
                                          :height height)))
#+nil
(defun resize-window (window new-framebuffer &key (origin :top-left))
  (assert (eql (mezzano.gui:surface-format new-framebuffer) :argb32))
  (submit-compositor-event (make-instance 'resize-event
                                          :window window
                                          :origin origin
                                          :new-fb new-framebuffer)))

;;;;[FIXME::] bring in process-event code from mezzano

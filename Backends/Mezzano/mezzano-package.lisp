(defpackage #:mezzano.sync
  (:use :cl)
  (:export
   #:make-mailbox
   #:mailbox-send
   #:mailbox-receive))

(defpackage #:mezzano.supervisor
  (:use :cl)
  (:export
   #:make-thread ;
   #:current-thread ;[MCCLIM FIXME]
   #:debug-print-line ;

   #:panic ;

   ;;#:fifo
   ;;#:fifo-p
   #:make-fifo ;
   #:fifo-push ;
   #:fifo-pop ;
   ;;#:fifo-reset
   ;;#:fifo-size
   ;;#:fifo-element-type

   #:current-framebuffer
   #:framebuffer-blit ;[MCCLIM FIXME]
   #:framebuffer-width ;
   #:framebuffer-height ;
   ))
#+nil ;;[MCCLIM]
(defpackage #:mezzano.gui.font
  (:export
   #:GLYPH-MASK
   #:LINE-HEIGHT
   #:ASCENDER
   #:CHARACTER-TO-GLYPH
   #:GLYPH-ADVANCE
   #:GLYPH-XOFF
   #:GLYPH-YOFF
   #:LINE-HEIGHT)
  )

(defpackage #:mezzano.internals
  (:use :cl)
  (:export
   #:log-and-ignore-errors))

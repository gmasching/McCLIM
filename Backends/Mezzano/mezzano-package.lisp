(defpackage #:mezzano.sync
  (:export
   #:make-mailbox
   #:mailbox-send
   #:mailbox-receive))

(defpackage #:mezzano.supervisor
  (:export
   #:make-thread ;

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
   ;;#:framebuffer-blit
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
  (:export
   #:log-and-ignore-errors))

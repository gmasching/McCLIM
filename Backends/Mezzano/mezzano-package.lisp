(defpackage #:mezzano.sync
  (:export
   #:make-mailbox
   #:mailbox-send
   #:mailbox-receive))

(defpackage #:mezzano.supervisor
  (:export
   #:framebuffer-width
   #:framebuffer-height
   #:current-thread
   #:fifo-push
   #:framebuffer-blit
   #:current-framebuffer
   #:make-thread))

(defpackage #:mezzano.internals
  (:export
   #:log-and-ignore-errors))

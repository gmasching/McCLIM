
(defsystem #:mcclim-raster-image
  :depends-on (#:mcclim-render
               #:mcclim-backend-common
               #-mezzano #:mcclim-render/clx)
    :serial t
    :components
    ((:file "package")
     (:file "graft")
     (:file "medium")
     (:file "top-level-pane")
     (:file "port")
     (:file "stream")
     (:file "output-to-png")
     (:file "rgb-port")))

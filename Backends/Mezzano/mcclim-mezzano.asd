(asdf:defsystem #:mcclim-mezzano
    :depends-on (#:mcclim-backend-common
                 #:mcclim-render
		 ;;For a FIFO queue
		 #:lparallel
		 ;;For color multiplication
		 #:sb-cga
		 #:opticl)
    :components
    (
     (:file "mezzano-package")
     (:file "mezzano-shim")
     (:module
      "mezzano/gui"
      :serial t
      :components
      ((:file "package")
       (:file "blit-generic")
       (:file "colour")
       (:file "blit")
       (:file "surface")
       (:file "compositor")
       (:file "widgets")))
     (:file "package")
     (:file "mcclim-mezzano")))

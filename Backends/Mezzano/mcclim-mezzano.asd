(asdf:defsystem #:mcclim-mezzano
    :depends-on (#:mcclim-backend-common
                 #:mcclim-render)
    :components
    (
     ;;(:file "mezzano")
     (:file "mezzano-package")
     (:module
      "mezzano/gui"
      :serial t
      :components
      ((:file "package")
       (:file "colour")
       (:file "blit")
       (:file "blit-generic")
       (:file "surface")
       (:file "compositor")
       (:file "widgets")))
     (:file "package")
     (:file "impl")
     (:file "mcclim-mezzano")))

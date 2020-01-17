(asdf:defsystem #:mcclim-mezzano
    :depends-on (#:mcclim-backend-common
                 #:mcclim-render)
    :components
    ((:file "package")
     (:file "impl")
     (:file "mcclim-mezzano")))

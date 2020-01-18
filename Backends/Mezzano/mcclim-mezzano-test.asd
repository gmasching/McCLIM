(asdf:defsystem #:mcclim-mezzano-test
  :depends-on (#:mcclim-mezzano
	       #:clim-examples
	       #:application
	       #:image-utility
	       #:sucle-temp)
    :components
    ((:file "mcclim-mezzano-test")))

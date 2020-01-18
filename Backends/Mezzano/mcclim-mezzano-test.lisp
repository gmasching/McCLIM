(defpackage #:test5
  (:use :cl)
  (:export #:start #:change-image))
(in-package #:test5)

(defun start ()
  (application:main
   (lambda ()
     (gl:clear-color 0.5 0.5 0.5 0.0)
     (let ((shader
	    (glhelp:create-opengl-shader
	     ;;The vertex shader
	     "
out vec2 texcoord_out;
in vec3 position;
in vec2 texcoord;
void main () {
texcoord_out = texcoord;
gl_Position=vec4(position,1.0)*vec4(1.0,-1.0,1.0,1.0);
}"
	     ;;The fragment shader
	     "
in vec2 texcoord_out;
uniform sampler2D sampler;
void main () {
gl_FragColor = texture2D(sampler,texcoord_out.xy); 
}"
	     ;;Bind inputs to locations
	     '(("position" 0) 
	       ("texcoord" 2))
	     ;;Unifrom name in lisp, uniform name in the shader
	     '((:sampler "sampler")))))     
       (loop (application:poll-app)
	  (frame shader))))
   :width 512
   :height 512
   :title "5. Drawing an image"))

(defun change-image (new-data)
  (deflazy:refresh 'image nil)
  (setf *image-data* new-data)
  (values))
(defparameter *image-data* nil)
(deflazy:deflazy image ()
  (or *image-data*
      (img:load
       (sucle-temp:path #P"res/terrain.png"))))

(glhelp:deflazy-gl terrain (image)
  (glhelp:wrap-opengl-texture
   (glhelp:create-opengl-texture-from-data image)))

(defun frame (shader)
  (mezzano.gui.compositor:submit-mouse-absolute
   (floor window::*mouse-x*)
   (floor window::*mouse-y*)
   :buttons (logior 
	     (if (window:button :mouse :down :left)
		 #b1
		 0)
	     (if (window:button :mouse :down :right)
		 #b10
		 0)
	     (if (window:button :mouse :down :middle)
		 #b100
		 0)
	     ))
  (glhelp:use-gl-program shader)
  (let* ((image (deflazy:getfnc 'image))
	 (w (array-dimension image 1))
	 (h (array-dimension image 0)))
    (glhelp:set-render-area 0 0 w h))
  ;;Set uniforms within the shader
  (glhelp:with-uniforms uniform shader
    (glhelp:set-uniforms-to-textures
     ((uniform :sampler)
      (glhelp:handle (deflazy:getfnc 'terrain))))
    ;;Set the uniform referenced by :sampler in shader to the terrain texture
    )
  (gl:clear :color-buffer-bit
	    :depth-buffer-bit
	    :stencil-buffer-bit)
  (gl:disable :cull-face :depth-test)
  (let ((position-buffer (scratch-buffer:my-iterator))
	(tex-buffer (scratch-buffer:my-iterator)))
    ;;Draw square coordinates and texcoords to the
    ;;intermediate buffers named position-buffer and color-buffer
    (square position-buffer tex-buffer)
    (let
	;;This becomes set to a display-list or VAO depending
	;;on the OpenGL version [as well as possible the
	;;GPU vendor]
	(drawable)
      (let ((total-vertices
	     ;;Make sure that each buffer has the correct amount
	     ;;of data, and determine the total vertices.
	     (all-the-same
	      ;;The position-buffer is referenced 3 times per vertex.
	      (/ (scratch-buffer:iterator-fill-pointer position-buffer)
		 3)
	      ;;The tex-buffer is referenced 2 times per vertex.
	      (/ (scratch-buffer:iterator-fill-pointer tex-buffer)
		 2))))
	(scratch-buffer:flush-bind-in* ((position-buffer gl-vertex)
					(tex-buffer gl-texcoord))
	  (setf drawable
		;;This is a macro that generates code to
		;;write to either a display-list or VAO,
		;;depending on the OpenGL version [as well
		;;as possible the GPU vendor]
		(glhelp:create-vao-or-display-list-from-specs
		 (:quads total-vertices)
		 ;;Attribute locations
		 ((2 (gl-texcoord) (gl-texcoord))
		  (0 (gl-vertex) (gl-vertex) (gl-vertex)))))))
      (glhelp:slow-draw drawable)
      ;;For the purposes of the example, delete it immediately.
      ;;Usually we'll draw it many times.
      (glhelp:slow-delete drawable))))

(defun all-the-same (&rest values)
  (let ((only-one (remove-duplicates values)))
    (assert (equal (list-length only-one) 1))
    (car only-one)))

(defun square (position-buffer tex-buffer)
  ;;In test3 we used immediate-mode opengl, which means
  ;;vertices are sent to the CPU every time the triangle is
  ;;drawn.
  ;;Here, we send data to an intermediate buffer,
  ;;position-buffer and color-buffer.
  ;;Then we send position-buffer and color-buffer in
  ;;bulk to openGL, where it can be called many times as
  ;;a display-list or VAO
  (scratch-buffer:bind-out* ((position-buffer gl-vertex)
			     (tex-buffer gl-texcoord))
    (gl-texcoord 0.0 0.0)
    (gl-vertex -1.0 -1.0 0.5)
    (gl-texcoord 0.0 1.0)
    (gl-vertex -1.0 1.0 0.5)
    (gl-texcoord 1.0 1.0)
    (gl-vertex 1.0 1.0 0.5)
    (gl-texcoord 1.0 0.0)
    (gl-vertex 1.0 -1.0 0.5)))

(defpackage :repl
  (:use :cl)
  (:nicknames #:mmt)
  (:export
   #:start
   #:stop
   #:framebuffer
   #:load-example
   #:save))
(in-package :repl)

#+nil
(defun mez-event-fifo ()
  (clim-mezzano::mezzano-mez-fifo clim-mezzano::*port*))
#+nil
(defun mez-window ()
  (clim-mezzano::mezzano-port-window clim-mezzano::*port*))


(defparameter *compositor* nil)
(defparameter *quit* nil)
(defun start-compositor ()
  (stop-compositor)
  (setf *quit* nil)
  (when (not *compositor*)
    (setf *compositor* (mezzano.supervisor:make-thread
			(lambda ()
			  (loop :until *quit* :do (per-frame)))
			:name "Compositor"))))


(defun per-frame ()
  (mezzano.gui.compositor::compositor-thread-body)
  (test5:change-image (pixels))
  (sleep 0.3))

(defun stop-compositor ()
  (when (and
	 (bt:threadp *compositor*)
	 (bt:thread-alive-p *compositor*))
    (bt:destroy-thread *compositor*))
  (setf *compositor* nil))

(defun pixels ()
  (mezzano.supervisor::framebuffer-pixels
   (MEZZANO.SUPERVISOR:CURRENT-FRAMEBUFFER)))

(defun framebuffer ()
  (remove-duplicates
   (coerce
    (mezzano.gui::array-flatten
     (pixels))
    'list)))

(defun load-example ()
  #+quicklisp
  (ql:quickload "clim-examples"))

(defparameter *clim* nil)
(defparameter *quit-clim* nil)
(defun start-clim ()
  (stop-clim)
  (setf *quit-clim* nil)
  (when (not *clim*)
    (ql:quickload :clim-examples)
    (setf *clim* (mezzano.supervisor:make-thread
		  (lambda ()
		    (uiop:symbol-call
		     :clim-demo
		     :demodemo)
		    #+nil
		    (loop :until *quit-clim* :do ))
		  :name "Clim"))))

(defun stop-clim ()
  (when (and
	 (bt:threadp *clim*)
	 (bt:thread-alive-p *clim*))
    (bt:destroy-thread *clim*))
  (setf *clim* nil))

(defun save ()
  (let* ((output (asdf:system-relative-pathname :mcclim-mezzano "alive.png"))
	 (save-p (y-or-n-p "Save to: ~s" output)))
    (when save-p
      (opticl:write-png-file
       output
       (pixels)))))

(defun stop ()
  (stop-compositor)
  (stop-clim))

(defun start ()
  (start-compositor)
  (start-clim)
  
  (test5:start))

#+nil
(defparameter +button-to-clim-alist+
  `((,(byte 1 0) . ,+pointer-left-button+)
    (,(byte 1 1) . ,+pointer-right-button+)
    (,(byte 1 2) . ,+pointer-middle-button+)
    (,(byte 1 3) . ,+pointer-wheel-up+)
    (,(byte 1 4) . ,+pointer-wheel-down+)
    ;; (,(byte 1 ???) . ,+pointer-wheel-left+)
    ;; (,(byte 1 ???) . , +pointer-wheel-right+)
    ))

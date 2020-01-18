McCLIM Mezzano
----------------------------------------------------------

Requires the latest version of the sucle graphics library.

(ql:mcclim-mezzano-test)
(mmt:start)

Mezzano OS has a CL gui toolkit that supports MCCLIM, thanks to fittestbits.

This project seeks to rip out that code from mezzano, and repurpose it to an OpenGL + glfw3 [Or possibly sdl2] backend instead.
Why OpenGL + glfw3/sdl2?
- Portable: all major OSs
- Easy to install
- The same everywhere

/Mezzano is code ripped out directly from Mezzano, with some features commented out.

mezzano-package.lisp defines functions that would have been defined by Mezzano OS.
mezzano-shim.lisp implements these missing functions. For example, a Mezzano mailbox is replaced with an lparallel queue, and thread operations are replaced with bordeaux-threads.

[MCCLIM] means this code was modified.
[MCCLIM FIXME] means this code is not working.
[MCCLIM ... OPTIMIZATION] means the type declarations are removed, so it is slower.

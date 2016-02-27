:- use_package([assertions]).
:- comment(nodoc,assertions).

:- comment(title,"Connecting with the Web").

:- comment(module,"@cindex{WWW, interfacing with} This package
   implements the PiLLoW library @cite{pillow-ws-dist} in Ciao.  It is
   not yet documented here, but you can access
   @uref{http://clip.dia.fi.upm.es/Software/pillow/pillow.html} to see
   documentation on previous versions. However, note that this version
   is newer and has some bug fixes and added functionality (templates,
   XML processing, and others, which you can try to guess the rest by
   looking at the source).").

:- include(library(pillow)).

%% CIAO syntax
:- use_package(assertions).  
:- comment(nodoc,assertions).  

:- comment(title,"Find out which architecture we are running on").

:- comment(author,"Manuel Carro").
:- comment(author,"Robert Manchek").

:- comment(module,"The architecure and operating system the engine is
compiled for determines whether we can use or not certain libraries.
This script, taken from a PVM distribution, uses a heuristic (which
may need to be tuned from time to time) to find out the platform.  It
returns a string which is used throughout the engine (in #ifdefs) to
enable/disable certain characteristics.

   @section{Usage (ciao_get_arch)}

   @begin{verbatim}
   Usage: ciao_get_arch
   @end{verbatim}

   @section{More details}

   Look at the script itself...

").


main.

:- comment(version(0*0+1,2000/05/08,20:27*07+'CEST'), "Solaris 5.X, X
> 6, on Sparc returns SolarisSparc5.7; there is a separate entry in
the system-dependent makefiles for it.  This is due to a system call
moved to a separate library. (MCL)").

:- comment(version(0*0+0,1998/08/12,19:53*41+'MET DST'), "Working.
   (MCL)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:



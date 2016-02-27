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



:- comment(version(1*10+8,2007/01/28,17:30*03+'CEST'),
   "Options to detect and compile in 64-bit Intel architecture.").

:- comment(version(0*0+6,2001/03/26,13:56*52+'CEST'), "Added FreeBSD,
thanks to Paul Broome.  (MCL)").

:- comment(version(0*0+5,2001/03/26,13:55*47+'CEST'), " 5 Jan 2001 MCL
--- Added LINUXSparc64.").

:- comment(version(0*0+4,2001/03/26,13:55*27+'CEST'), " 7 Dec 2000
JFMC --- Added DARWINppc").

:- comment(version(0*0+3,2001/03/26,13:55*16+'CEST'), " 19 Dec 1998
MCL --- More OS and architecture specified: Solaris on i86, NT on i86,
NT on alpha, Linux on i86, Linux on sparc, Pentium 686 (not all of
them tested).  (MCL)").

:- comment(version(0*0+2,2001/03/26,13:54*43+'CEST'), " MCL -- added
uname option at the beginning to recognize NT under Cygnus-Windows32
set of utilities.  Remember to copy uname to /bin/uname (as well as
/bin/sh, which is needed for scripts).  (MCL)").

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



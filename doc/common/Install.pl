

:- use_package(assertions).

:- comment(title,"Installing Ciao from the source distribution").

:- comment(author,"Manuel Carro").
:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"@include{INSTALL.lpdoc}"). 

main.

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+104,2000/04/03,10:41*02+'CEST'), "Several permission
   bugs fixed in installation scripts.  (Manuel Carlos Rodriguez)").

:- comment(version(1*5+101,2000/03/31,12:31*30+'CEST'), "Added force
   target to readmes dir and installation so that regeneration is
   forced previous to making a distribution (temporary due to bug in
   lpdoc makefile dependencies, which is fixed in 2.0).  (Manuel
   Hermenegildo)").

:- comment(version(1*5+84,2000/03/23,19:26*08+'CET'), "Several
   improvements to the documentation.  (Manuel Hermenegildo)").

:- comment(version(1*5+79,2000/03/23,11:46*02+'CET'), "Several mods to
   reduce size of distribution (eliminated some uneeded files).
   (Manuel Hermenegildo)").

:- comment(version(1*5+78,2000/03/23,10:53*30+'CET'), "Fixed minor
   installation problems (distclean, clpqr compilation).  (Manuel
   Hermenegildo)").

:- comment(version(1*5+77,2000/03/22,18:43*46+'CET'), "Latest version
   of clpr/clpq now in distribution.  (Manuel Hermenegildo)").

:- comment(version(1*5+71,2000/03/17,20:02*19+'CET'), "@apl{unix2dos}
   now not needed for installation.  (Manuel Hermenegildo)").

:- comment(version(1*5+70,2000/03/17,16:26*37+'CET'), "Fixed serious
   bug in Library makefiles: a second uninstall deleted the source
   library dir! (Manuel Hermenegildo)").

:- comment(version(1*5+69,2000/03/17,16:25*55+'CET'), "Fixed serious
   bug in makefiles: a second uninstall deleted the source lib dir!
   (Manuel Hermenegildo)").

:- comment(version(1*5+68,2000/03/17,14:47*31+'CET'), "Changed all
   @file{.nodistribute} to @file{NODISTRIBUTE} (also in
   @tt{Makefile}s).  (Manuel Hermenegildo)").

:- comment(version(1*5+67,2000/03/17,14:46*42+'CET'), "A file
   @file{NOCOMPILE} now also prevents compilation of a library during
   installation (in addition to @file{.nocompile}).  (Manuel
   Hermenegildo)").

:- comment(version(1*3+117,1999/11/25,01:44*07+'MET'), "Fixed some
   minor installation quirks. Checked out things again with Win95.
   (Manuel Hermenegildo)").

:- comment(version(1*3+113,1999/11/23,17:27*10+'MET'), "Fixed
   installation problem in Windows: installing when the source is in a
   path that contains a space now works correctly.  (Manuel
   Hermenegildo)").

:- comment(version(1*3+109,1999/11/22,10:44*43+'MET'), "Fixed a minor
   problem in the etc Makefile and a wrong reference in documentation.
   (Manuel Hermenegildo)").

:- comment(version(1*3+101,1999/11/13,18:07*58+'MET'), "More changes
   to Windows installation. Added @tt{hw.pls} example.  (Manuel
   Hermenegildo)").

:- comment(version(1*3+98,1999/11/11,19:07*10+'MET'), "Cleaned up
   installation of etc directory.  (Manuel Hermenegildo)").

:- comment(version(1*3+97,1999/11/11,12:28*17+'MET'), "Manuals now
   installed in separate Ciao group in info.  (Manuel Hermenegildo)").

:- comment(version(1*3+92,1999/11/07,01:08*10+'MET'), "Some more
   changes to installation scripts and instructions (specially under
   Win32).  (Manuel Hermenegildo)").

:- comment(version(1*3+89,1999/10/22,17:17*03+'MEST'), "Minor changes
   to installation scripts and instructions.  (Manuel Hermenegildo)").

:- comment(version(1*3+87,1999/10/21,19:54*14+'MEST'), "Fixed bug in
   DOTcshrc and DOTprofile which made Unix man pages not be found when
   @tt{PATH} was undefined before installation (thanks to
   @index{Roberto Bagnara} for the fix). (Manuel Hermenegildo)").


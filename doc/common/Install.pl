

:- use_package(assertions).

:- comment(title,"Installing Ciao").

:- comment(author,"Manuel Carro").
:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"@include{INSTALL.lpdoc}"). 

main.

:- comment(version_maintenance,dir('../../version')).

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


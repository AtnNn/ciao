
:- use_package([assertions,metaprops]).
:- comment(nodoc,assertions).
% does not work:
%:- comment(nodoc,metaprops).
:- comment(hide,callme/2).

:- comment(title,"ISO-Prolog modes").
 
:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").
 
:- comment(module,"This file defines the ``@concept{modes}''
   used in the documentation of the ISO-Prolog standard.
   See also @ref{Classical Prolog modes} for an alternative set of
   modes.").

:- comment(usage,"@tt{:- use_package([assertions,isomodes]).}").

:- include(library(isomodes)).

:- comment('?'/1,"Unspecified argument.").
:- comment('*'/1,"Unspecified argument.").

% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*3+112,1999/11/23,01:08*50+'MET'), "Adapted to
   new, simpler doc possible with lpdoc-1.9p22.  (Manuel
   Hermenegildo)").

:- comment(version(0*9+42,1999/04/08,02:14*11+'MEST'), "Separated
   documentation from include file. (Manuel Hermenegildo)").
% ----------------------------------------------------------------------------



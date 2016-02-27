
:- use_package(assertions).

:- comment(filetype, part).

:- comment(title,"PART X - Miscellaneous standalone utilities").

:- comment(subtitle,"@bf{The CIAO System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP X/98.1").
:- comment(subtitle,"@em{Draft printed on:} @today{}").

:- include(library('ClipAddress')).
:- include(library('Copyright')).

summary_text("This is the documentation for a set of
   miscellaneous standalone utilities contained in the @tt{etc}
   directory of the CIAO distribution.").

:- comment(summary,"@includefact{summary_text/1}").

:- comment(module,"@includefact{summary_text/1}").

:- comment(version(1*3+119,1999/11/26,11:48*19+'MET'), "Solved a
problem with the Makefile not working in Solaris with sh not having a
'-nt' option for the 'test' command.  (MCL)").

:- comment(version(0*5+0,1998/1/15), "Started automatic
   documentation. (Manuel Hermenegildo)").

%% This is a dummy definition of main to force documenter to produce 
%% application-type documentation (rather than library-type).

main.

:- comment(version_maintenance,dir('../version')).


:- use_package([assertions,metaprops]).
:- comment(nodoc,assertions).
:- comment(nodoc,metaprops).

:- comment(filetype,package).

:- comment(title,"Run-time checking of assertions").

:- comment(author, "German Puebla").

:- comment(module,"This library package allows the use of run-time
	checks for the assertions introduced in a program.

        The recommended way of performing @index{run-time checks} of
	predicate assertions in a program is via the Ciao preprocessor
	(see @tt{ciaopp} manual), which performs the required program
	transformation. However, this package can also be used to
	perform checking of program-point assertions.
").

:- comment(bug,"All the code in this package is included in the user
	program when it is used, ant there is a lot of it! A module
        should be used instead.").
:- comment(bug,"@tt{check/1} uses lists instead of ""proper"" properties.").

:- include(library(rtchecks)).

:- comment(hide,callme/1). 
:- comment(hide,callme/2). 
:- comment(hide,proves/2). 
:- comment(hide,disproves/2). 
:- comment(hide,calls/2). 
:- comment(hide,c_prec/3). 
:- comment(hide,prec/3). 
:- comment(hide,possibly_entailed/2). 
:- comment(hide,postc/2). 
:- comment(hide,disentailed/3). 
:- comment(hide,is_disentailed/1). 
:- comment(hide,entailed/3). 
:- comment(hide,is_entailed/1). 
:- comment(hide,check/2). 
:- comment(hide,perform/2). 
:- comment(hide,system_dependent_compatible/1). 
:- comment(hide,system_dependent_incompatible/1). 
:- comment(hide,system_dependent_entailed/1). 
:- comment(hide,system_dependent_disentailed/1). 
:- comment(hide,show_prop/1). 
:- comment(hide,show_prog_point/1). 
:- comment(hide,show_prop_and_prog_point/2).

:- comment(doinclude,check/1).
:- comment(check(Property),
    "Checks whether the property defined by @var{Property} holds.
     Otherwise, a warning message is issued.
     It corresponds to a program-point @concept{check assertion}
     (see @ref{The Ciao assertion package}).").
:- pred check(Property) : regtype((^((list;list))|list)).

/*
:- prop check(Property) : regtype(Property,
             (Property :- (Property=(X;Y),list(X),list(Y) ; list(Property)) )
				 ).
*/

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*11+83,2003/12/20,14:47*06+'CET'), "First
   revision.  (Edison Mera)").


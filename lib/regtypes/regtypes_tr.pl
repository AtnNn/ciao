:- module(regtypes_tr,
	[ expand_regtypes/2 ],
	[ assertions, isomodes ]).

:- comment(title,"Regular type definition support").

%% ------------------------------------------------------------------------

%% If a '+' field is present by not recognizing it here as is_type_decl 
%% we simply leave it as is! (old?)
expand_regtypes( (:- regtype((T # C)))   , (:- prop((T + regtype # C)))  ). 
expand_regtypes( (:- regtype(S,(T # C))) , (:- prop(S,(T + regtype # C)))). 
expand_regtypes( (:- regtype(T))         , (:- prop(T + regtype))        ).
expand_regtypes( (:- regtype(S,T))       , (:- prop(S,T + regtype))      ).

%% ------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*3+30,1999/07/12,18:50*23+'MEST'), "Eliminated
   definitely references to @tt{type/1} and @tt{type/2}.  (Manuel
   Hermenegildo)").

:- comment(version(0*8+39,1999/03/01,19:46*44+'MET'), "Documented
   usage.  (Manuel Hermenegildo)").

:- comment(version(0*8+14,1998/12/02,18:22*05+'MET'), "Ported to ciao
   version 0.8.  (Manuel Hermenegildo)").

:- comment(version(0*6+10,1998/08/03,13:52*14+'MET DST'), "Removed bug
   in previous comment starting with two double quotes.  (MCL)").

:- comment(version(0*6+9,1998/07/31,17:50*52+'MET DST'), "Now
   supporting # also as comment separator.  (Manuel Hermenegildo)").

:- comment(version(0*5+26,1998/06/25,20:10*07+'MET DST'), "Changed
   error reporting format to make it similar to preprocessor error
   messages.  (Manuel Hermenegildo)").

:- comment(version(0*5+5,1998/2/7), "Taken out syntax(modes), and put
   in check status for typedefs (is_typedef/4). (Francisco Bueno
   Carrillo)").

:- comment(version(0*5+4,1998/2/4), "Limited checking to the bare
   minimum to limit read-time overhead.  (Manuel Hermenegildo)").

:- comment(version(0*5+3,1998/2/4), "Made all definitions (except
   typeslib) local to avoid dependency cycles (so that this syntax can
   be used almost anywhere). (Manuel Hermenegildo)").

:- comment(version(0*5+2,1998/2/3), "Merged type and typedef
   declaration processing into a sigle ``types'' library. (Manuel
   Hermenegildo)").

:- comment(version(0*5+1,1998/2/3), "Added type checking. (Manuel
   Hermenegildo)").

:- comment(version(0*5+0,1998/1/28), "Started on-line documentation. (Manuel
   Hermenegildo)").

%% ------------------------------------------------------------------------



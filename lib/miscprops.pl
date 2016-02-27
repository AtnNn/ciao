:- module(miscprops,[
%	               cost/2, %% Should be replaced by lower/upper/etc.
% the corresponding lower/upper/etc. are now in assertions(native_props)
		       directoryname/1, %% Not a regular type!
%		       filename/1, %% Not a regular type!
% it is now in assertions(c_itf_props)
		       formatstring/1, %% Not a regular type!
		       moduletype/1,
%		       no_fail/1,
% replaced by not_fails/1 in assertions(native_props)
		       structofstrings/1 %% Not a regular type!
		     ],
                     [
                       assertions,regtypes
                     ]).

:- use_module(engine(hiord_rt), [call/1]).

%% Please keep in alphabetical order!

%% These definitions should be
%%  - in builtins if it is something that is always loaded 
%%  - in the corresponding module for properties that  
%%    correspond to that module (e.g., the type "stream" should be 
%%    defined in streams.pl, etc.)

%% --------------------------------------------------------------------------

:- comment(title,"Some basic properties").

:- comment(module,"@cindex{properties, basic} This library contains a
   basic set of property definitions. They are intended for use both
   as testing builtins within programs and as properties in
   assertions.  ").


:- comment(bug,"The solution of putting all these predicates here is
   temporary. Most of these should be in the files where the
   corresponding builtins are defined in the system (e.g.,
   builtins.pl, etc.). This file is useful while we finish adding
   declarations to the system library files, and also for running
   under other Prolog systems.").

%% --------------------------------------------------------------------------


:- prop cost(G,Exp) 
   # "The number of execution steps is given by @var{Exp}".

cost(_,_).

:- regtype directoryname(X) 
   # "@var{X} is an atom describing the name of a directory.".

directoryname(X) :- 
	atm(X).

:- regtype formatstring(String) 
   # "@var{String} is a character string with formatting characters
      (as needed by @pred{format/2}).".

%% Not quite right...
formatstring(X) :- 
	string(X).

:- regtype moduletype(X) # "@var{X} is a module type (@tt{module} or 
   @tt{include} file).". 

moduletype(use_module).
moduletype(include).

:- prop no_fail(G) # "The predicate does not fail.".

no_fail(G) :- \+ \+ G.

:- prop structofstrings(S) 
   # "@var{S} is a structure of strings.".

structofstrings(A) :-
	A =.. [_F|Args],
	allstrings(Args).

allstrings([H]) :-
	string(H).
allstrings([H|T]) :-
	string(H),
	allstrings(T).

%% --------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(0*8+17,1998/12/03,10:33*44+'MET'), "Changed types
   to regtypes. Updated somewhat. Still, this library has to be
   eventually deleted...  (Manuel Hermenegildo)").

:- comment(version(0*5+4,1998/4/1), "Moved all types to basictypes.pl.
   (Francisco Bueno Carrillo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

:- comment(version(0*3+6,1997/12/16), "Ported to native ciao. (Manuel
   Hermenegildo)").

%% Left it in lists (otherwise there was a collision)
%% :- comment(version(0*3+5,1997/11/25), "Imported list/1 from
%%    library(lists) and re-exported (Francisco Bueno Carrillo)").

:- comment(version(0*3+4,1997/8/27), "Included type term/1 (Daniel
   Cabeza Gras)").

:- comment(version(0*3+3,1997/07/29), "Added unary '=' (quote in
   types).").
%% --------------------------------------------------------------------------


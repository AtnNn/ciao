
:- module(atom2term,
	[ atom2term/2,
	  string2term/2,
          parse_term/3
	],
	[ assertions,
	  basicmodes
	]).

:- comment(title,"Atom to term conversion").
:- comment(author,"Francisco Bueno").
:- comment(author,"Daniel Cabeza").
:- comment(bug,"This is just a quick hack written mainly for parsing 
   daVinci's messages. There should be a call to the standard reader
   to do this!").

:- pred atom2term(+Atom,-Term) # "Convert an atom into a term.
   @var{Atom} is an atom, but must have term syntax.  @var{Term} is a
   term resulting from parsing @var{Atom} char by char.".

atom2term(Atom,Term):-
	name(Atom,String),
	parse_term(String,Term, _).

:- pred string2term(+String,-Term) # "Same as @pred{atom2term/2} but
   first argument is a string (containing a term).".

string2term(String,Term):-
	parse_term(String,Term, _).

:- pred parse_term(+String, -Term, ?Dummy)
      # "@var{String} is parsed into @var{Term} upto @var{Dummy}
         (which is the non-parsed rest of the list).".

parse_term([],'',[]).
parse_term([C|String0],Term,String):-
	parse_term0(C,String0,Term,String).
	
parse_term0(0'\s,String0,Term,String):- % space
        parse_term(String0,Term,String).
parse_term0(0'[,String0,Term,String):- !,
	parse_args0(String0,Term,[0']|String]).
parse_term0(0'",String0,Term,String):- !,
	parse_string(String0,Str,String),
	parse_term(Str,Term,[]).
parse_term0(0'( , _, _, _):- !, fail.
parse_term0(0') , _, _, _):- !, fail.
parse_term0(0'] , _, _, _):- !, fail.
parse_term0(0', , _, _, _):- !, fail.
parse_term0(C0,String0,Term,String):-
        parse_functor(C0,String0,FunctorStr,String1),
        name(Functor,FunctorStr),
	parse_args(String1,Args,String),
	Term=..[Functor|Args].

parse_functor(0'\s,String,[],[0'\s|String]).
parse_functor(0'( ,String,[],[0'( |String]).
parse_functor(0') ,String,[],[0') |String]).
parse_functor(0'[ ,String,[],[0'[ |String]).
parse_functor(0'] ,String,[],[0'] |String]).
parse_functor(0', ,String,[],[0', |String]).
parse_functor(C, String,[C|Functor],String1):-
        parse_functor_(String,Functor,String1).

parse_functor_([], [], []).
parse_functor_([C|String],Functor,String1):-
	parse_functor(C,String,Functor,String1).

%parse_args([],[],[]).
parse_args([0'(|String0],Args,String):- !,
	parse_args0(String0,Args,[0')|String]).
parse_args(String,[],String).

parse_args0(String0,[Arg|Args],String):-
	parse_term(String0,Arg,String1), !,
	parse_args1(String1,Args,String).
parse_args0(String, [], String).

parse_args1([0'\s|String0],Args,String) :- !,
        parse_args1(String0,Args,String).
parse_args1([0', |String0],[Arg|Args],String):- !,
	parse_term(String0,Arg,String1),
	parse_args1(String1,Args,String).
parse_args1(String,[],String).

parse_string([],[],[]).
parse_string([C|String],List,String1):-
	parse_string0(C,String,List,String1).

parse_string0(0'",String,[],String):- !.
parse_string0(C,String,[C|List],String1):-
	parse_string(String,List,String1).


% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(0*9+78,1999/05/03,19:19*27+'MEST'), "Fixed a bug in
   parse_term/3 which failed on alone atoms. (Daniel Cabeza Gras)").

:- comment(version(0*9+41,1999/04/07,22:25*54+'MEST'), "Changed
   atom2term to be more efficient and cover more cases.  (Daniel Cabeza
   Gras)").

:- comment(version(0*9+39,1999/04/07,09:20*47+'MEST'), "Added some
   comments - but this should go eventually.  (Manuel Hermenegildo)").
% ----------------------------------------------------------------------------

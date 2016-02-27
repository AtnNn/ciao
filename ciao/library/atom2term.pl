:- module(atom2term,
	[ atom2term/2,
	  string2term/2,
          parse_term/3,
          parse_term/4
	],
	[ assertions,
	  basicmodes
	]).

:- doc(title,"Atom to term conversion").

:- doc(author,"Francisco Bueno").
:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(module, "This module implements predicates for atom or
   string to term conversion.").

:- doc(bug,"This is just a quick hack written mainly for parsing 
   daVinci's messages. There should be a call to the standard reader
   to do this!").

:- pred atom2term(+Atom,-Term) # "Convert an atom into a term.
   @var{Atom} is an atom, but must have term syntax.  @var{Term} is a
   term resulting from parsing @var{Atom} char by char. The term is
   assumed to be ground.".

:- test atom2term(A,T) : ( A = a ) => ( T = a ).
:- test atom2term(A,T) : ( A = '1' ) => ( T = 1 ).
:- test atom2term(A,T) : ( A = 'A' ) => ( T = 'A' ).
:- test atom2term(A,T) : ( A = 'f(a)' ) => ( T = f(a) ).
:- test atom2term(A,T) : ( A = 'f/2' ) => ( T = f/2 ).

atom2term(Atom,Term):-
	atom_codes(Atom,String),
	parse_term(String,Term, _, novars), !.

:- pred string2term(+String,-Term) # "Same as @pred{atom2term/2} but
   first argument is a string (containing a term).".

:- test string2term(A,T) : ( A = "a" ) => ( T = a ).
:- test string2term(A,T) : ( A = "1" ) => ( T = 1 ).
:- test string2term(A,T) : ( A = "A" ) => ( T = 'A' ).
:- test string2term(A,T) : ( A = "f(a)" ) => ( T = f(a) ).
:- test string2term(A,T) : ( A = "f/2" ) => ( T = f/2 ).

string2term(String,Term):-
	parse_term(String,Term, _, novars), !.

:- pred parse_term(+String, -Term, ?Rest)
      # "@var{String} is parsed into @var{Term} up to @var{Rest}
         (which is the non-parsed rest of the list). The term is
         assumed to be ground.".

:- test parse_term(A,T,R) : ( A = "f(a)" ) => ( T = f(a), R = "" ).
:- test parse_term(A,T,R) : ( A = "f(a) foo " ) => ( T = f(a), R = " foo " ).

parse_term(String, Term, Rest) :- 
	parse_term(String, Term, Rest, novars).

:- pred parse_term(+String, -Term, ?Rest, +Vars)
      # "@var{String} is parsed into @var{Term} up to @var{Rest}
         (which is the non-parsed rest of the list). The term is
         assumed to be ground. If @var{Vars} is @tt{vars} then upper
         case identifiers in the term are turned into variables.".

:- test parse_term(A,T,R,V) : ( A = "f(X)", V = nonvars ) => ( T = f('X'), R = "" ).
:- test parse_term(A,T,R,V) : ( A = "f(X)", V = vars ) => ( T = f("X"), R = "" ).
:- test parse_term(A,T,R,V) : ( A = "f(X) foo ", V = vars ) => ( T = f("X"), R = " foo " ).

parse_term([],'',[], _).
parse_term([C|String0],Term,String,Vars):-
	parse_term0(C,String0,Term,String,Vars).
	
parse_term0(0'\s,String0,Term,String,Vars):- !, % space
        parse_term(String0,Term,String,Vars).
parse_term0(0'\t,String0,Term,String,Vars):- !, % tab
        parse_term(String0,Term,String,Vars).
parse_term0(0'\n,String0,Term,String,Vars):- !, % newline
        parse_term(String0,Term,String,Vars).
parse_term0(0'[,String0,Term,String,Vars):- !, % list
	parse_args0(String0,Term,[0']|String],Vars).
parse_term0(0'",String0,Term,String,_):- !, % string
	parse_string(String0,Str,String),
	atom_codes(Term,Str).
parse_term0(0'( , _, _, _, _):- !, fail.
parse_term0(0') , _, _, _, _):- !, fail.
parse_term0(0'] , _, _, _, _):- !, fail.
parse_term0(0', , _, _, _, _):- !, fail.
parse_term0(C0,String0,Term,String,Vars):- % struct
        parse_functor(C0,String0,FunctorStr,String1),
	(  FunctorStr = [U|_], Vars = vars, is_upper_case(U)
           % It is a variable
	-> % leave as a variable 
	   Term = FunctorStr,
           String = String1
	;  % atom_codes(Functor,FunctorStr),
           % We use name instead because by default we 
           % want '1' to be converted to a number.
	   name(Functor,FunctorStr),
           (  String1 = [0'/ |NumS] 
           -> parse_term(NumS,Arity,String,Vars), 
              % Conversion not needed because atom used above
	      % atom_codes(Arity,T),
	      % number_codes(Num,T),
	      % Term='/'(Functor,Num) 
	      Term='/'(Functor,Arity) 
           ;  parse_args(String1,Args,String,Vars),
              Term=..[Functor|Args] )).

is_upper_case(Ch) :-
	Ch >= 65, Ch =< 90.

parse_functor(0'\s,String,[],[0'\s|String]) :- !.
parse_functor(0'\t,String,[],[0'\t|String]) :- !.
parse_functor(0'\n,String,[],[0'\n|String]) :- !.
parse_functor(0'( ,String,[],[0'( |String]) :- !.
parse_functor(0') ,String,[],[0') |String]) :- !.
parse_functor(0'[ ,String,[],[0'[ |String]) :- !.
parse_functor(0'] ,String,[],[0'] |String]) :- !.
parse_functor(0'/ ,String,[],[0'/ |String]) :- !.
parse_functor(0', ,String,[],[0', |String]) :- !.
parse_functor(C, String,[C|Functor],String1):-
        parse_functor_(String,Functor,String1).

parse_functor_([], [], []).
parse_functor_([C|String],Functor,String1):-
	parse_functor(C,String,Functor,String1).

%parse_args([],[],[]).
parse_args([0'(|String0],Args,String,Vars):- !,
	parse_args0(String0,Args,[0')|String],Vars).
parse_args(String,[],String,_).

parse_args0(String0,[Arg|Args],String,Vars):-
	parse_term(String0,Arg,String1,Vars), !,
	parse_args1(String1,Args,String,Vars).
parse_args0(String, [], String,_).

parse_args1([0'\s|String0],Args,String,Vars) :- !,
        parse_args1(String0,Args,String,Vars).
parse_args1([0', |String0],[Arg|Args],String,Vars):- !,
	parse_term(String0,Arg,String1,Vars),
	parse_args1(String1,Args,String,Vars).
parse_args1(String,[],String,_).

parse_string([],[],[]).
parse_string([C|String],List,String1):-
	parse_string0(C,String,List,String1).

parse_string0(0'",String,[],String):- !.
parse_string0(C,String,[C|List],String1):-
	parse_string(String,List,String1).


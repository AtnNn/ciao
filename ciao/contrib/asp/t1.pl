:- module(t1,[s2t/2]).

:- use_module(library(strings)).

s2t(String,Term) :- parse_term(String,Term,_).

parse_term([],'',[]).
parse_term([C|String0],Term,String) :-
	display('parse_term:'), XX=[C|String0],
	display('  [C|String0]='), write_string(XX), nl,
	parse_term0(C,String0,Term1,String1),
	display('  parse term:after parse_term0='), write_string(String1), nl,
	parse_term(String1,Term2,String),
	display('  parse term:end:'), write_string(String),
	display(' Term2='), display(Term2), nl,
	(Term2='' -> Term=Term1;Term=(Term1,Term2)),
	display('  parse_term:finally:'),display(Term),nl.
parse_term([_|String0],	Term,String) :-
	parse_term(String0, Term, String).

parse_term0(0'\s,String0,Term,String):- % space
        parse_term(String0,Term,String).
parse_term0(0'[,String0,Term,String):- !,
	parse_args0(String0,Term,[0']|String]).
parse_term0(0'",String0,Term,String):- !,
	parse_string(String0,Str,String),
	atom_codes(Term,Str).
parse_term0(0'( , _, _, _):- !, fail.
parse_term0(0') , _, _, _):- !, fail.
parse_term0(0'] , _, _, _):- !, fail.
parse_term0(0', , _, _, _):- !, fail.
parse_term0(C0,String0,Term,String):-
	display('  PARSE_TERM0:'),nl,
        parse_functor(C0,String0,FunctorStr,String1),
	display('    FunctorStr='), write_string(FunctorStr),
	display(' String1='), write_string(String1), nl,
        name(Functor,FunctorStr),
	parse_args(String1,Args,String),
	display('    Args='), display(Args),
	display(' String='), write_string(String), nl,
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


parse_args([0'(|String0],Args,String):- !,
	display('      parse_args:'),
	display('String0='), write_string(String0), nl,
	parse_args0(String0,Args,[0')|String]),
	display('      Args='), display(Args), nl,
	display('      String='), write_string(String), nl.
parse_args([0'\s|String0],Args,String) :-
	parse_one_arg(String0,Args,String),!.
parse_args([0') |String],[],String) :- !.
parse_args(String,[],String).

parse_one_arg([0'\s|String0],Args,String1) :-
	parse_one_arg(String0, Args, String1),!.
parse_one_arg([0',|_],_,_) :- !,fail.
parse_one_arg([],_,_) :- !,fail.
parse_one_arg(String0,[Args],String) :-
	parse_term(String0,Args,String).

parse_args0(String0,[Arg|Args],String):-
	parse_term(String0,Arg,String1), !,
	display('parse_arg0: call parse_arg1 String0='), 
	write_string(String0), nl, display(' String1='),
	write_string(String1), nl,read(_),
	parse_args1(String1,Args,String).
parse_args0(String, [], String).

parse_args1([0'\s|String0],Args,String) :- !,
        parse_args1(String0,Args,String).
parse_args1([0', |String0],[Arg|Args],String):- !,
	parse_term(String0,Arg,String1),
	parse_args1(String1,Args,String).
%parse_args1([0') |String0],[],String0):- !.
parse_args1(String,[],String).

parse_string([],[],[]).
parse_string([C|String],List,String1):-
	parse_string0(C,String,List,String1).

parse_string0(0'",String,[],String):- !.
parse_string0(C,String,[C|List],String1):-
	parse_string(String,List,String1).

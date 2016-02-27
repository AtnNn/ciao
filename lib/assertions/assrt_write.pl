:- module(assrt_write,
	[ write_assertion/6,
	  write_assertion_as_comment/6
	],
	[ assertions, regtypes
	]).

:- comment(title,"Pretty-printing assertions").

:- comment(module,"This module defines some predicates which are
   useful for writing assertions in a readable form.").

:- comment(author,"Francisco Bueno Carrillo").

% ISO-Prolog compatibility libraries
:- use_module(library(format)).  

% Other libraries
:- use_module(library('assertions/assrt_lib'),[assertion_body/7]).
:- use_module(library(messages)).
:- use_module(library('assertions/assertions_props')).

:- regtype status_flag(F) # "@var{F} is @tt{status} or @tt{nostatus}.".

status_flag(status).
status_flag(nostatus).

:- pred write_assertion(Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to current output.".

write_assertion(Goal,Status,Type,Body,Dict,Flag):-
	write_assertion_(Goal,Status,Type,Body,Dict,Flag,no).

:- pred write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag)
	:: ( assrt_status(Status), assrt_type(Type),
	     nabody(Body), dictionary(Dict),
	     status_flag(Flag) )
        # "Writes the (normalized) assertion to current output as
           a Prolog comment.".

write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag):-
	write_assertion_(Goal,Status,Type,Body,Dict,Flag,yes).

write_assertion_(Goal,Status,Type,Body,Dict,Flag,AsComm):-
	unify_vars(Dict),
	( Flag=nostatus
	-> write_nostatus_assertion(AsComm,Type,Goal)
	 ; write_status_assertion(AsComm,Status,Type,Goal)
	),
	assertion_body(Goal,Compat,Call,Succ,Comp,Comm,Body),
	write_if_not_empty(Compat,'::',AsComm,conj),
	decide_on_call(Call,FormC),
	write_if_not_empty(Call,' :',AsComm,FormC),
	decide_on_call(Succ,FormS),
	write_if_not_empty(Succ,'=>',AsComm,FormS),
	decide_on_call(Comp,FormP),
	write_if_not_empty(Comp,' +',AsComm,FormP),
	write_comment(Comm,AsComm),
	format(".~n~n",[]),
	!.
write_assertion_(_Goal,Status,Type,Body,_Dict,_Flag,_AsComm):-
	error_message("Error printing assertion:~n:- ~w ~w ~w~n",
               [Status,Type,Body]),
	fail.

write_nostatus_assertion(yes,Type,Goal):-
	format("%% :- ~w ~q",[Type,Goal]).
write_nostatus_assertion(no,Type,Goal):-
	format(":- ~w ~q",[Type,Goal]).

write_status_assertion(yes,Status,Type,Goal):-
	format("%% :- ~w ~w ~q",[Status,Type,Goal]).
write_status_assertion(no,Status,Type,Goal):-
	format(":- ~w ~w ~q",[Status,Type,Goal]).

write_comment([],_AsComm):- !.
write_comment(Comm,AsComm):-
	check_comas_in_comment(Comm,CC ),
	write_comment_as_comment(AsComm,CC).

write_comment_as_comment(yes,Comm):-
	format('~n%% ~8|#  "~s"',[Comm]).
write_comment_as_comment(no,Comm):-
	format('~n~8|#  "~s"',[Comm]).

check_comas_in_comment( [] , [] ).

check_comas_in_comment( [A|Ar] , [A|Br] ) :-
	A \== 0'",
	check_comas_in_comment(Ar,Br).

check_comas_in_comment( [A|Ar] , [0'\\,A|Br] ) :-
	check_comas_in_comment(Ar,Br).


write_if_not_empty([],_Mod,_AsComm,_Always):- !.
write_if_not_empty([true],_Mod,_AsComm,conj):- !.
write_if_not_empty([[]],_Mod,_AsComm,disj):- !.
write_if_not_empty([[true]],_Mod,_AsComm,disj):- !.
write_if_not_empty(List,Mod,AsComm,Form):-
	write_as_comment(AsComm,Mod),
	print_prop_list(Form,List).

write_as_comment(yes,Mod):-
	format("~n%% ~8|~w ",[Mod]).
write_as_comment(no,Mod):-
	format("~n~8|~w ",[Mod]).

print_prop_list(conj,List):-
	print_conjunction(List).
print_prop_list(disj,List):-
	print_disjunction(List).

print_disjunction([]).
print_disjunction([Prop]):- !,
	print_conjunction(Prop).
print_disjunction([Prop|Props]):-
	format("( ",[]),
	print_conjunction(Prop),
	print_tail_disj(Props).

print_tail_disj([]):-
	format(" )",[]).
print_tail_disj([Prop|Props]):-
	format("; ",[]),
	print_conjunction(Prop),
	print_tail_disj(Props).

print_conjunction([]).
print_conjunction([Prop]):- !,
	format("~q",[Prop]).
print_conjunction([Prop|Props]):-
	format("( ~q",[Prop]),
	print_tail_conj(Props).

print_tail_conj([]):-
	format(" )",[]).
print_tail_conj([Prop|Props]):-
	format(", ~q",[Prop]),
	print_tail_conj(Props).

unify_vars([]).
unify_vars([N=V|Dict]):-
	V='$VAR'(N),
	unify_vars(Dict).

decide_on_call(Call,disj):-
	list(Call,list), !.
decide_on_call(_Call,conj).

%% ---------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+320,2004/03/03,18:29*59+'CET'), "BUG:printing
   comas in comment is fixed up (David Trallero Mena)").

:- comment(version(1*5+2,1999/11/29,18:02*53+'MET'), "assrt_props is
   now assertions_props. (Francisco Bueno Carrillo)").

:- comment(version(1*3+45,1999/08/05,14:43*07+'MEST'), "Print
   properties in assertions as disjunctions or conjunctions.
   (Francisco Bueno Carrillo)").

:- comment(version(0*9+26,1999/03/26,14:43*08+'MET'), "Fixed
   multi-arity warnings.  (Manuel Hermenegildo)").

:- comment(version(0*5+1,1998/09/25,17:26*24+'MET DST'), "Modified to
   be able to print assertions as a comment.  (Francisco Bueno
   Carrillo)").

:- comment(version(0*5+0,1998/2/2), "Created. (Francisco Bueno)").

%% ---------------------------------------------------------------------------


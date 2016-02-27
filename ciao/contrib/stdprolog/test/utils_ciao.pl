% This file defines some utility predicates in SICStus 

% :- set_prolog_flag(language, iso). % !! not for ciao

:- use_package(iso). % Dat: doesn't fix X is 1/0.

% !! add this for ciao
%%%% pts %%%%
% vvv abort on all errors and warnings
%:- user:asserta(( message_hook(Severity, Message, _Lines) :-
%   ( Severity=warning -> Code=3 ; Severity=error, Code=4 ), !,
%   user:retractall(message_hook(_,_,_)),
%   print_message(Severity, Message),  halt(Code) )).
%% vvv abort on Ctrl-<C> -- doesn't seem to be needed, and seems to have no effect
%:- prolog:'$abolish'(control_c_handler, prolog),
%   prolog:asserta((control_c_handler :- halt(125))).

:- use_module(library(dcg(dcg_tr))). % Dat: for regexp.pl ERROR: this module should import phrase/3 from dcg_tr

:- use_module(library(iso_incomplete)). % stream_property/2, close/1. % !! ciao should import by default

/*
retractall(X)
numbervars(T, N, NR)
member(X, L)
append(L1, L2, L3)
select(X, L1, L2)
prefix(L1, L2)
last(L, X) :-
term_variables(T, V)
variant(T1, T2)
between(A, B, C)
*/

memberchk(X, [X|_]):- !.
memberchk(X, [_|L]):-
	memberchk(X, L).
        
prefix([], _).
prefix([X|LP], [X|L]) :-
	prefix(LP, L).
    
/* --- */

term_variables(T, V) :-
	collect_vars(T, V, []).

%% collect_vars(Term, Vs, Vs0): Vs is the list of variables in Term prepended
%%      to Vs0.
collect_vars(V, Vs, Vs0) :-
	var(V),
	\+(var_memberchk(V, Vs0)), !,
	Vs = [V|Vs0].
collect_vars(C, Vs, Vs0) :-
	compound(C), !,
	C =.. [_|Args],
	collect_vars0(Args, Vs, Vs0).
collect_vars(_, Vs, Vs).

%% collect_vars0(List, Vs, Vs0): Vs is the list of variables in all of the
%%      terms in List prepended to Vs0.
collect_vars0([], Vs, Vs).
collect_vars0([H|T], Vs, Vs0) :-
	collect_vars(H, Vs1, Vs0),
	collect_vars0(T, Vs, Vs1).

var_memberchk(V0, [V|_]) :-
	V0 == V, !.
var_memberchk(V0, [_|L]) :-
	var_memberchk(V0, L).


/* --- */

subsumes_chk(General, Specific) :-
	\+  (   numbervars(Specific, 0, _),
		\+ General = Specific
	    ).

variant(A, B) :-
	subsumes_chk(A, B),
	subsumes_chk(B, A).

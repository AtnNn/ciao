% This file defines some utility predicates in SICStus 

:- set_prolog_flag(language, iso). % !! document this

% !! document: how to add a new Prolog implementation
% !! document this
% !! document: argument order in message_hook/3 is different in SICStus and SWI-Prolog
%%%% pts %%%%
% vvv abort on all errors and warnings
:- user:asserta(( message_hook(Severity, Message, _Lines) :-
   ( Severity=warning -> Code=3 ; Severity=error, Code=4 ), !,
   user:retractall(message_hook(_,_,_)),
   print_message(Severity, Message),  halt(Code) )).

% vvv abort on Ctrl-<C> -- doesn't seem to be needed, and seems to have no effect
:- prolog:'$abolish'(control_c_handler, prolog),
   prolog:asserta((control_c_handler :- halt(125))).

/*
retractall(X)
numbervars(T, N, NR)
member(X, L)
memberchk(X, L)
append(L1, L2, L3)
select(X, L1, L2)
prefix(L1, L2)
last(L, X) :-
term_variables(T, V)
variant(T1, T2)
between(A, B, C)
*/

:- use_module(library(lists)).
:- use_module(library(terms)).

% --- Auxilary.

iso :-
	set_prolog_flag(language, iso).

sicstus :-
	set_prolog_flag(language, sicstus).



:- module(term_to_string, [term_to_string/2, string_to_term/2], []).

:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(strings)).
:- use_module(library(write)).
:- use_module(library(read)).

term_to_string(T, S) :-
	mktemp_in_tmp('t2sXXXXXX', TMP),
	open_output(TMP, Out),
	display(T),
	close_output(Out),
	open_input(TMP, In),
	get_line(S),
	close_input(In),
	delete_file(TMP).

string_to_term(S, T) :-
	mktemp('t2sXXXXXX', TMP),
	open_output(TMP, Out),
	write_string(S),
	write('.'),
	close_output(Out),
	open_input(TMP, In),
	read_term(T, [variable_names(V)]),
	close_input(In),
	delete_file(TMP),
	change_names(V).

change_names([]).
change_names([H=H1|T]) :-
	H=H1,
	change_names(T).

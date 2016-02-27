:- module(atom_to_term, [atom_to_term/2], []).

:- use_module(library(read)).

atom_to_term(Atom, Term) :-
	pipe(ReadFrom, WriteTo),
	display(WriteTo, Atom),
	display(WriteTo, '.'),
	close(WriteTo),
	read_term(ReadFrom, Term, []),
	close(ReadFrom).

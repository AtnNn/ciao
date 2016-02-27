:- module(_, _, []).

:- use_module(library(system)).
:- use_module(library(file_utils)).
:- use_module(library(make(system_extra)), [no_tr_nl/2]).

shell_string(Command, String) :-
	popen(Command, read, Stream),
	stream_to_string(Stream, String0),
	no_tr_nl(String0, String).

shell_atom(Command, Atom) :-
	shell_string(Command, String),
	atom_codes(Atom, String).

:- module(miniprolog, _, [fsyntax, assertions]).

:- use_module(library(llists)).
:- use_module(library(file_utils)).
:- use_module(library(messages)).
:- use_module(library(read)).
:- use_module(library(streams)).
:- use_module(library(terms)).
:- use_module(library(write)).

:- use_module(library(make(system_extra))).
:- use_module(engine(system_info), [get_exec_ext/1]).

:- reexport(library(miniprolog(miniprolog_auto))).

:- doc(author, "Edison Mera").

:- doc(module,
	    "This module provides utilities to work with miniprolog.").

:- doc(generate_flat_file/2, "Takes a file and generate a prolog
	file that only contain directives that can be understood by
	the mini prolog engine.").

:- initialization(init_miniprolog_dir).

:- data miniprolog_dir_db/1.

init_miniprolog_dir :-
	absolute_file_name(library(miniprolog), FileName0),
	atom_concat(BaseName, '.pl', FileName0),
	atom_concat(BaseName, '/',   MiniPrologDir),
	set_miniprolog_dir(MiniPrologDir).

set_miniprolog_dir(File) :-
	retractall_fact(miniprolog_dir_db(_)),
	asserta_fact(miniprolog_dir_db(File)).

generate_flat_file(FileName, FlatFile) :-
	open(FileName, read, S),
	open_output(FlatFile, OS),
	read_write_items(S),
	!,
	close_output(OS),
	close(S).

write_clause((H :- B)) :-
	!,
	write_literal(H),
	write(' :-'),
	nl,
	write_body(B),
	write('.'),
	nl.
write_clause(H) :-
	write_literal(H),
	write('.'),
	nl.

write_body((L, B)) :-
	!,
	write('  '),
	write_literal(L),
	write(','), nl,
	write_body(B).
write_body(B) :-
	write('  '),
	write_literal(B).

write_literal(A) :-
	var(A),
	!,
	write(A).
write_literal(A) :-
	constant(A),
	!,
	write(A).
write_literal(A is B) :-
	!,
	write(A),
	write(' is '),
	write(B).
write_literal(A < B) :-
	write(A),
	write(' < '),
	write(B).
write_literal(A > B) :-
	!,
	write(A),
	write(' > '),
	write(B).
write_literal(A =< B) :-
	!,
	write(A),
	write(' =< '),
	write(B).
write_literal(A >= B) :-
	!,
	write(A),
	write(' >= '),
	write(B).
write_literal(A \== B) :-
	!,
	write(A),
	write(' \\== '),
	write(B).
write_literal(A = B) :-
	!,
	write_functor(A),
	write(' = '),
	write_functor(B).
write_literal(A) :-
	write_functor(A).

write_functor(A) :-
	var(A),
	!,
	write(A).
write_functor(A) :-
	constant(A),
	!,
	write(A).
write_functor([A|B]) :-
	write('[ '),
	write_functor(A),
	write_list(B).
write_functor(A + B) :-
	write('\'+\'('),
	write_functor(A),
	write(','),
	write_literal(B),
	write(')').
write_functor(A - B) :-
	write('\'-\'('),
	write_functor(A),
	write(','),
	write_functor(B),
	write(')').
write_functor(A * B) :-
	write('\'*\'('),
	write_functor(A),
	write(','),
	write_functor(B),
	write(')').
write_functor(A / B) :-
	write('\'/\'('),
	write_functor(A),
	write(','),
	write_functor(B),
	write(')').
write_functor(L) :-
	L =.. [F|Args],
	write_atom(F),
	(
	    Args \== [] ->
	    write('( '),
	    write_args(Args),
	    write(' )')
	;
	    true
	).

write_list(B) :-
	(
	    var(B) ->
	    write(' | '),
	    write_functor(B),
	    write(' ]')
	;
	    B = [H|T] ->
	    write(', '),
	    write_functor(H),
	    write_list(T)
	;
	    B = [] ->
	    write(' ]')
	;
	    write(' | '),
	    write_functor(B),
	    write(' ]')
	).

write_atom('=') :- !, write('\'=\'').
write_atom('+') :- !, write('\'+\'').
write_atom('-') :- !, write('\'-\'').
write_atom('*') :- !, write('\'*\'').
write_atom('**') :- !, write('\'**\'').
write_atom('/') :- !, write('\'/\'').
write_atom(A) :-
	atom(A),
	write(A).

write_args([]).
write_args([Arg|Args]) :-
	write_functor(Arg),
	write_args2(Args).

write_args2([]).
write_args2([Arg|Args]) :-
	write(', '),
	write_functor(Arg),
	write_args2(Args).

read_write_items(S) :-
	read(S, R),
	(
	    R == end_of_file ->
	    true
	;
	    ( R = (:- _B) -> true
	    ; write_clause(R) ),
	    read_write_items(S)
	).

generate_compile_commands(FileBase, TmpFile) :-
	string_to_file(~flatten(["consult(\'", ~atom_codes(FileBase),
		    "_flat_auto.pl\').\nsavewamcode(\'",
		    ~atom_codes(FileBase),
		    ".wam\').\nhalt.\n"]), TmpFile).

:- doc(compile_file/1, "Takes a prolog file and generates a .wam
	file that contains the compiler output.").

miniprolog_wam := ~atom_concat([~miniprolog_dir_db,
		'bin/_', ~get_platform, '/wam/wam', ~get_exec_ext]).

compile_file(FileAlias) :-
	absolute_file_name(FileAlias, FileName),
	atom_concat(FileBase, '.pl', FileName),
	get_tmp_dir(TmpDir),
	generate_flat_file(FileName,
	    ~atom_concat(FileBase, '_flat_auto.pl')),
	generate_compile_commands(FileBase,
	    ~atom_concat(TmpDir, '/compile_commands.tmp')),
	do([~miniprolog_wam, ' < ', TmpDir, '/compile_commands.tmp'],
	    ~atom_concat(TmpDir, '/compile.log'),
	    ~atom_concat(TmpDir, '/compile.err'),
	    [show_output_on_error, show_error_on_error, nofail]).

mpsetup(Command) :-
	display(do(['cd ', ~miniprolog_dir_db, ' ; make -s MPARCH=',
		    ~get_platform, ' ', Command], nofail)),
	do(['cd ', ~miniprolog_dir_db, ' ; make MPARCH=',
		~get_platform, ' ', Command], nofail).

lazy_miniprolog_start :-
	file_exists(~miniprolog_wam) -> true
    ;
	show_message(warning, "Miniprolog is not compiled. Compiling now."),
	mpsetup(all).

:- module(_, [detect_c_headers/1], [assertions]).

:- use_module(library(llists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(foreign_compilation)).
:- use_module(library(system)).
:- use_module(library(distutils)).
:- use_module(library(make(system_extra))).


:- doc(author, "Edison Mera").

:- doc(title, "C Header Detection").

:- doc(summary, "This module provides a predicate to detect a C Header").

headers_includes([],               []        ).
headers_includes([Header|Headers], [["#include <", HeaderS, ">\n"]
	    |Includes]) :-
	atom_codes(Header, HeaderS),
	headers_includes(Headers, Includes).

:- true pred detect_c_headers(+ list(filename)) # "Succeeds if all the
	c header files in the argument are valid.".

detect_c_headers(Headers) :-
	headers_includes(Headers, Includes),
	flatten([Includes,
		"int main(void){\n"||
		"return 0;\n"||
		"}\n"], S),
	mktemp_in_tmp('headertmpXXXXXX', FileBase),
	atom_concat(FileBase, '.c', FileName),
	string_to_file(S, FileName),
	get_exec_ext(Exec),
	compiler_and_opts(Compiler, Opts),
	flatten([Compiler, ' ', Opts, ' ', '-o ', FileBase, ' ', FileName,
		' > /dev/null 2>&1'], C),
	atom_concat(C, CA),
	atom_concat(FileBase, Exec, FileExec),
	!,
	try_finally(
	    true,
	    (
		system(CA, R),
		R == 0
	    ),
	    del_files_nofail([FileBase, FileExec, FileName])
	).

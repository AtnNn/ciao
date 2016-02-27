:- module(_, [], [assertions, fsyntax, hiord]).

:- doc(title,  "Auxiliary Definitions for makedir").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jos@'{e} F. Morales").

:- use_module(library(distutils), [make_subdir/5]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(ciaodesrc(makedir('ConfigValues')), [lpdoc2/1, gmake/1,
	build_doc_dir/1, setlocalciao/1, command_option/1, stop_if_error/1]).

:- export(invoke_lpdoc/2).
invoke_lpdoc(ConfigFile, Options) :-
	invoke_lpdoc(~atom_concat([' -d stop_if_error=', ~stop_if_error,
		    ' -f ', ConfigFile, ' ', Options])).

:- export(invoke_lpdoc/1).
invoke_lpdoc(Options) :-
	make_subdir(~lpdoc2, ~build_doc_dir, ~setlocalciao, Options, ~command_option).

:- use_module(ciaodesrc(makedir('ConfigValues')), [build_doc_dir/1]).

:- export(invoke_gmake/2).
invoke_gmake(Dir, Target) :-
	make_subdir(~gmake, Dir, '', Target, ~command_option).

:- export(invoke_gmake_localciao/2).
% (like invoke_gmake, but with the enviroment set for the local ciao)
invoke_gmake_localciao(Dir, Target) :-
	make_subdir(~gmake, Dir, ~setlocalciao, Target, ~command_option).

:- export(codes_atom/2).
codes_atom(A, B) :- atom_codes(B, A).


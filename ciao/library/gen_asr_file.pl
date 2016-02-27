:- module(_, [gpo/1, gaf/1, gaf_main_rel/1], [assertions, nortchecks]).

:- use_module(library(assertions(assrt_lib)),
	    [
		get_code_and_related_assertions/5,
		cleanup_code_and_related_assertions/0
	    ]).

:- use_module(library(llists)).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(make(up_to_date))).
:- use_module(library(compiler),                 [make_po/1]).
:- use_module(library(compiler(c_itf_internal)), [cleanup_itf_cache/0]).

get_dummy_file_name(FileName, DummyFileName) :-
	atom_concat(FileBase, '.pl',        FileName),
	atom_concat(FileBase, '_tmp_co.pl', DummyFileName).

gen_dummy_file(FileName, DummyFileName) :-
	get_dummy_file_name(FileName, DummyFileName),
	atom_codes(FileName, SFileName),
	flatten([
		":- module(_, _, [assertions]).\n" ||
		":- use_module(\'" || SFileName,
		"\').\n" ||
		"main."], Content),
	string_to_file(Content, DummyFileName).

:- doc(bug, "gaf_main_rel/1 is implemented using a dummy file,
	and looks like a kludge, because the assertion library is
	unable to generate the asr file for the main predicate.").

gaf_main_rel(F) :-
	absolute_file_name(F, FileName),
	gen_asr_file_main_rel(FileName).

gen_asr_file_main_rel(FileName) :-
	gen_dummy_file(FileName, DummyFileNamePl),
	gen_asr_file(DummyFileNamePl),
	atom_concat(DummyFileName, '.pl',  DummyFileNamePl),
	atom_concat(DummyFileName, '.itf', DummyFileNameItf),
	delete_file(DummyFileNamePl),
	delete_file(DummyFileNameItf).

/*
slow_gaf(F) :-
	absolute_file_name(F, FileName),
	gen_asr_file(FileName).
*/

gen_asr_file(FileName) :-
	get_code_and_related_assertions(FileName, _M, _Base, _Suffix, _Dir),
	cleanup_itf_cache,
	cleanup_code_and_related_assertions.

:- doc(bug, "If runtime_checks are enabled, then the itf file
	generated in gaf/1, could be not vaild w.r.t. the generated in
	gpo/1, that is why I delete the itf in the gpo/1 predicate,
	when creating the po file. -- EMM").

gaf(F) :-
	absolute_file_name(F, FileName),
	atom_concat(FileBase, '.pl',  FileName),
	atom_concat(FileBase, '.asr', FileNameAsr),
	( up_to_date(FileNameAsr, FileName) -> true
	; gen_asr_file_main_rel(FileName) ).

gpo(F) :-
	absolute_file_name(F, FileName),
	atom_concat(FileBase, '.pl',  FileName),
	atom_concat(FileBase, '.po',  FileNamePo),
	atom_concat(FileBase, '.itf', FileNameItf),
	(
	    up_to_date(FileNamePo,  FileName),
	    up_to_date(FileNameItf, FileName) ->
	    true
	;
	    ( file_exists(FileNameItf) ->
		delete_file(FileNameItf)
	    ; true
	    ), % Force 
	    ( file_exists(FileNamePo) ->
		delete_file(FileNamePo)
	    ; true
	    ), % Force 
	    make_po(FileName)
	).

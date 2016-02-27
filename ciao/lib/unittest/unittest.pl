:- module(unittest,
	    [
		run_test_dir/2,
		run_test_module/1,
		run_test_module/2,
		run_test_module/3,
		show_tests_summaries/1,
		run_test_related_modules/1,
		show_untested_preds/1
	    ],
	    [assertions, isomodes, nativeprops, dcg, fsyntax, hiord]).
:- use_module(library(unittest(unittest_statistics))).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(sort)).
:- use_module(library(aggregates)).
:- use_module(library(rtchecks(rtchecks_utils))).
:- use_module(library(assertions(assrt_lib)),
	    [
		cleanup_code_and_related_assertions/0,
		assertion_read/9,
		clause_read/7,
		get_code_and_related_assertions/5,
		assertion_body/7,
		comps_to_goal/3
	    ]).
:- use_module(library(assertions(c_itf_props)), [filename/1]).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(hiordlib)).
:- use_module(library(compiler(c_itf))).
:- use_module(library(compiler), [unload/1]).
:- use_module(library(write)).
:- use_module(library(pretty_print)).
:- use_module(library(read)).
:- use_module(library(lists), [append/3, length/2, select/3, intersection/3,
		difference/3]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(make(system_extra))).
:- use_module(library(compiler(exemaker)), [make_exec/2]).
:- use_module(library(unittest(unittest_base))).
:- use_module(library(distutils(collect_modules)), [current_dir_module/6]).
:- use_module(library(distutils(skip_settings)), [nocompile_dirs/1,
		nocompile_files/1]).

:- doc(title, "Unit test driver").

:- doc(author, "Edison Mera").
:- doc(author, "Alvaro Sevilla San Mateo").

:- doc(module, "This module contains the predicates that lets to
	run the unit tests.").


:- push_prolog_flag(write_strings, on).

loader_name('ciao_unittest_loader').

cleanup_unittest(TmpDir) :-
	cleanup_c_itf_data,
	cleanup_code_and_related_assertions,
	cleanup_test_attributes,
	cleanup_global_runners(TmpDir).

cleanup_test_attributes :-
	retractall_fact(test_attributes_db(_, _, _, _, _, _, _, _)).

cleanup_global_runners(_TmpDir) :-
	atom_concat('rm -rf ', _TmpDir, A),
	system(A).

:- pred show_untested_preds(A) : sourcename(A).

show_untested_preds(Alias) :-
	tmp_dir(TmpDir),
	cleanup_unittest(TmpDir),
	get_assertion_info(Alias, _Module),
	findall(Message, current_untested_pred(Alias, Message), Messages),
	pretty_messages(Messages).

unittest_type(test).
unittest_type(texec).

current_untested_pred(Alias, Message) :-
	absolute_file_name(Alias, '_opt', '.pl', '.', FileName, FileBase,
	    AbsDir),
	atom_concat([AbsDir, '/', Module], FileBase),
	exports(FileBase, F, A, _DefType, _Meta),
	functor(Pred, F, A),
	\+ (
	    assertion_read(Pred, Module, check, Type, _Body, _Dict,
		_Source, _LB, _LE),
	    unittest_type(Type)
	),
	(clause_read(FileBase, Pred, _, _, FileName, LB, LE) -> true),
	Message = message_lns(FileName, LB, LE, warning,
	    [Module, ':', F, '/', A, ' does not have any unit test']).

notest_dirs := ['NOTEST', '.NOTEST'].

notest_files := ['NOTESTFILES', '.NOTESTFILES'].

:- pred run_test_dir(BaseDir, Args) : (filename * list(atm)) #
"Executes all the tests in the modules of a directory and its
	subdirectories. You can create a NOTEST file to indicate that
	the modules in the sub-directory containing such archive must
	not be tested.  Also an optional File NOTESTFILES can be
	created with the list of patterns for modules that must not be
	tested".

:- data test_result_summary/2.

run_test_dir(BaseDir, Args) :-
	tmp_dir(TmpDir),
	cleanup_unittest(TmpDir),
	(
	    append(~nocompile_dirs,  ~notest_dirs,  NoDirs),
	    append(~nocompile_files, ~notest_files, NoFiles),
	    current_dir_module(BaseDir, NoDirs, NoFiles, _, _, FileName),
	    run_test_module_args(TmpDir, FileName, Args, IdxTestSummaries),
	    show_tests_summaries(IdxTestSummaries),
	    assertz_fact(test_result_summary(FileName, IdxTestSummaries)),
	    fail
	;
	    true
	),
	display('{Summary}\n'),
	findall(E, (
		retract_fact(test_result_summary(FN, E)),
		statistical_summary(['{In ', FN, '\n'], E)), L),
	statistical_summary(['{Total:\n'], L).

/*
:- doc(bug, "The following implementation of run_test_dir/2 does
	not works well because it do not load the source code in the
	clause_read/7 dynamic predicate. -- EMM").

run_test_dir_opt(yes, BaseDir, Args) :-
	run_test_dir_fast(BaseDir, Args).
run_test_dir_opt(no,  BaseDir, Args) :-
	run_test_dir(BaseDir, Args).

puller_file('puller_auto.pl').

run_test_dir(BaseDir, Args) :-
	tmp_dir(TmpDir),
	cleanup_unittest(TmpDir),
	make_dirpath(TmpDir),
	puller_file(BPullerFile),
	atom_concat(TmpDir, BPullerFile, PullerFile),
	create_puller_module(BaseDir, PullerFile),
	get_code_and_related_assertions(PullerFile, _, _, _, _),
	dir_assrt_modules(BaseDir, Modules),
	run_test_assertions(TmpDir, Modules, Args),
	cleanup_unittest(TmpDir).

create_puller_module(BaseDir, PullerFile) :-
	open(PullerFile, write, SO),
	write_puller_module(BaseDir, SO),
	close(SO).

write_puller_module(BaseDir, SO) :-
	portray_clause(SO, (:- module(_, _, [assertions]))),
	(
	    current_dir_module(BaseDir, FileName),
	    portray_clause(SO, (:- use_module(FileName))),
	    fail
	;
	    true
	).

dir_assrt_modules(BaseDir) :=
	~sort(~findall(Module, ( current_dir_module_(BaseDir, Module),
		    current_assr_module(Module) ))).

current_dir_module_(BaseDir, Module) :-
	current_dir_module(BaseDir, File),
	file_dir_name(File, _, Name),
	atom_concat(Module, '.pl', Name).

run_test_dir_fast_0(BaseDir, Args) :-
	cleanup_unittest,
	read_asr_dir(BaseDir),
	set_of_modules(Modules),
	run_test_assertions(Modules, Args).

read_asr_dir(BaseDir) :-
	(
	    read_asr_current_module(BaseDir),
	    fail
	;
	    true
	).

read_asr_current_module(BaseDir) :-
	current_dir_module(BaseDir, FileName),
	atom_concat(FileBase, '.pl', FileName),
	atom_concat(FileBase, '.asr', AsrFile),
	file_exists(AsrFile),
	read_asr_file(FileBase, '').
*/

module_src(Module, Src) :-
	defines_module(Src, Module),
	!.
module_src(Module, Src) :-
	(
	    unittest_type(Type),
	    assertion_read(_, Module, check, Type, _, _, Src, _, _),
	    atom_concat([_, '/', Module, '.pl'], Src) -> true
	;
	    fail
	).

:- pred show_tests_summaries(TestSummaries) # "Show a list of messages
	with the results of tests.".

show_tests_summaries(IdxTestSummaries0) :-
	flatten(IdxTestSummaries0, IdxTestSummaries),
	map(IdxTestSummaries, process_runtime_check, Messages, []),
	pretty_messages(Messages).
%	statistical_summary(IdxTestSummaries0).

:- pred run_test_module(Alias, Args, TestSummaries) :
	(sourcename(Alias), list(Args)) => list(TestSummaries) # "Run
	the tests for the specified module.  The option list
	@var{Args} can contain '-o' or '-e' if you like to show the
	standard output and standard error of the test execution
	respectively.  @var{TestSummaries} contains a list of terms
	with the results of tests.".

run_test_module(Alias, Args, IdxTestSummaries) :-
	tmp_dir(TmpDir),
	run_test_module_args(TmpDir, Alias, Args, IdxTestSummaries).

:- pred run_test_module(Alias, Args) :
	(sourcename(Alias), list(Args)) # "Like
	@pred{run_test_module/3}, but show messages with the results
	of tests.".

run_test_module(Alias, Args) :-
	run_test_module(Alias, Args, IdxTestSummaries),
	show_tests_summaries(IdxTestSummaries).

:- pred run_test_module(Alias) : sourcename(Alias) # "Like
	@pred{run_test_module/2}, but using default options.".

run_test_module(Alias) :-
	run_test_module(Alias, [], IdxTestSummaries),
	show_tests_summaries(IdxTestSummaries),
	statistical_summary(['{In ', Alias, '\n'], IdxTestSummaries).

run_test_module_args(TmpDir, Alias, Args, IdxTestSummaries) :-
	cleanup_unittest(TmpDir),
	get_assertion_info(Alias, Module),
	run_test_assertions(TmpDir, [Module], Args, IdxTestSummaries).
% 	cleanup_unittest(TmpDir).

:- pred run_test_related_modules(Alias) : sourcename(Alias).

run_test_related_modules(Alias) :-
	tmp_dir(TmpDir),
	run_test_related_modules_args(TmpDir, Alias, [], IdxTestSummaries),
	show_tests_summaries(IdxTestSummaries).

run_test_related_modules_args(TmpDir, Alias, Args, IdxTestSummaries) :-
	cleanup_unittest(TmpDir),
	absolute_file_name(Alias, FileName),
	get_code_and_related_assertions(FileName, _, _, _, _),
	set_of_modules(Modules),
	run_test_assertions(TmpDir, Modules, Args, IdxTestSummaries),
	cleanup_unittest(TmpDir),
	(unload(FileName) -> true ; true).

:- export(get_assertion_info/2).
get_assertion_info(Alias, Module) :-
	absolute_file_name(Alias, '_opt', '.pl', '.', FileName, Base,
	    AbsDir),
	atom_concat([AbsDir, '/', Module], Base),
	get_code_and_related_assertions(FileName, Module, Base, '.pl',
	    AbsDir).

set_of_modules := ~sort(~findall(Module, current_assr_module(Module))).

current_assr_module(Module) :-
	assertion_read(_A, Module, check, Type, _E, _F, _G, _H, _I),
	unittest_type(Type).

:- pred create_runner(+atm, +list) + (not_fails, no_choicepoints).

create_runner(TmpDir, Modules) :-
	(
	    Modules \== [] ->
	    create_global_runner(TmpDir, Modules, RunnerFile),
	    create_loader(TmpDir, RunnerFile)
	;
	    true
	).

% But note that create_loader/2 clean the assertion_read/9 database,
% that means, from this point, we cannot trust in what such predicate
% contains.

:- pred create_loader(+atm, +atm) + (not_fails, no_choicepoints).

create_loader(TmpDir, RunnerFile) :-
	loader_name(BLoader),
	atom_concat(TmpDir, BLoader, Loader),
	make_exec([RunnerFile], Loader).

:- pred run_test_assertions(+atm, +list, +list, -list) +
	(not_fails, no_choicepoints).

run_test_assertions(TmpDir, Modules, Args, IdxTestSummaries) :-
% 	show_message(note, "Creating test applications"),
	make_dirpath(TmpDir),
	create_test_input(TmpDir, Modules),
	( test_attributes_db(_, _, _, _, _, _, _, _) ->
	    create_runner(TmpDir, Modules),
% 	    runner_global_file_name(BRunnerFile),
% 	    atom_concat(TmpDir, BRunnerFile, RunnerFile),
% 	    show_message(note, "Running tests"),
	    findall(IdxTestSummary, run_test_assertion(TmpDir, Args,
		    IdxTestSummary), IdxTestSummaries)
	; IdxTestSummaries = []
	).

run_test_assertion(TmpDir, Args, TestAttributes-TestSummary) :-
	loader_name(BLoader),
	atom_concat(TmpDir, BLoader, Loader),
	do_test(TmpDir, Loader, Args, Idx, TestResults),
	group_list(TestResults, [], TestSummary),
	test_attributes_db(Idx, Module, F, A, Comment, Source, LB, LE),
	TestAttributes = test_attributes(Module, F, A, Comment,
	    Source, LB, LE).

count_text(1, '') :- !.
count_text(N, [' ', N, ' times']).

signals_text([],      '') :- !.
signals_text(Signals, [' Signals thrown: ', ~~(Signals)]).

comment_text("",      '') :- !.
comment_text(Comment, [' <<', $$(Comment), '>>']).

:- pred process_runtime_check(TATS, M0, M) : nonvar(TATS) => nonvar(M0)
	+ not_fails.

process_runtime_check(TestAttributes-TestSummary) -->
	{TestAttributes = test_attributes(Module, F, A, Comment,
		Source, LB, LE)},
	map(TestSummary, process_runtime_check_ta(Module, F, A, Comment,
		Source, LB, LE)).

process_runtime_check_ta(count(ErrorStatus, Count), Module, F, A, Comment,
	    Source, LB, LE) -->
	{ErrorStatus = st(RTCErrors, Signals, Result)},
	{count_text(Count, CountMsg)},
	{signals_text(Signals, SignalsMsg)},
	{comment_text(Comment, CommentMsg)},
	(
	    {is_failed_test(ErrorStatus)} ->
	    [message_lns(Source, LB, LE, error, [Module, ':', F, '/', A,
			' (Result: ', ''(Result), [](CountMsg),
			') Failed test', [](CommentMsg), '.', [](SignalsMsg)])
	    ],
	    map(RTCErrors, rtcheck_to_messages)
	;
	    [message_lns(Source, LB, LE, note, [Module, ':', F,
			'/', A, ' (Result: ', ''(Result), [](CountMsg),
			') Passed test', [](CommentMsg), '.', [](SignalsMsg)])]
	),
	!.

:- data test_input_db/2.
:- data test_output_db/2.
:- data test_attributes_db/8.

:- meta_predicate assert_file(?, pred(1)).
assert_file(File, AssertMethod) :-
	open(File, read, SI),
	repeat,
	read(SI, Term),
	(
	    Term == end_of_file ->
	    !,
	    close(SI)
	;
	    AssertMethod(Term),
	    fail
	).

assert_test_input(test_input_db(A, B)) :-
	assertz_fact(test_input_db(A, B)).

assert_test_output(test_output_db(A, B)) :-
	assertz_fact(test_output_db(A, B)).

select_commands(DisplayStdOut, DisplayStdErr) -->
	(select('-o') -> {DisplayStdOut = yes} ; {DisplayStdOut = no}),
	(select('-e') -> {DisplayStdErr = yes} ; {DisplayStdErr = no}).

do_test(TmpDir, Loader, Args0, Idx, TestResults) :-
	file_test_output(BOutFile),
	atom_concat(TmpDir, BOutFile, OutFile),
	file_test_input(BInFile),
	atom_concat(TmpDir, BInFile, InFile),
	empty_output(TmpDir),
	select_commands(DisplayStdOut, DisplayStdErr, Args0, Args),
	exec(Loader, Args, SExecI, SExecO, SErrO, wait, _PID, _ErrCode),
	close(SExecI),
	stream_to_string(SExecO, StrOut),
	(DisplayStdOut == yes -> display_string(StrOut) ; true),
	stream_to_string(SErrO, StrErr),
	(DisplayStdErr == yes -> display_string(StrErr) ; true),
	retractall_fact(test_input_db(_,  _)),
	retractall_fact(test_output_db(_, _)),
	assert_file(InFile,  assert_test_input),
	assert_file(OutFile, assert_test_output),
	retract_fact(test_input_db(Idx0, _Module)),
% 	message(note, ['Running test ', Idx, ' for module ', _Module]),
	findall(TestResult, retract_fact(test_output_db(Idx0, TestResult)),
	    TestResults0),
	(
	    TestResults0 == [] ->
	    !,
	    (
		Idx = Idx0,
		TestResults = [st([], [], aborted(StrErr))]
	    ;
		save_remaining_test_inputs(InFile),
		do_test(TmpDir, Loader, Args, Idx, TestResults)
	    )
	;
	    Idx = Idx0,
	    TestResults = TestResults0
	).

save_remaining_test_inputs(File) :-
	open(File, write, SI),
	(
	    retract_fact(test_input_db(Idx, Module)),
	    portray_clause(SI, test_input_db(Idx, Module)),
	    fail
	;
	    close(SI)
	).

% :- pred atom_concat_(+atm,+atm,-atm) + (not_fails, no_choicepoints).

% atom_concat_(A,B,C) :- atom_concat(A,B,C).

:- pred create_test_input(+atm, +list) + (not_fails, no_choicepoints).

create_test_input(TmpDir, Modules) :-
	file_test_input(BFileTestInput),
	atom_concat(TmpDir, BFileTestInput, FileTestInput),
	cleanup_test_attributes,
	open(FileTestInput, write, SI),
	(
	    member(Module, Modules),
	    current_fact(assertion_read(Pred, Module, check, Type,
		    Body, Dict, Src, LB, LE), Ref),
	    unittest_type(Type),
	    assertion_body(_Pred, _, _, _, _, Comment, Body),
	    functor(Pred, F, A),
	    ref_atom(Ref, Idx),
	    assertz_fact(test_attributes_db(Idx, Module, F, A, Comment, Src,
		    LB, LE)),
	    unittest_print_clause(test_input_db(Idx, Module), SI, Dict),
	    fail
	;
	    close(SI)
	).

:- pred create_global_runner(+atm, +list, ?atm) + (not_fails, no_choicepoints).

create_global_runner(TmpDir, Modules, RunnerFile) :-
	runner_global_file_name(BRunnerFile),
	atom_concat(TmpDir, BRunnerFile, RunnerFile),
	open(RunnerFile, write, IO),
	(
	    unittest_print_clauses(
		[
		    (:- module(_, _, [ciaopaths])),
		    (:- use_module(library(unittest(unittest_utils)))),
		    (:- use_module(library(unittest(unittest_base))))
		], IO, []),
	    (
		member(Module, Modules),
		module_src(Module, Src),
		(
		    wrapper_file_name(TmpDir, Module, WrapperFile),
		    create_module_wrapper(TmpDir, Module, Src,
			WrapperFile),
		    unittest_print_clause(
			(:- use_module(WrapperFile)), IO, []),
		    module_test_entry(Module, TestEntry, Idx),
		    unittest_print_clause(
			( internal_runtest_module(Module, Idx) :-
			    TestEntry ), IO, [])
		->
		    true
		;
		    error(['Failure in create_global_runner/3'])
		),
		fail
	    ;
		true
	    ),
	    file_test_input(BFileTestInput),
	    atom_concat(TmpDir, BFileTestInput, FileTestInput),
	    unittest_print_clauses(
		[
		    ( main(A) :-
			process_test_args(A),
			runtests
		    ),
		    ( runtests :-
			empty_output(TmpDir),
			read_file_loop(FileTestInput,
			    test_input_db(Idx, Module)),
			internal_runtest_module(Module, Idx),
			fail
		    ;
			true
		    )
		], IO, [])
	),
%	fmode(RunnerFile, M0),
%	M1 is M0 \/ ((M0 >> 2) /\ 0o111), % Copy read permissions to execute
%	chmod(RunnerFile, M1),
	close(IO).

:- use_module(library(rtchecks(rtchecks_basic)),
	    [list_to_lits/2, get_prop_args/3, get_pretty_names/5]).
:- use_module(library(rtchecks(rtchecks_tr)),
	    [collect_assertions/3, valid_commands/1, generate_rtchecks/7]).

module_test_entry(Module, TestEntry, Idx) :-
	atom_concat(Module, '$test', ModuleF),
	TestEntry =.. [ModuleF, Idx].

current_test_module(Src, (:- use_module(TestModule))) :-
	clause_read(Src, 1, load_test_module(TestModule), _, _, _, _).
current_test_module(Src, (:- use_module(TestModule, Predicates))) :-
	clause_read(Src, 1, load_test_module(TestModule, Predicates),
	    _, _, _, _).
current_test_module(Src, (:- use_package(TestModule))) :-
	clause_read(Src, 1, load_test_package(TestModule),
	    _, _, _, _).

collect_test_modules(Src) :=
	~sort(~findall(TestModule, current_test_module(Src, TestModule))).

create_module_wrapper(TmpDir, Module, Src, WrapperFile) :-
	open(WrapperFile, write, IO),
	unittest_print_clauses(
	    [
		( :- module(_, _, [assertions, nativeprops, unittestutils,
			    rtchecks]) ),
		(:- push_prolog_flag(unused_pred_warnings, no)),
		(:- use_module(library(unittest(unittest_props)))),
		(:- pop_prolog_flag(unused_pred_warnings)),
		(:- use_module(Src))
	    ], IO, []),
	collect_test_modules(Src, TestModules),
	nl(IO),
	unittest_print_clauses(TestModules, IO, []),
	nl(IO),
	module_test_entry(Module, TestEntry, ARef),
	unittest_print_clause((:- push_prolog_flag(single_var_warnings, off)),
	    IO, []),
	findall(Message, print_each_test_entry(TmpDir, Module, Src, IO,
		TestEntry, ARef, Message), Messages),
	unittest_print_clause((:- pop_prolog_flag(single_var_warnings)),
	    IO, []),
	close(IO),
	pretty_messages(Messages).

comp_prop_to_name(C0, C) :- C0 =.. [F, _|A], C =.. [F|A].

print_each_test_entry(TmpDir, Module, Src, IO, TestEntry, ARef, Message) :-
	if(
	    (
		current_fact(assertion_read(_Pred, Module, check, Type,
			Body, Dict, ASource, AL0, AL1), Ref),
		unittest_type(Type),
		ref_atom(Ref, ARef),
		assertion_body(Pred, Compat, Precond, Success, Comp, _, Body),
		intersection(Comp, ~valid_commands, CompComm),
		comps_to_goal(CompComm, Goal0, Goal),
		difference(Comp, ~valid_commands, CompProp),
		comps_to_goal(CompProp, Goal10, Goal2),
		( Type == texec, Goal10 \== Goal2 ->
		    functor(Pred, F, A),
		    map(CompProp, comp_prop_to_name, CompNames),
		    Message = message_lns(ASource, AL0, AL1, warning,
			['texec assertion for ', F, '/', A,
			    ' can have only unit test commands, ',
			    'not comp properties: \n', ''(CompNames),
			    '\nProcessing it as a test assertion'])
		; Message = []
		),
		current_prolog_flag(rtchecks_namefmt, NameFmt),
		Term = n(Pred, Compat, Precond, Success, Comp),
		get_pretty_names(NameFmt, Term, Dict, TermName, _Dict),
		TermName = n(PredName, _, _, Names, _),
		functor(Pred, F, A),
		functor(Head, F, A),
		current_prolog_flag(rtchecks_asrloc, UseAsrLoc),
		ALoc = asrloc(loc(ASource, AL0, AL1)),
		(
		    clause_read(Src, Head, _, _, PSource, PL0, PL1) ->
		    PLoc = loc(PSource, PL0, PL1),
		    PosLoc = [predloc(PredName, PLoc), ALoc],
		    current_prolog_flag(rtchecks_predloc, UsePredLoc),
		    UsePosLoc = (UsePredLoc, UseAsrLoc)
		;
%		    PLoc = loc(ASource, AL0, AL1),
		    UsePosLoc = (no, UseAsrLoc),
		    PosLoc = [ALoc]
		),
		( Goal10 == Goal2 -> Goal1 = Goal10
		; Goal1 = comploc_stack(PosLoc, Goal10, PredName)
		),
		Goal2 = Pred,
		(
		    Success == [] ->
		    Goal3 = Goal1
		;
		    get_prop_args(Success, Pred, Args),
		    Goal3 =
		    (
			Goal1,
			catch(rtcheck(success, PredName, Success, Args, Names,
				non_inst, [ALoc]), Ex,
			    throw(postcondition(Ex)))
		    )
		),
		( clause_read(Src, 1, rtchecked, _, _, _, _) ->
		    RTCheck = Goal3
		;
		    collect_assertions(Pred, Module, Assertions),
		    ( Assertions == [] ->
			RTCheck = Goal3
		    ;
			generate_rtchecks(Assertions, Pred, Dict, PLoc,
			    UsePosLoc, RTCheck, Goal3)
		    )
		),
		Goal = testing(ARef, TmpDir, ~list_to_lits(Precond), RTCheck),
		unittest_print_clause((TestEntry :- Goal0), IO, Dict)
	    ),
	    true,
	    (
		unittest_print_clause((TestEntry :- fail), IO, []),
		fail
	    )
	),
	Message \== [].

% show_diff(A, B) :-
% 	absolute_file_name(A, AA),
% 	absolute_file_name(B, AB),
% 	system(~atom_concat(['diff -ruN ', AA, ' ', AB]), _R).

:- export(generate_test/1).

:- meta_predicate generate_test(addterm(goal)).

generate_test(Goal, Term) :-
	functor(Term, F, N),
	Term =.. [_|Args],
	length(Vars, N),
	separate_ground_vars(Args, Vars, AssignG, AssignV),
	Pred =.. [F|Vars],
	\+ \+ (
	    call(Goal) ->
	    display_long_clause(( :- test Pred : ~list_to_lits(AssignG)
		    => ~list_to_lits(AssignV) ) + not_fails)
	;
	    display_long_clause(( :- test Pred : ~list_to_lits(AssignG)
		    + fails ))
	).

display_long_clause(Clause) :-
	pretty_print(Clause, []),
	nl.

separate_ground_vars([],         [],         [],       []).
separate_ground_vars([Arg|Args], [Var|Vars], AssignG0, AssignV0) :-
	(
	    var(Arg) ->
	    AssignG0 = AssignG,
	    AssignV0 = [Var=Arg|AssignV]
	;
	    AssignG0 = [Var=Arg|AssignG],
	    AssignV0 = AssignV
	),
	separate_ground_vars(Args, Vars, AssignG, AssignV).

:- pop_prolog_flag(write_strings).

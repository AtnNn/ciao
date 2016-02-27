:- module(make_rt, [make/1, target/1, make_option/1,
		verbose_message/1,
		verbose_message/2,
		dot_concat/2,
		call_unknown/1,
		all_values/2,
		get_value/2,
		get_value_def/3,
		get_all_values/2,
		name_value/2,
		set_name_value/2,
		cp_name_value/2,
		get_name_value/3,
		get_name_value_string/3,
		add_name_value/2,
		del_name_value/1,
		check_var_exists/1,
		find_file/2,
		vpath/1,
		add_vpath/1,
		vpath_mode/3,
		add_vpath_mode/3,
		bold_message/1,
		bold_message/2,
		normal_message/2,
		bolder_message/1,
		bolder_message/2,
		newer/2,
%                   fancy_display/1,
%% Not used any more
%%		   dyn_load_cfg_file_into_make/1,
		register_module/1,
		unregister_module/1,
		push_name_value/3,
		pop_name_value/1,
		push_active_config/1,
		pop_active_config/0,
		get_active_config/1,
		dyn_load_cfg_module_into_make/1,
		get_settings_nvalue/1],
	    [assertions, regtypes, hiord]).

:- include(library(make(make_com))).

%% ---------------------------------------------------------------------------

:- doc(title, "Predicates Available When Using The Make Package").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Edison Mera").

:- doc(usage, "This module is loaded automatically when the
   @lib{make} library package is used.").

:- doc(module, "This is the run-time module which implements the
   predicates which are provided when using the @lib{make} library
   package in a given application. For example, they are used
   internally by @apl{lpmake}.").

%% Needs fixing the English... MH
:- doc(bug, "The current handle of help messages is defficient.
   It must be in a standar form, and the user of this library only
   must be allowed to add messages, not procedures to print it.").

:- doc(bug, "target_comment/1 does not work, why? :-(.").

%% ---------------------------------------------------------------------------

%% ISO Prolog-like modules
%% :- use_module(library(compiler), [ensure_loaded/1,use_module/1]).
:- use_module(library(compiler), [use_module/1, unload/1]).

%% CIAO libraries
:- use_module(library(filenames), [file_name_extension/3]).
:- use_module(library(terms),     [atom_concat/2]).
:- use_module(library(system),    [file_exists/1]).
:- use_module(library(format),    [format/3]).
:- use_module(library(lists),     [append/3]).

:- use_module(library(messages)).

:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(library(format), [format_control/1]).
:- pop_prolog_flag(unused_pred_warnings).

:- use_module(library(lists), [list_concat/2]).
%% :- use_module(library(assertions(assrt_lib)),[set_libs/2]).
:- reexport(library(make(up_to_date))).

:- data name_value/2.
:- data vpath/1.
:- data vpath_mode/3.

:- data active_config/1.

active_config(_).

register_module(A) :- dyn_load_cfg_module_into_make(A).
unregister_module(A) :- unload(A).

add_name_value(Name, Value) :-
	data_facts:assertz_fact(name_value(Name, Value)).

del_name_value(Name) :-
	data_facts:retractall_fact(name_value(Name, _)).

set_name_value(Name, Value) :-
	data_facts:retractall_fact(name_value(Name, _)),
	data_facts:asserta_fact(name_value(Name, Value)).

push_active_config(ActiveConfig) :-
	data_facts:asserta_fact(active_config(ActiveConfig)).

pop_active_config :-
	data_facts:retract_fact(active_config(_)).

get_active_config(ActiveConfig) :-
	active_config(ActiveConfig),
	!.


:- pred push_name_value(Name, Var, R)

# "Push variable name @var{Name} with all values of variable @var{Var}
  and returns @var{R}, an abstract type to pass to
  @pred{pop_name_value/1} to undo the push changes. Push cannot be
  nested.".

push_name_value(Name, Var, R) :-
	findall(L, get_value(Var, L), Values),
	data_facts:retractall_fact(name_value(Name, _)),
	push_name_value__(Values, Name, R).


push_name_value__([],     _,    []).
push_name_value__([A|As], Name, [RA|RAs]) :-
	data_facts:assertz_fact(name_value(Name, A), RA),
	push_name_value__(As, Name, RAs).



:- pred pop_name_value(R)

# "Restores the value of the variable indicated by the abstract type
  @var{R}. Notice that @var{R} _must be_ the argument returned by
  @pred{push_name_value/2}.".

pop_name_value([]).
pop_name_value([R|Rs]) :-
	erase(R),
	pop_name_value(Rs).


:- pred cp_name_value(Source, Target)
	: (atm(Source), atm(Target))
# "Copy the variable values from @var{Source} to @var{Target}".

cp_name_value(Source, Target) :-
	data_facts:retractall_fact(name_value(Target, _)),
	(
	    get_value(Source, Value),
	    data_facts:assertz_fact(name_value(Target, Value)),
	    fail
	;
	    true
	).

add_vpath(Path) :-
	ensure_dirpath(Path, DirPath),
	( data_facts:current_fact(vpath(DirPath)) ->
	    true
	; data_facts:assertz_fact(vpath(DirPath))
	).

% Add a vpath_mode/3 entry:
%  - see apply_vpath_mode/4 for more details
%  - use map_target_base/4 for resolving bases
% note: Path can be unbound
add_vpath_mode(Suffix, Path, Mode) :-
	( data_facts:current_fact(vpath_mode(Suffix, Path, Mode)) ->
	    true
	; data_facts:assertz_fact(vpath_mode(Suffix, Path, Mode))
	).

:- regtype target(T) # "@var{T} is a Makefile target.".

target(X) :- atm(X).

verb_target_comment(Target) :-
	( make_option('-v') ->
	    show_target_comment(Target)
	; true
	).

show_target_comment(Target) :-
	( call_unknown(_:target_comment(Target)) ->
	    true
	; ( get_active_config(AC),
	    m_target_comment(AC, Target, Comment, Args) ->
	      bold_message([0'~, 0'w, 0':, 0' |Comment], [Target|Args])
	  ; true
	  )
	).

bold_message(Mess) :-
	bold_message(Mess, []).

bold_message(Mess, Args) :-
%	format( user_output,
%"*** ----------------------------------------------------------------------",
%	    [] ),
	prefix_lines(Mess, ":: ", NMess),
	format(user_output, NMess, Args),
	format(user_output, "~n",  []).
%	format( user_output,
%"*** ----------------------------------------------------------------------~n",
%	    [] ).

normal_message(Mess, Args) :-
	prefix_lines(Mess, "   ", NMess),
	format(user_output, NMess, Args),
	format(user_output, "~n",  []).

bolder_message(Mess) :-
	bolder_message(Mess, []).

bolder_message(Mess, Args) :-
% 	format(user_output,
%"*** ======================================================================",
%	[]),
	prefix_lines(Mess, "== ", NMess),
	format(user_output, NMess, Args),
	format(user_output, "~n",  []).

%	format(user_output,
%"*** ======================================================================~n",
%	    []).

% Add @var{Prefix} to each line in @var{String0}, put result in @var{String}
prefix_lines(String0, Prefix, String) :-
	append(Prefix, String1, String),
	prefix_lines_(String0, Prefix, String1).

prefix_lines_([],       _,      []).
prefix_lines_([0'\n|R], Prefix, NR) :- !,
	NR = [0'\n|NR1],
	append(Prefix, NR0, NR1),
	prefix_lines_(R, Prefix, NR0).
prefix_lines_([C|R], Prefix, [C|NR]) :- !,
	prefix_lines_(R, Prefix, NR).

:- pred make(TargetList) : list(target)
# "This is the main entry point to the make library. It makes the list
	of targets one by one as well as any intermediate targets
	needed as dictated by the dependency rules.".

make(A) :-
	dependency_list(A, R, [], Faileds, R, []),
	show_dependency_list(R, A),
	catch(list(R, make_dep),
	    make_error(Message, Args),
	    rethrow_make_error(Faileds, Message, Args)).

dependency_list(Targets, R0, ProcessedTargets, Faileds, R1, R) :-
	( dependency_list_(Targets, R0, ProcessedTargets, R1, R) -> true
	; R1 = R
	),
	findall(Failed, retract_fact(failed_target(Failed)), Faileds).

rethrow_make_error([], Message, Args) :-
	!,
	throw(make_error(Message, Args)).
rethrow_make_error(Faileds, Message, Args) :-
	throw(make_error(
		"Could not complete ~w. " ||
		"Verify that the dependent elements "||
		"exist in a known path or that "||
		"it is a valid target.  Use the -v option " ||
		"to see more detailed information.\n" || Message,
		[Faileds|Args])).

% dependency_list(A, R) :-
% 	dependency_list(A, R, [], R, []).

do_target(Target, VarSet) :-
	get_active_config(AC),
	m_do_target_atm(AC, Target, VarSet),
	!.
do_target(Target, VarSet) :-
	get_active_config(AC),
	m_do_target_var(AC, Target, VarSet).

make_dep(do_target(Target, VarSet)) :-
	show_verbose_processing(Target),
	verb_target_comment(Target),
	(
	    do_target(Target, VarSet) -> true
	;
	    throw(make_error("Failure when making target ~w", [Target]))
	).
make_dep(do_dependency(Target, TSuffix, SSuffix, FileBase)) :-
	do_show_dependency_comment(TSuffix, SSuffix, FileBase),
	(
	    get_active_config(AC),
	    m_do_dependency(AC, TSuffix, SSuffix, FileBase) -> true
	;
	    throw(make_error("Failure when making dependency ~w <- ~w~w",
		    [Target, FileBase, SSuffix]))
	).

do_show_dependency_comment(TSuffix, SSuffix, FileBase) :-
	(
	    call_unknown(_:dependency_comment(SSuffix, TSuffix, FileBase))
	-> true
	;
	    show_dependency_comment(SSuffix, TSuffix, FileBase)
	).

% ----------------------------------------------------------------------------

is_member(_, L) :-
	var(L),
	!,
	fail.
is_member(X, [Y|_]) :-
	X = Y,
	!.
is_member(X, [_|L]) :-
	is_member(X, L).

:- data failed_target/1.

dependency_list_([],               _,  _,                R,  R) :- !.
dependency_list_([Target|Targets], R0, ProcessedTargets, R1, R) :-
	!,
	dependency_list_(Target,  R0, ProcessedTargets, R1, R2),
	dependency_list_(Targets, R0, ProcessedTargets, R2, R).
dependency_list_(Target, R0, ProcessedTargets, R1, R) :-
	( is_member(Target, ProcessedTargets) ->
	    show_warning_circular_reference(Target, ProcessedTargets),
	    R1 = R
	;
	    get_active_config(AC),
	    (
		m_target_exists(AC, Target),
		dependency_target(Target, R0, ProcessedTargets, R1, R) ->
		true
	    ;
		file_name_extension(Target, FileBase, TSuffix),
		m_dependency_exists(AC, TSuffix, SSuffix),
		dependency_suffix(Target, FileBase, TSuffix,
		    SSuffix, R0, ProcessedTargets, R1, R) ->
		true
	    ;
		find_file(Target, PathTarget) ->
		show_verbose_unconditional_target_exists(PathTarget),
		R1 = R
	    ;
		(
		    m_target_exists(AC, Target) ->
		    show_verbose_dependent_target_not_exist(Target)
		;
		    file_name_extension(Target, FileBase, TSuffix),
		    m_dependency_exists(AC, TSuffix, SSuffix) ->
		    show_verbose_dependent_target_not_exist(Target)
		;
		    show_warning_target_not_exist(Target)
		),
		assertz_fact(failed_target(Target)),
		fail
	    )
	).

dependency_target(Target, R0, ProcessedTargets, R1, R) :-
	E = do_target(Target, VarSet),
	( is_member(E, R0) ->
	    show_verbose_ignoring_already_added_target(Target),
	    R1 = R
	;
	    get_active_config(AC),
	    m_target_deps(AC, Target, Deps, VarSet),
	    dependency_list_(Deps, R0, [Target|ProcessedTargets], R1, R2),
	    insert_dependency_element(Target, Deps, E, R0, R2, R),
	    ( Deps == [] -> show_verbose_unconditional_target(Target)
	    ; show_verbose_conditional_target(Target, Deps)
	    )
	).

dependency_suffix(Target, FileBase, TSuffix, SSuffix, R0, ProcessedTargets,
	    R1, R) :-
	show_verbose_checking(FileBase, TSuffix, SSuffix),
	atom_concat(FileBase, SSuffix, Dep),
	(
	    get_active_config(AC),
	    m_dependency_precond(AC, TSuffix, SSuffix, Deps0) ->
	    Deps = [Dep, Deps0]
	;
	    Deps = Dep
	),
	E = do_dependency(Target, TSuffix, SSuffix, FileBase),
	(
	    is_member(E, R0) ->
	    show_verbose_ignoring_already_added_dependency(FileBase, TSuffix,
		SSuffix),
	    R1 = R
	;
	    dependency_list_(Deps, R0, [Target|ProcessedTargets], R1, R2),
	    show_verbose_dependency(FileBase, TSuffix, SSuffix),
	    insert_dependency_element(Target, Deps, E, R0, R2, R)
	).

already_added(Dep, _) :-
	var(Dep),
	!,
	fail.
already_added([], _) :-
	!,
	fail.
already_added([Deps|_], R) :-
	already_added(Deps, R),
	!.
already_added([_|Deps], R) :-
	already_added(Deps, R),
	!.
already_added(Dep, R) :-
	is_member(do_target(Dep, _), R),
	!.
already_added(Dep, R) :-
	is_member(do_dependency(Dep, _, _, _), R),
	!.

insert_dependency_element(Target, Deps, E, R0, R1, R) :-
	(
	    nonvar(Deps),
	    newer(Deps, Target),
	    \+ already_added(Deps, R0) ->
	    show_verbose_up_to_date(Target),
	    R1 = R
	;
	    R1 = [E|R]
	).

show_dependency_list(R, A) :-
	( R \= [] -> verbose_message("Dependency list for ~w is ~w", [A, R])
	; % TODO: This cannot be an error since sometimes there are no dependencies
% 	    --jfran
% 	error_message("No rules found to build ~w.", [A])
	    verbose_message("Dependency list for ~w is empty.", [A])
	).
show_verbose_ignoring_already_added_target(Target) :-
	verbose_message("Ignoring already added target ~w", [Target]).
show_verbose_unconditional_target(Target) :-
	verbose_message("Making unconditional target ~w", [Target]).
show_verbose_conditional_target(Target, Deps) :-
	verbose_message("Adding conditional target ~w < ~w", [Target, Deps]).
show_verbose_unconditional_target_exists(PathTarget) :-
	verbose_message("Unconditional target ~w exists", [PathTarget]).
show_warning_target_not_exist(PathTarget) :-
	warning_message("Target ~w does not exist", [PathTarget]).
show_verbose_dependent_target_not_exist(PathTarget) :-
	verbose_message("Target ~w does not exist", [PathTarget]).
show_warning_circular_reference(Target, ProcessedTargets) :-
	warning_message("Ignoring circular reference ~w -> ~w",
	    [Target, ProcessedTargets]).
show_verbose_up_to_date(Target) :-
	verbose_message("~w is up to date", [Target]).
show_verbose_processing(Target) :-
	verbose_message("Processing ~w", [Target]).
show_verbose_dependency(FileBase, TSuffix, SSuffix) :-
	verbose_message("Found that ~w~w can be generated from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
show_verbose_checking(FileBase, TSuffix, SSuffix) :-
	verbose_message("Checking if ~w~w should be generated from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
%% This is what makes calls for dependency targets
show_dependency_comment(SSuffix, TSuffix, FileBase) :-
	verbose_message("Generating ~w~w from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
show_verbose_ignoring_already_added_dependency(FileBase, TSuffix, SSuffix) :-
	verbose_message("Ignoring already added dependency ~w~w from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).

%% ---------------------------------------------------------------------------
%% Procesing target dependencies
%% ---------------------------------------------------------------------------

newer(Files, Target) :-
	verbose_message("Checking if ~w is newer than ~w", [Files, Target]),
	find_file(Target, PathTarget),
	newer_(Files, PathTarget).

newer_([],           _) :- !.
newer_([File|Files], Target) :-
	!,
	newer_(File,  Target),
	newer_(Files, Target).
newer_(File, Target) :-
	find_file(File, PathFile),
	verbose_message("Checking if ~w is newer", [PathFile]),
	up_to_date(Target, PathFile).

%% ---------------------------------------------------------------------------
%% Resolve the path name for file w.r.t. the paths in vpath/1
%% ---------------------------------------------------------------------------

% TODO: Add types and document these predicates

find_file(File, PathFile) :-
	find_file_(File, _, PathFile).

find_file_(File, Path, PathFile) :-
	(Path = '' ; vpath(Path)),
	apply_path(File, Path, PathFile),
	file_exists(PathFile),
	!.

% TODO: nondet becaue of split_suffix_or_pl/3
apply_path(File, Path, PathFile) :-
% 	obtain the base (name without suffix) and suffix
	( Path = '' ->
	    split_suffix(File, BaseNoSuffix, Suffix)
	; split_suffix_or_pl(File, BaseNoSuffix, Suffix)
	),
% 	obtain the path encoding mode
	lookup_vpath_mode(Suffix, Path, Mode),
% 	apply the path to the base
	apply_vpath_mode(Mode, Path, BaseNoSuffix, PathFileNoSuffix),
	add_suffix(PathFileNoSuffix, Suffix, PathFile).

lookup_vpath_mode(Suffix, Path, Mode) :-
	( vpath_mode(Suffix, Path, Mode) ->
	    true
	; Mode = concat
	).

% Compose a path and a base using a given vpath mode
% TODO: Enrich this mapping with more modes
:- export(apply_vpath_mode/4). % TODO: temporal export (lpdoc)
apply_vpath_mode(just_name_at_cwd, _, Base, PathFile) :-
% 	Take the name as pathfile
% 	TODO: This is unsafe, replace with an absolute file name
	get_name(Base, Name),
	PathFile = Name.
apply_vpath_mode(concat, Path, Base, PathFile) :-
% 	add the path to the file
	( Path = '' ->
	    PathFile = Base
	; ensure_dirpath(Path, DirPath),
	  atom_concat(DirPath, Base, PathFile)
	).

% if there is no suffix, try appending .pl
% TODO: NONDET!
% TODO: This should be done from the side of the lpmake user, when
%       specifying the rules, not the system
split_suffix_or_pl(Path, Base, Suffix) :-
	split_suffix(Path, Base0, Suffix0),
	( Suffix0 = '', Base = Base0, Suffix = pl
	; Base = Base0, Suffix = Suffix0
	).

% TODO: Deprecated (JFMC)
% :- export(map_target_base/4).
% :- pred map_target_base(SourceBase, SourceSuffix, TargetSuffix, TargetBase) :
% 	atm * atm * atm * atm
% 
% # "Resolve @var{TargetBase} for a @var{TargetSuffix} given a
%   @var{SourceSuffix} and @var{SourceBase}".
% 
% map_target_base(SourceBase, SourceSuffix, TargetSuffix, TargetBase) :-
% 	add_suffix(SourceBase, SourceSuffix, SourceFile),
% 	find_file_(SourceFile, Path, _),
% 	lookup_vpath_mode(TargetSuffix, Path, Mode),
% 	apply_vpath_mode(Mode, Path, SourceBase, TargetBase).

%	% TODO: Just debugging
%	( TargetBase == SourceBase ->
%	    true
%	; display(user_error, rebased(Path, SourceBase, SourceSuffix, TargetSuffix, TargetBase)), nl(user_error)
%	).

%% ---------------------------------------------------------------------------
%% Support code
%% ---------------------------------------------------------------------------

:- pred dyn_load_cfg_module_into_make(ConfigFile) : sourcename

# "Used to load dynamically a module (typically, a @file{Makefile})
      into the make library from the application using the library.".

dyn_load_cfg_module_into_make(ConfigFile) :-
	use_module(ConfigFile).

:- pred make_option(Option) : atm

# "Asserting/retracting facts of this predicate sets/clears library 
      options. Default is no options (i.e., the predicate is undefined). The 
      following values are supported:
@begin{verbatim}
make_option('-v'). % Verbose: prints progress messages (useful 
                   % for debugging rules).
@end{verbatim}
  ".

:- data make_option/1.

%% Default is silent. Typically asserted by 
%% make_option('-v').

:- pred verbose_message(Text, ArgList) : format_control * list

# "The text provided in @var{Text} is printed as a message, using
     the arguments in @var{ArgList}, if @tt{make_option('-v')} is
     defined. Otherwise nothing is printed.".

:- push_prolog_flag(multi_arity_warnings, off).

verbose_message(Text) :-
	verbose_message(Text, []).

verbose_message(Mess, Args) :-
	( make_option('-v') ->
	    simple_message(Mess, Args)
	;
	    true
	).

:- pop_prolog_flag(multi_arity_warnings).

:- use_module(library(aggregates)).

% :- meta_predicate call_unknown(goal).

call_unknown(G) :-
	prolog_flag(unknown, Old,  fail),
	prolog_flag(quiet,   QOld, error),
	(
	    call(G),
	    prolog_flag(unknown, _, Old),
	    prolog_flag(quiet, _, QOld)
	; prolog_flag(unknown, _, Old),
	    prolog_flag(quiet, _, QOld),
	    fail
	).

all_pred_values(Name, Values) :-
	findall(Value,
	    (
		Pred =.. [Name, Value],
		call_unknown(_:Pred)
	    ),
	    Values).

all_name_values(Name, Values) :-
	findall(Value, name_value(Name, Value), Values).

% read all values
all_values(Name, Values) :-
	all_name_values(Name, Values0),
	(
	    Values0 == [] ->
	    all_pred_values(Name, Values)
	;
	    Values = Values0
	).

get_name_value(NameValue, Name, Value) :-
	get_name_value_string(NameValue, Name, ValueS),
	atom_codes(Value, ValueS).

get_name_value_string(NameValue, Name, ValueS) :-
	atom_codes(NameValue, NameValueS),
	list_concat([NameS, "=", ValueS], NameValueS),
	!,
	atom_codes(Name, NameS).

get_value(Name, Value) :-
	name_value(Name, _) ->
	name_value(Name, Value)
    ;
	get_pred_value(Name, Value).

get_pred_value(Name, Value) :-
	atom(Name),
	!,
	Pred =.. [Name, Value],
	call_unknown(_:Pred).
get_pred_value(Name, Value) :-
	Name =.. Flat,
	append(Flat, [Value], PredList),
	Pred =.. PredList,
	call_unknown(_:Pred).

get_value_def(Name, DefValue, Value) :-
	(get_value(Name, Value) -> true ; DefValue = Value).
% 	name_value(Name, Value) -> true
%  ;
% 	get_pred_value_def(Name, DefValue, Value).
%
% get_pred_value_def(Name, _DefValue, Value) :-
% 	Pred =.. [Name, Value],
% 	catch(call_unknown(_:Pred), _Error, false),
% 	!.
% get_pred_value_def(_PredName, DefValue, DefValue).


:- pred get_settings_nvalue(Pred)
	: term(Pred)

# "Executes @var{Pred} as unkown call, in other words, it is useful to
  execute predicates that have been loaded by @var{register_module/1}. 

  Example: get_settings_nvalue(component_options(ciao, A, B)).".

get_settings_nvalue(Pred) :-
	call_unknown(_:Pred).

:- meta_predicate check_var_exists(addmodule).

:- pred check_var_exists(Var)

# "Fails printing a message if variable @var{Var} does not exist.".

check_var_exists(Var, _) :-
	get_value(Var, _),
	!.
check_var_exists(Var, M) :-
	error_message(_, "In module ~w: Variable ~w not found", [M, Var]),
% 	display( 'Current Defined Variables:\n' ),
% 	get_value( V , VV ),
% 	displayq( V ),
% 	display( ' = ' ),
% 	displayq( VV ),nl,
	fail.

:- pred get_all_values(Name, Values)

# "@var{Values} are all the possible values of @var{Name}.".

get_all_values(Name, Value) :-
	findall(V, get_value(Name, V), Value).

% ---------------------------------------------------------------------------
% Path manipulation

:- use_module(library(lists), [reverse/2]).

:- export(get_name/2).
:- pred get_name(Path, Name)
# "@var{Name} is the file name for the path @var{Path}".

get_name(Path, Name) :-
	atom_codes(Path, String),
	reverse(String, S0),
	( append(S1, "/"||_, S0) ->
	    reverse(S1, NameCodes),
	    atom_codes(Name, NameCodes)
	; Name = Path
	).

% Split a path into base and suffix (or the path itself and '' if
% there is no suffix)
% TODO: This can be optimized (e.g. in C)

:- pred split_suffix(Path, Base, Suffix) # "The path @var{Path} is
   decomposed in @var{Base} and suffix @var{Suffix}".

split_suffix(PathAtm, BaseAtm, SuffixAtm) :-
	atom_codes(PathAtm, Path),
	reverse(Path, RPath),
	( append(RSuffix, "."||RBase, RPath) ->
	    reverse(RSuffix, Suffix),
	    reverse(RBase,   Base),
	    atom_codes(SuffixAtm, Suffix),
	    atom_codes(BaseAtm, Base)
	; SuffixAtm = '', BaseAtm = PathAtm
	).

add_suffix(PathNoSuffix, '', Path) :- !,
	Path = PathNoSuffix.
add_suffix(PathNoSuffix, Suffix, Path) :-
	atom_concat([PathNoSuffix, '.', Suffix], Path).

ensure_dirpath(Path, DirPath) :-
	( atom_concat(_, '/', Path) ->
	    DirPath = Path
	; atom_concat(Path, '/', DirPath)
	).

dot_concat(A, A) :- A == '', !.
dot_concat(A, A) :- atom_codes(A, [0'.|_]), !.
dot_concat(T, TSuffix) :- atom_concat('.', T, TSuffix).

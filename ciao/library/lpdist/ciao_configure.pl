:- module(ciao_configure, [], [ciaopaths, assertions, make, dcg, fsyntax, hiord]).

:- doc(title, "The Configuration Process").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module reads the configuration options from
   @tt{ciao_config_options} and selects values (automatic or
   interactively) for the options. The values are stored and saved
   through the @tt{ciao_config_db} module.

   The configuration process may invoke external configuration tools
   if required, including the @tt{config-sysdep/config-and-dump.sh}
   script.").

:- use_module(library(hiordlib), [map/3]).
:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(system_extra)).
:- use_module(library(make(make_rt))).
:- use_module(library(messages)).

:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(ciao_config_options))).
:- use_module(library(lpdist(ciao_config_db))).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

% Be careful with get_vers and get_patch: when uninstalling, the patch
% version may differ with the version that we try to uninstall.

% ----------------------------------------------------------------------------

:- export(silentconfig/0).
:- pred silentconfig # "Do a silent configuration (show no messages)".
silentconfig :- get_name_value('SILENT', 'true'), !.

% TODO: document, this seems to be the level for *interactive* configuration
configlevel('1', default).
configlevel('2', minimum).
configlevel('3', extended).

% ---------------------------------------------------------------------------

% The full configuration is stored in different formats, for different
% tools:
%
%   - internal format (saved, no sysdep):
%       ~ciao_build_dir/CONFIG_saved  
%
%   - makefile format, includes sysdep:
%       ~ciao_build_dir/CONFIG_mkf
%
%   - SH format, includes sysdep:
%       ~ciao_build_dir/CONFIG_sh
%
%   - input for sysdep configuration script (a projection of
%     CONFIG_saved in sh format):
%       ~ciao_build_dir/CONFIG_input

:- export(configure/0).
:- pred configure # "Detect configuration options automatically.
   If @tt{INTERACTIVE_CONFIG} is specified, the process is interactive.".

configure :-
	% First part: infer environment settings, obtain user
	% preferences, and save the configuration.
	check_name_values_are_ok, % (for user prefs)
	infer_config_values, % (can be interactive)
	save_config_file, % (save CONFIG_save)
	% Second part: deduce system dependant options
	config_sysdep_and_dump,
	%
	bold_message("Configuration complete").

% Precondition: the configuration values are loaded
config_sysdep_and_dump :-
	CfgInput = ~fsR(sys_dir(build)/'CONFIG_input'),
	export_config_as_sh(CfgInput),
	do(['cd ', ~fsR(bundle_src(ciao)), ' && ',
	    './config-sysdep/config-and-dump.sh', ' ', CfgInput],
	   []).

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(aggregates), [findall/3]).

export_config_as_sh(FileName) :-
	string_to_file(~list_concat([
	    "# Warning: This file has been created automatically\n\n",
	    ~options_to_sh_string]), FileName).

options_to_sh_string(String) :-
	findall(Line, ( current_config_value(Name, Value),
		atom_codes(Name,  NameS),
		atom_codes(Value, ValueS),
		list_concat([NameS, "=\"", ValueS, "\"\n"], Line) ), L),
	list_concat(L, String).

% ---------------------------------------------------------------------------
% Verify that user provided options corresponds to configuration
% settings (ciao_config_entry/3)

check_name_values_are_ok :-
	% TODO: this code looks ugly, fix
	findall(N,
	        (ciao_config_entry(_,Ps), member(option_name(N),Ps)),
		OtherNames),
	%
	( name_value(Name, _),
	    ( member(Name, OtherNames) ->
	        true
	    ; normalize(Name, NameN),
	      ciao_config_entry(NameN, _Properties) ->
		true
	    ; show_message(warning, "Invalid configure option: --~w", [Name])
	    ),
	    fail
	; true
	).

normalize(Name, NameN) :-
	atom_codes(Name, NameC),
	map(NameC, normalizecode, NameNC),
	atom_codes(NameN, NameNC).

normalizecode --> touppercode, normunderscore.

normunderscore(0'-, 0'_) :- !.
normunderscore(C,   C).

verify_dep((Name, Value), MainName) :-
	( current_config_value(Name, Value0) ->
	    true
	; ( var(Value) ->
	      show_message(warning,
	        "~w Undefined because ~w is not defined yet",
	        [MainName, Name])
	  ; true
	  ),
	  fail
	),
	Value = Value0.

% TODO: rename... move to ciao_config_db?
:- export(get_name_value/2).
% TODO: This seems to be used only for options from lpmake (make_rt).
%       Normalization is made in the wrong way. It could be deterministic.
get_name_value(Name0, Value) :-
	normalize(Name0, NameN),
	name_value(Name, Value), % TODO: from make_rt?
	% TODO: nondeterministic?!
%	display(user_error, foo(Name, NameN)), nl(user_error),
	normalize(Name, NameN).
%	display(user_error, bar(Name, NameN, Value)), nl(user_error).

% TODO: rename... move to ciao_config_db?
:- export(toupper/2).
toupper(Name, Upper) :-
	atom_codes(Name, NameC),
	map(NameC, touppercode, UpperC),
	atom_codes(Upper, UpperC).

touppercode(C, U) :-
	0'a =< C,
	C =< 0'z,
	!,
	U is C + (0'A - 0'a).
touppercode(C, C).

% TODO: rename... move to ciao_config_db?
:- export(tolower/2).
tolower(Name, Lower) :-
	atom_codes(Name, NameC),
	map(NameC, tolowercode, LowerC),
	atom_codes(Lower, LowerC).

tolowercode(C, U) :-
	0'A =< C,
	C =< 0'Z,
	!,
	U is C - 0'A + 0'a.
tolowercode(C, C).

hidden_opt := 'INTERACTIVE_CONFIG'.

% Display the configuration options
% (this predicate is necessary to generate makedir/config_opts.txt,
%  which is used from 'ciaosetup')
:- export(show_config_opts/0).
show_config_opts :-
	show_config_predefined_opts,
	show_config_custom_opts.

show_config_predefined_opts :-
	display_string(
"List of configuration options:

--help          Show this help.

--menu          Configure options via a menu (recommended).

").

show_config_custom_opts :-
	( ciao_config_entry(Name, Properties),
	    \+ hidden_opt(Name),
	    show_config_opt(Name, Properties),
	    fail
	; true
	).

show_config_opt(Name, Properties) :-
	( member(option_name(Lower), Properties) -> true
	; tolower(Name, Lower)
	),
	member(query(Help, _ConfigLevels), Properties),
	display_list(['--', Lower, '=', Name, '\n\n']),
	display_string(Help),
	nl,
	nl,
	( member(valid_values(ValidValues), Properties) ->
	    display_list(['Valid values are ', ValidValues, '\n\n'])
	; true
	),
	nl.

get_config_level(ConfigLevel) :-
	current_config_value('INTERACTIVE_CONFIG', NConfigLevel),
	!,
	configlevel(NConfigLevel, ConfigLevel).
get_config_level(default).

% ===========================================================================

% Determine the values for the configuration entries
% (automatically or via user interaction, depending on settings
% INTERACTIVE_CONFIG)
infer_config_values :-
	( % (failure-driven loop)
	  ciao_config_entry(Name, Properties),
	    infer_config_value(Name, Properties),
	    fail
	; true
	).

% Infer the value for one configuration variable @var{Name}
infer_config_value(Name, Properties) :-
	% Check dependencies
	( member(depend_on(DepList), Properties) ->
	    list(DepList, verify_dep(Name))
	; true
	),
	( member(valid_values(ValidValues), Properties) ->
	    true
	; true
	),
	( get_name_value(Name, Value) ->
	    ( silentconfig ->
		true
	    ; display_list(['   {predefined value for \'', Name,
			'\' is \'', Value, '\'}\n'])
	    )
	; member(option_name(OptName), Properties),
	  get_name_value(OptName, Value) ->
	    % option name as specified in the command line
	    true
	; member(set_value(Value), Properties) ->
	    true
	; member(set_value(ValMethod, Value), Properties),
	  call_in_config_options_mod(ValMethod) ->
	    true
	; ( \+ member(noprevious, Properties),
	      current_config_value(Name, DefaultValue) ->
	      true
	  ; member(default(DefaultValue), Properties) ->
	      true
	  ; member(default(DefMethod, DefaultValue), Properties) ->
	      call_in_config_options_mod(DefMethod),
	      ( member(show_option(Message), Properties) ->
		  display_option(Message, DefaultValue)
	      ; member(show_option_with_help(Message, MessageNo), Properties) ->
		  display_option_with_help(Message, DefaultValue, MessageNo)
	      ; true
	      )
	  ; true
	  ),
	  ( ( member(query(Help, ConfigLevels), Properties),
	      get_config_level(ConfigLevel),
	      member(ConfigLevel, ConfigLevels)
	    ) ->
	      % ask for a value, interactively with the user
	      query_value(Help, Name, ValidValues, DefaultValue, Value),
	      ( DefaultValue \== Value ->
		  clean_dependent_values(Name)
	      ; true
	      )
	  ; % chose default value
	    Value = DefaultValue
	  )
	),
	( member(Value, ValidValues) ->
	    clean_dependent_values(Name)
	; display_list(
              ['Error: invalid value \'', Value, '\' for ',
	       Name, '. Valid values are: ', ValidValues]),
	  halt(1)
	),
	( member(show(ShowHelp, ShowConfigLevels), Properties),
	  ( current_config_value('INTERACTIVE_CONFIG', NShowConfigLevel) ->
	      configlevel(NShowConfigLevel, ShowConfigLevel)
	  ; fail
	  ),
	  member(ShowConfigLevel, ShowConfigLevels) ->
	    % This shows the value for the configuration option
	    % TODO: add a flag to disable it?
	    option_name(ShowHelp, Name, ShowName),
	    display_option(ShowName, Value)
	; true
	),
	( member(nosave, Properties) ->
	    true
	; ground(Value) ->
	    set_config_value(Name, Value)
	; atom_codes(Name, NameS),
	    warning_message("Undefined value for "|| NameS)
	),
	!.

clean_dependent_values(Name) :-
	( % (failure-driven loop)
	  ciao_config_entry(DependentName, Properties),
	    ( member(depend_on(DepList), Properties),
	      member((Name, _),          DepList) ->
	        clean_dependent_values(DependentName),
	        retractall(current_config_value(DependentName, _))
	    ; true
	    ),
	    fail
	; true
	).

repeated_display(_,    0) :- !.
repeated_display(Term, Times) :-
	Times > 0,
	Times2 is Times - 1,
	display(Term),
	repeated_display(Term, Times2).

option_name(ShowHelp, Name, ShowName) :-
	(ShowHelp == "" -> atom_codes(Name, ShowName) ; ShowName = ShowHelp).

display_option(ShowName, Value) :-
	length(ShowName, N),
	N2 is 30 - N,
	(N2 < 0 -> N3 is 0 ; N3 is N2),
	display('   '),
	display_string(ShowName),
	display(': '),
	repeated_display('.', N3),
	display_list([' ', Value, '\n']).

display_option_with_help(NameMsg, Value, HelpMsg) :-
	display_option(NameMsg, Value),
	( Value = yes ->
	    true
	; warning($$(HelpMsg))
	).

% ---------------------------------------------------------------------------

:- doc(section, "Input operations for user interaction").

% (exported)
query_value(Help, Name, ValidValues, DefaultValue, Value) :-
	( query_value_(Help, Name, DefaultValue, Value),
	    member(Value, ValidValues) ->
	    true
	; warning_message(
		"Value not allowed for this menu option, try again\n"),
	    display_list(['Valid values are ', ValidValues, '\n\n']),
	    query_value(Help, Name, ValidValues, DefaultValue, Value)
	).

query_value_(Help, Name, DefaultValue, Value) :-
	nl,
	display_string(Help),
	nl,
	display_list(['\n', Name, '=[', DefaultValue, '] ? ']),
	get_atom(Value1),
	( Value1 = '' ->
	    Value = DefaultValue
	; Value = Value1
	).

% (exported?)
get_atom(Atom) :-
	current_input(S),
	get_atom_(S, Atom).

get_atom_(Stream, Atom) :-
	get_string(Stream, String),
	atom_codes(Atom, String).

get_string(Stream, String) :-
	get_code(Stream, Code),
	( "\n" = [Code] ->
	    String = []
	; String = [Code|String2],
	  get_string(Stream, String2)
	).


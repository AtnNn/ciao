:- module(_, _, [ciaopaths, assertions, make, dcg, fsyntax, hiord]).

:- use_module(library(hiordlib), [map/3]).
:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(make(system_extra))).
:- use_module(library(make(make_rt))).
:- use_module(library(component_registry), [component_src/2]).
:- use_module(library(persvalue)).
:- use_module(library(messages)).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(ciaodesrc(makedir(makedir_component))).

% TODO: Redesign the code for the menus
% TODO: This predicate is called though settings_set_value, via set_value
:- use_module(ciaodesrc(makedir('ConfigValues')), [build_doc_dir/1]).

:- reexport(ciaodesrc(makedir('DOCCOMMON'))).

:- doc(author, "Edison Mera").

% Be careful with get_vers and get_patch: when uninstalling, the patch
% version may differ with the version that we try to uninstall.

%:- initialization(ciaoinitvals).

% ----------------------------------------------------------------------------
% Utilities that are used only with SETTINGS and SHARED files:
% ----------------------------------------------------------------------------

configlevel('1', default).
configlevel('2', minimum).
configlevel('3', extended).

autoconfig :-
	get_name_value('AUTOCONFIG', '1'),
	!.

silentconfig :-
	get_name_value('SILENT', 'true'),
	!.

% ciaoinitvals :-
% 	load_file(ciaosrcsettings, ~settings_auto).

% ---------------------------------------------------------------------------

menuconfig :-
	( \+ silentconfig, autoconfig ->
	    bold_message("Doing automatic configuration")
	; true
	),
	do_config,
	bold_message("Configuration complete"),
	( \+ silentconfig ->
	    normal_message("Configuration settings saved in file '~w'.",
		['ciao/SETTINGS_AUTO']),
	    show_what_is_next
	; true
	).

show_what_is_next :-
	normal_message(
"Please check that all the values and messages above are correct. If
not, you can change or customize the configuration using the command
line or --menu configure option.

To continue, execute:
   \"./ciaosetup build\"        to compile, then
   \"./ciaosetup docs\"         to generate the documentation,
                              (if not in distribution), then
   \"./ciaosetup install\"      to install the system.", []),
	!.

% ---------------------------------------------------------------------------

do_config :-
	check_name_values_are_ok,
	config_values,
	save_group(ciaosrcsettings),
	copy_file(~settings_auto, ~atom_concat(~component_src(ciao),
		'/SETTINGS'), [overwrite]).

% ---------------------------------------------------------------------------

check_name_values_are_ok :-
	( name_value(Name, _),
	    ( normalize(Name, NameN),
		config_entry(_Group, NameN, _Properties) ->
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

verify_dep((Name, Value), Group, MainName) :-
	( current_value(Group, Name, Value0) -> true
	;
	    ( var(Value) ->
		show_message(warning,
		    "~w Undefined because ~w is not defined yet", [MainName,
			Name])
	    ; true
	    ),
	    fail
	),
	Value = Value0.

config_values :-
	( config_entry(Group, Name, Properties),
	    config_value(Group, Name, Properties),
	    fail
	; true
	).

get_name_value(Name0, Value) :-
	normalize(Name0, NameN),
	name_value(Name, Value),
	normalize(Name, NameN).

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

black_list_opts := 'CONFIGLEVEL'.

ciaosetup_opts :-
	ciaosetup_fixed_opts,
	ciaosetup_var_opts.

ciaosetup_fixed_opts :-
	display_string(
"List of configuration options:

--help          Show this help.

--menu          Configure options via a menu (recommended).

--reset         Erase any previous configuration values.

--preserve      Preserve previous configuration values.

--default       Configure using default values.

").

ciaosetup_var_opts :-
	( config_entry(_Group, Name, Properties),
	    \+ black_list_opts(Name),
	    ciaosetup_opt(Name, Properties),
	    fail
	; true
	).

ciaosetup_opt(Name, Properties) :-
	tolower(Name, Lower),
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

get_config_level(Group, ConfigLevel) :-
	current_value(Group, 'CONFIGLEVEL', NConfigLevel),
	!,
	configlevel(NConfigLevel, ConfigLevel).
get_config_level(_, default).

config_value(Group, Name, Properties) :-
	( member(depend_of(DepList), Properties) ->
	    list(DepList, verify_dep(Group, Name))
	; true
	),
	(member(valid_values(ValidValues), Properties) -> true ; true),
	( get_name_value(Name, Value) ->
	    ( silentconfig ->
		true
	    ; display_list(['   {predefined value for \'', Name,
			'\' is \'', Value, '\'}\n'])
	    )
	; member(set_value(Value), Properties) ->
	    true
	; member(set_value(ValMethod, Value), Properties),
	    settings_set_value(ValMethod) ->
	    true
	; ( \+ member(noprevious, Properties),
		current_value(Group, Name, DefaultValue) ->
		true
	    ; member(default(DefaultValue), Properties) ->
		true
	    ; member(default(DefMethod, DefaultValue), Properties) ->
		settings_set_value(DefMethod)
	    ; true
	    ),
	    ( member(query(Help, ConfigLevels), Properties),
		get_config_level(Group, ConfigLevel),
		\+ autoconfig,
		member(ConfigLevel, ConfigLevels) ->
		query_value(Help, Name, ValidValues, DefaultValue, Value),
		( DefaultValue \== Value ->
		    clean_dependent_values(Group, Name)
		; true
		)
	    ; Value = DefaultValue
	    )
	),
	( member(Value, ValidValues) ->
	    clean_dependent_values(Group, Name)
	; display_list(['Error: invalid value \'', Value, '\' for ',
		    Name, '. Valid values are: ', ValidValues]),
	    halt(1)
	),
	( member(show(ShowHelp, ShowConfigLevels), Properties),
	    ( current_value(Group, 'CONFIGLEVEL', NShowConfigLevel) ->
		configlevel(NShowConfigLevel, ShowConfigLevel)
	    ; fail
	    ),
	    member(ShowConfigLevel, ShowConfigLevels) ->
	    ( fail, silentconfig ->
		true
	    ; option_name(ShowHelp, Name, ShowName),
		display_option(ShowName, Value)
	    )
	; true
	),
	( member(nosave, Properties) ->
	    true
	; ground(Value) ->
	    set_value(Group, Name, Value)
	; atom_codes(Name, NameS),
	    warning_message("Undefined value for "|| NameS)
	),
	!.

clean_dependent_values(Group, Name) :-
	(
	    config_entry(Group, DependentName, Properties),
	    (
		member(depend_of(DepList), Properties),
		member((Name, _),          DepList) ->
		clean_dependent_values(Group, DependentName),
		retractall(current_value(Group, DependentName, _))
	    ;
		true
	    ),
	    fail
	;
	    true
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

% ---------------------------------------------------------------------------

:- doc(section, "Input operations for user interaction").

% (exported)
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


% Ciao configuration value database (from ~ciao_build_dir/CONFIG_saved)
% TODO: Split this file in two? (doc definitions and core)
:- module(ciao_config_db, [], [ciaopaths, assertions, dcg, fsyntax]).

:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(library(system)).

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

% ===========================================================================

:- doc(section, "Store (persistently) Configuration Variables").
% Based on code originally written by Edison Mera
% author: Jose F. Morales

:- use_module(library(file_utils)).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(system)).
%:- use_module(library(dynamic)).
:- use_module(library(strings)).

:- export(current_config_value/2).
:- data current_config_value/2.

:- export(saved_config_file/1).
saved_config_file := ~fsR(sys_dir(build)/'CONFIG_saved').

% Restore the saved configuration (if the file exists)
:- initialization(restore_ciao_config).

% ===========================================================================
% Load the configuration
% TODO: Simplify, just read terms...

:- export(restore_ciao_config/0).
restore_ciao_config :-
	retractall_fact(current_config_value(_, _)),
	config_read_loop.

% Restore all variables from the saved configuration file
config_read_loop :-
	( % (failure-driven loop)
	  read_config_values(NameS, ValueS),
	    atom_codes(Name,  NameS),
	    atom_codes(Value, ValueS),
	    assertz_fact(current_config_value(Name, Value)),
	    fail
	;
	    true
	).

read_config_values(Name, Value) :-
	FileName = ~saved_config_file,
	( file_exists(FileName) ->
	    file_to_string(FileName, String)
	; String = ""
	),
	get_value_from_string(String, Name, Value).

get_value_from_string(String, Name, Value) :-
	get_value_from_string2(Name, Value, String, _Tail).

get_value_from_string2(Name, Value) -->
	"\n",
	!,
	get_value_from_string2(Name, Value).
get_value_from_string2(Name, Value) -->
	"#",
	any_text(_Comment),
	!,
	get_value_from_string2(Name, Value).
%
get_value_from_string2(Name, Value) -->
	equality(Name1, Value1),
	!,
	(
	    {Name = Name1},
	    {Value = Value1}
	;
	    get_value_from_string2(Name, Value)
	).

any_text([C|Cs]) --> [C], {C \== 0'\n}, any_text(Cs).
any_text([]) --> "".

char_name(C) --> [C], {C \== 0'=}.

name([C|Cs]) --> char_name(C), name(Cs).
name([C]) --> char_name(C).

equality(Name, Value) -->
	name(Name),
	"=",
	any_text(Value).

% ===========================================================================
% Save the configuration
% TODO: Simplify, just save terms...

% Save ~ciao_build_dir/CONFIG_saved
% (the file has been loaded from ciao_config_db)
% TODO: We can use any format for CONFIG_saved now
%       (it is only read from Prolog)
:- export(save_config_file/0).
save_config_file :-
	FileName = ~saved_config_file,
	string_to_file(~list_concat([
		    "# -*- mode: Makefile; -*-\n",
		    "# Warning: This file has been created automatically\n\n",
		    ~config_values_string]), FileName).

config_values_string(String) :-
	findall(Line, ( current_config_value(Name, Value),
		atom_codes(Name,  NameS),
		atom_codes(Value, ValueS),
		list_concat([NameS, "=", ValueS, "\n"], Line) ), L),
	list_concat(L, String).

% ===========================================================================

% Write a configuration value
:- export(set_config_value/2).
set_config_value(Name, Value) :-
	retractall_fact(current_config_value(Name, _)),
	assertz_fact(current_config_value(Name, Value)).

% Read a configuration value, emit error message if not found
:- export(get_config_val/2).
% TODO: Throw exception if the value is not found...
:- meta_predicate get_config_val(?, addmodule).
get_config_val(Name, Value, _) :-
	current_config_value(Name, Value0),
	!,
	Value = Value0.
get_config_val(Name, _Value, Module) :-
	message(error, ['In module ', Module,
		', Could not find enviroment variable ',
		Name, ' defined in ', ~saved_config_file]),
	fail.


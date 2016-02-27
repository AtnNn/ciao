:- module(autodoc_settings, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(title,"Access to Default Settings").
:- doc(author,"Jose F. Morales").

:- doc(module, "
	This module defines the setting values with some default values.

@begin{alert}   
@bf{Note: This part needs better documentation. -- JFMC}
@end{alert}
   ").

% ===========================================================================

:- doc(section, "Checking or Setting Options").

:- use_module(library(make(system_extra))).
:- use_module(library(make(make_rt))).

:- export(lpdoc_option/1).
:- doc(lpdoc_option/1, "Defines the global options of lpdoc.").
:- data lpdoc_option/1.
lpdoc_option('-ncv').

:- export(check_setting/1).
check_setting(Name) :- check_var_exists(Name).

% (With implicit default value)
:- export(setting_value_or_default/2).
:- pred setting_value_or_default(Var, Value)
# "Returns in @var{Value} the value of the variable @var{Var}. In case
  this variable does not exists, it returns a default value. If there
  is no default value for the variable @var{Var} it fails.".

setting_value_or_default(Name, Value) :-
	( make_rt:get_value(Name, Value0) ->
	    Value = Value0
	; Value = ~default_val(Name)
	).

default_val(startpage) := 1.
default_val(papertype) := afourpaper.
default_val(perms) := perm(rwX, rX, rX).
default_val(owner) := ~get_pwnam.
default_val(group) := G :- ( G = ~get_grnam -> true ; G = 'unknown' ).

% (With explicit default value)
:- export(setting_value_or_default/3).
setting_value_or_default(Name, DefValue, Value) :-
	( make_rt:get_value(Name, Value0) ->
	    Value = Value0
	; Value = DefValue
	).

:- export(setting_value/2).
setting_value(Name, Value) :-
	make_rt:get_value(Name, Value).

:- export(all_setting_values/2).
%all_setting_values(Name) := ~findall(T, ~setting_value(doc_mainopt)).
all_setting_values(X) := ~findall(T, setting_value(X, T)) :-
	( X = doc_mainopts ; X = doc_compopts ), !. % TODO: all_values fail if empty?!
all_setting_values(Name) := ~all_values(Name).

:- use_module(library(aggregates)).

:- export(get_command_option/1).
% TODO: Document?
get_command_option([C]) :-
	make_rt:get_value(stop_if_error, V),
	stop_command_option(V, C),
	!.
get_command_option([nofail, silent, show_error_on_error|A]) :-
	( current_fact(make_option('-v')) ->
	    A = [verbose]
	; A = []
	).
%get_command_option := exception.

:- use_module(library(distutils), [stop_command_option/2]).

:- export(requested_file_formats/1).
:- pred requested_file_formats(F) # "@var{F} is a requested file format".
requested_file_formats := F :-
	F = ~all_values(docformat).

% ===========================================================================

:- doc(section, "Paths to Code").

:- export(load_vpaths/0).
load_vpaths :-
	load_filepath,
	load_systempath.

load_filepath :-
	( setting_value(filepath, P),
	  add_vpath(P),
	  verbose_message("Added file path: ~w", [P]),
	  fail
	; true
	).

load_systempath :-
	( setting_value(systempath, P),
	  add_vpath(P),
	  verbose_message("Added system path: ~w", [P]),
	  fail
	; true
	).


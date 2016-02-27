% ===========================================================================
% Common definitions and main/1 entry point for an application using a
% command line interface, lpmake package and SETTINGS.pl
%
% Author: Jose F. Morales based on code from 'lpdoc'
%
% ===========================================================================
%
% The user must include this file in its module and and define the
% following entry points:
%
%   startup/1
%   app_name/1
%   app_copyright/1
%   app_options_message/1
%
% Command line options may be handled with:
%   is_option0/1, handle_option0/1
%   is_option1/1, handle_option1/2
%
% ---------------------------------------------------------------------------
% TODO:
%  - Use real (language supported) interfaces / inheritance to
%    implement this.
%  - Use the more general command line package, or integrate with it.
%  - Merge documentation definitions with lpdoc comments (e.g. extract
%    comments).
%  - Locate SETTINGS.pl in the directory tree (in the style of GIT).
%  - Compare 'lpmake' command line interface with this one.
%  - Make more explicit what are the module dependencies that this code need.
% ---------------------------------------------------------------------------

%% This one is for testing from the top level.
% main(Args) :-
% 	activate_trace,
% 	profile(main_(Args)).

main(Args) :-
	startup(Args).

startup(Args) :-
	catch(handle_args(Args), E, (my_handle_error(E), fail)).

my_handle_error(error(system_error(E, Goal))) :- !,
	error_message("Execution of ~w returned ~w", [Goal, E]).
my_handle_error(error(Error)) :- !,
	error_message("Unknown error: ~w", [Error]).
my_handle_error(error(Error, Where)) :- !,
	handle_error(Error, Where).
my_handle_error(make_error(Format, Args)) :- !,
	error_message(Format, Args).
my_handle_error(Error) :-
	error_message("Unknown error: ~w", [Error]).

% ---------------------------------------------------------------------------
% Parse command line arguments

handle_args([X]) :- help_option(X), !, report_usage.
handle_args([X]) :- version_option(X), !, report_version.
handle_args(Args) :-
	handle_args_(Args, Targets), !,
	load_settings,
	start(Targets).
handle_args(Args) :-
	format(user_error, "~nIllegal arguments: ~w~n~n", [Args]),
	report_usage.

handle_args_([Opt|Args], Targets) :-
	is_option0(Opt),
	!,
	handle_option0(Opt),
	handle_args_(Args, Targets).
handle_args_([Opt, Arg|Args], Targets) :-
	is_option1(Opt),
	!,
	handle_option1(Opt, Arg),
	handle_args_(Args, Targets).
handle_args_(Args, Targets) :- !,
	Targets = Args.

% Options with 0 arguments
:- discontiguous(is_option0/1).
:- discontiguous(handle_option0/1).
% Options with 1 argument
:- discontiguous(is_option1/1).
:- discontiguous(handle_option1/2).

help_option('-h').
help_option('--help').

version_option('-v').
version_option('--version').

% ---------------------------------------------------------------------------

is_option1('-d').
handle_option1('-d', NameValue) :-
	get_name_value(NameValue, Name, Value),
	add_name_value(Name, Value).

is_option0('-v').
handle_option0('-v') :-
	assertz_fact(make_option('-v')).

is_option1('-m').
handle_option1('-m', File) :- load_settings_file(File).
%
is_option1('-l').
handle_option1('-l', File) :- load_settings_file(library(File)).
%        use_module(library(LibraryFile)),

is_option1('-f').
handle_option1('-f', CfgFile) :-
	retractall_fact(settings_file(_)),
	assertz_fact(settings_file(CfgFile)).
%        use_module(library(LibraryFile)),

:- data settings_file/1.
settings_file('SETTINGS').

% ---------------------------------------------------------------------------

load_settings_file(Spec) :-
	verbose_message("loading additional settings file ~w", [Spec]),
	% Check whether the file exists
	use_module(Spec),
	dyn_load_cfg_module_into_make(Spec).

% ---------------------------------------------------------------------------

load_settings :-
	settings_file(ConfigFile),
	get_abs_path_no_check(ConfigFile, AbsFilePath),
	(
	    ( file_exists(AbsFilePath)
	    ; atom_concat(AbsFilePath, '.pl', TryThisFile),
	      file_exists(TryThisFile)
	    )
	->
	    verbose_message("Using configuration file ~w", [AbsFilePath]),
	    dyn_load_cfg_module_into_make(AbsFilePath)
	;
	    working_directory(CWD0, CWD0),
	    path_name(CWD0, CWD),
	    add_name_value(filepath, CWD),
	    add_name_value('$schema', 'SETTINGS_schema'), % assume that we have a valid schema
	    verbose_message("No configuration file. Setting filepath to ~w", [CWD])
	),
	( findall(T, make_rt:get_value(pathsfile, T), [ThePathAliasFile]) ->
	    use_module(library(ThePathAliasFile)),
	    dyn_load_cfg_module_into_make(library(ThePathAliasFile))
	;
	    true
	),
	!.
load_settings :-
	settings_file(ConfigFile),
	throw(make_error("settings file ~w does not exist", [ConfigFile])).

% ---------------------------------------------------------------------------
% Help messages

% This is in narrow format because that way it looks nicer in a man page.
common_options_message("
Help options:

-h,--help       Print this help message.
-v,--version    Print version

General (lpmake) options:

-m FILE         Load additional settings file FILE

-l FILE         Load additional settings file library(FILE)

-f FILE         Uses file FILE in the current directory as
                configuration file. Default is SETTINGS.pl.

-d Name=Value   Indicates that a variable binding Name=Value follows.
                If Name is one of the options used, then this will
                override the value defined in the configuration file.
                To get the value of this option in your program, simply
                call the 'name_value(Name, Value).'  (i.e., 'name_value(Name)
                := Value.') predicate, defined in the module
                library(make(make_rt)). 
").

% ---------------------------------------------------------------------------

report_version :-
	version(Version),
	app_name(AppName),
	format(user_error, "~w ~w~n", [AppName, Version]),
	app_copyright(Copyright),
	format(user_error, "~s~n", [Copyright]).

report_usage :-
	report_version,
	report_usage_text,
	report_commands.

report_usage_text :-
	% TODO: Why not using lpdoc itself to format this?
	format(user_error, "~nUsage:~n", []),
	app_name(AppName),
	common_options_message(Text2),
	app_options_message(Text3),
	format(user_error, "~w [OPTIONS] COMMANDS~n", [AppName]),
	format(user_error, "~nProcesses each command using the specified options.~n~n", []),
	format(user_error, Text2, []),
	format(user_error, Text3, []).

report_commands :-
	format(user_error, "~nSupported commands:~n", []),
	findall(tc(Target, Comment),
	    target_comment_action(Target, Comment),
	    TCs),
	( member(tc(Target, Comment), TCs),
	    format(user_error, "    ~w~n    ~s~n~n", [Target, Comment]),
	    fail
	; true
	).

target_comment_action(Target, SComment) :-
	m_target_comment(_AC, Target, Comment, Args),
	sformat(SComment, Comment, Args).

% TODO: This should be consulted in the documentation, not here!
%% report_index_names :-
%% 	format(user_error, "~nAcceptable index names:~n", []),
%% 	( index_comment(Index, IText),
%% 	    format(user_error, "    ~w~n    ~s~n~n", [Index, IText]),
%% 	    fail
%% 	; true ).
%% 
%% report_additional_options :-
%% 	format(user_error, "~nAdditional options (MiscOpts):~n", []),
%% 	( option_comment(Option, OText),
%% 	    format(user_error, "    ~w~n    ~s~n~n", [Option, OText]),
%% 	    fail
%% 	; true
%% 	).


:- module(distutils, [],
	    [
		runtime_ops, % required to allow reading of operators
		assertions,
		argnames,
		dcg,
		make,
		fsyntax,
		regtypes,
		basicmodes,
		hiord
	    ]).

:- use_module(library(hiordlib)).
:- use_module(library(make(system_extra))).
:- use_module(library(sort)).
:- use_module(library(aggregates)).
:- use_module(library(write), []).
:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(library(make(make_rt))).
:- use_module(library(format)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [length/2, append/3, reverse/2]).
:- use_module(library(compiler(exemaker)), [make_exec/2]).
:- use_module(library(distutils(skip_settings))).
:- use_module(library(compiler(c_itf_internal)), [cleanup_itf_cache/0]).

:- use_module(library(distutils(dirutils))).

:- reexport(library(distutils(collect_modules))).

:- doc(title, "Distribution and Installation Utilities").

:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

% ---------------------------------------------------------------------------

:- export(stop_command_option/2).
% TODO: Document and rename if necessary
stop_command_option(yes, halt).
stop_command_option(no,  nofail).

% ----------------------------------------------------------------------------

% TODO: move to makedir_aux?
:- export(enum_distpkg_codeitem_contents/6).
enum_distpkg_codeitem_contents(PackageType, BaseDir, SkipSubDir, Dir, File, FileName) :-
	current_filter_files(BaseDir, '*', ~skip_dist_files_bak(PackageType),
	    ~skip_dirs, SkipSubDir, ~nodist_dirs, Dir, File, FileName).

% TODO: Unused
%% :- export(list_filter_files_rec_dest/8).
%% list_filter_files_rec_dest(BaseDir, DestDir, Pattern, SkipFiles, SkipDirs,
%% 	    SkipSubDirs, NDFiles, DestList) :-
%% 	list_filter_files_rec(BaseDir, Pattern, SkipFiles, SkipDirs,
%% 	    SkipSubDirs, NDFiles, BaseList),
%% 	add_preffix(List, BaseDir, BaseList),
%% 	add_preffix(List, DestDir, DestList).

% ---------------------------------------------------------------------------

% TODO: move to makedir_aux?
:- export(list_filter_files_rec/7).
list_filter_files_rec(BaseDir0, Pattern, SkipFiles, SkipDirs, SkipSubDirs,
	    NDFiles, Result) :-
	path_name(BaseDir0, BaseDir),
	findall(FileName, current_filter_files(BaseDir, Pattern, SkipFiles,
		SkipDirs, SkipSubDirs, NDFiles, _, _, FileName), Result).

% ===========================================================================

:- doc(section, "Registering Code in Scripts").

% TODO: We could want a special Key in the register action, so that
%       different tools can register themselves? Or we can just add a
%       reference to some Ciao code and register code there.

:- export(register_in_script/3).
:- pred register_in_script(File, CommentMark, Code)

   # "Inserts @var{Code} in the script file @var{File}. Code is
     surrounded by special comments (starting with @var{CommentMark}),
     that allow identifying the inserted code later. If the @var{File}
     contained some inserted code, it is replaced.".

register_in_script(File, CommentMark, Code) :-
	( file_exists(File) ->
	    file_to_string(File, String)
	; String = []
	),
	( script_with_marked_code(CommentMark, Key0, Before0, _, After0, String) ->
	    Key = Key0, Before = Before0, After = After0
	; Key = ~unique_key_string, Before = String, After = []
	),
	script_with_marked_code(CommentMark, Key, Before, Code, After, NewString),
	backup_and_write(NewString, File).

% A 'unique' key string
% TODO: We do nothing to ensure that it is unique
unique_key_string := Value :-
	Min = 10000000,
	Max = 99999999,
	time(Time),
	Value0 is Min + (Time mod (Max - Min + 1)),
	Value = ~number_codes(Value0).

:- export(unregister_from_script/2).
unregister_from_script(File, CommentMark) :-
	( file_exists(File) ->
	    file_to_string(File, String),
	    ( script_with_marked_code(CommentMark, _Key, Before, _, After, String) ->
	        NewString = ~append(Before, After),
	        backup_and_write(NewString, File)
	    ; true
	    )
	; true
	).

% (decompose S in Before+SurroundedCode+After)
script_with_marked_code(CommentMark, Key, Before, Code, After, S) :-
	emit_string(Before, S, S0),
	( Before = [] -> FirstLine = yes ; FirstLine = no ),
	surrounded_code(FirstLine, CommentMark, Key, Code, S0, After),
	!.

% Usage:
%   (unbound string: emit)
%   (bound string: extract Key and Code)
surrounded_code(FirstLine, CommentMark, Key, Code) -->
	begin_mark(FirstLine, CommentMark, Key),
	emit_string(Code),
	end_mark(CommentMark, Key).

begin_mark(FirstLine, CommentMark, Key) -->
	% @tt{\\n} is not necessary in the first line
	( { FirstLine = yes } -> [] ; "\n" ),
	emit_string(CommentMark), " @begin(", emit_key(Key),
	")@ - Do not edit these lines - added automatically!\n".

end_mark(CommentMark, Key) -->
	emit_string(CommentMark), " @end(", emit_key(Key),
	")@ - End of automatically added lines.\n".

% (this can either recognize or emit a key)
emit_key("") --> "".
emit_key([D|H]) --> digit(D), emit_key(H).

digit(A, [A|S], S) :- A >= 0'0, A =< 0'9.

emit_string(Word, S, T) :-
	append(Word, T, S).

% ----------------------------------------------------------------------------

backup_and_write(String, FileName) :-
	backup_file(FileName),
	string_to_file(String, FileName).

% TODO: This predicate could be useful; move to a more generic library?
% :- export(backup_file/1).
:- pred backup_file(FileName) # "Save a backup copy of file @var{FileName}".
backup_file(FileName) :-
	( file_exists(FileName) ->
	    get_backup_filename(FileName, I, B),
	    ( I > 0,
	      I0 is I - 1,
	      compose_backup_filename(FileName, I0, B0),
	      file_to_string(FileName, FileNameS),
	      file_to_string(B0, BS),
	      BS = FileNameS ->
		true
	    ; --delete_file(B),
	      copy_file(FileName, B)
	    )
	; true
	).

% TODO: Simplify?
get_backup_filename(FileName, I, B) :-
	get_backup_filename_(FileName, 0, I, B).

get_backup_filename_(FileName, I, I, B) :-
	compose_backup_filename(FileName, I, B),
	\+ file_exists(B),
	!.
get_backup_filename_(FileName, I0, I, B) :-
	I1 is I0 + 1,
	get_backup_filename_(FileName, I1, I, B).

compose_backup_filename(FileName, I, B) :-
	atom_number(IA, I),
	atom_concat([FileName, '.bak~', IA, '~'], B).

% ===========================================================================

:- doc(section, "Operations on Source Trees").

:- export(autolibs/2).
:- meta_predicate autolibs(?, list(pred(1))).
autolibs(BaseDir0, Options) :-
	path_name(BaseDir0, BaseDir),
	bold_message("Compiling ~w libraries", [BaseDir]),
	compile_collected_modules(BaseDir, Options).

:- meta_predicate compile_collected_modules(?, list(pred(1))).
compile_collected_modules(BaseDir, Options) :-
	compile_modules(BaseDir, current_module_to_compile(BaseDir), Options).

current_module_to_compile(Dir, BaseDir, File, FileName) :-
	current_dir_module(BaseDir, Dir, File, FileName).

:- export(compile_modules/3).
:- meta_predicate compile_modules(?, pred(3), list(pred(1))).
compile_modules(BaseDir, Collector, Options) :-
	findall(m(Dir, File, FileName),
	    Collector(Dir, File, FileName), ModulesU),
	sort(ModulesU, Modules),
	show_duplicates(Modules),
	compile_module_list(Modules, BaseDir, Options).

show_duplicates(Modules) :-
	sort_by_file(Modules, Modules2),
	dump_duplicates(Modules2).

% TODO: This can be improved!
dump_duplicates(Modules) :-
	dump_duplicates_(Modules, [_|Modules]).

dump_duplicates_([], _).
dump_duplicates_([m(PrevFile, _, PrevFileName)|PrevModules],
	    [m(File, _, FileName)|Modules]) :-
	( File == PrevFile -> show_message(error,
		"Module ~w already defined in ~w", [FileName, PrevFileName])
	; true ),
	dump_duplicates_(PrevModules, Modules).

sort_by_file(Modules, Modules2) :-
	sort_by_file_(Modules, Modules1),
	sort(Modules1, Modules2).

sort_by_file_([], []).
sort_by_file_([m(Dir, File, FileName)|Modules],
	    [m(File, Dir, FileName)|Modules2]) :-
	sort_by_file_(Modules, Modules2).

:- export(compile_module_list/3).
:- meta_predicate compile_module_list(?, ?, list(pred(1))).
compile_module_list(Modules, BaseDir, Options) :-
	tty(UsingTTY),
	working_directory(OrigDir, OrigDir),
	(BaseDir == '' -> true ; cd(BaseDir)),
	compile_mods(Modules, Options, BaseDir, UsingTTY),
	cd(OrigDir).

:- meta_predicate compile_mods(?, list(pred(1)), ?, ?).
compile_mods(Modules, Options, BaseDir, UsingTTY) :-
	length(Modules, N),
	map(Modules, compile_mod(Options, BaseDir, UsingTTY, N), 1, _),
	format(user_error, "~w   Compiled ~w modules\n", [~newline_code(UsingTTY), N]).

display_compiling_msg(using_tty, I, N) :-
	format(user_error, "\r   Compiling ~w/~w ", [I, N]).
display_compiling_msg(no_tty, _, _) :-
	format(user_error, "\n   Compiling ", []).

:- meta_predicate compile_mod(?, list(pred(1)), ?, ?, ?, ?, ?).
compile_mod(m(_, _, FileName), Options, BaseDirP, UsingTTY, N, I, I1) :-
	atom_concat(BaseDirP, File, FileName),
	display_compiling_msg(UsingTTY, I, N),
	format(user_error, "~w ", [File]),
	list(Options, apply_pred(File)),
	cleanup_itf_cache,
	I1 is I + 1.

:- meta_predicate apply_pred(pred(1), ?).
apply_pred(Option, FileName) :-
	Option(FileName).

% ---------------------------------------------------------------------------

:- export(build_executable/3).
:- pred build_executable(AliasName, Version, TargetDir) # "Build an
   executable from the source file @var{AliasName} in the target
   directory @var{TargetDir}, and creates a link to it.  The name of
   the executable is obtained adding to the base name of the source
   file the character - and the version @var{Version}.  The link name
   is the source file base name.".

build_executable(AliasName, Version, TargetDir) :-
	absolute_file_name(AliasName, '_opt', '.pl', '.', FileName, FileBase, AbsDir),
	atom_concat(AbsDir, PBase, FileBase),
	get_ciao_ext(Ext),
	atom_concat([TargetDir, PBase, '-', Version, Ext], FileExec),
	atom_concat('/', Base, PBase),
	atom_concat([Base, '-', Version, Ext], BaseExec),
	make_exec([FileName], FileExec),
	-copy_file(BaseExec, ~atom_concat(TargetDir, PBase),
	    [overwrite, symlink]).

% ---------------------------------------------------------------------------
% TODO: move to makedir_aux?
% TODO: Simplify!

:- export(component_make/2).
% Perform lpmake command Cmd on component Component
component_make(Component, Cmd) :-
	( lpmake_subdir(Component, ~atom_concat(makedir_part_, Component), Cmd) ->
	    true
	; fail
	).

:- export(lpmake_subdir/3).
lpmake_subdir(Dir, ConfigModule, Action) :-
	atom_concat(['makedir/', ConfigModule, '.pl'], ConfigFile),
	lpmake_subdir_(Dir, [ConfigFile], ConfigModule, Action).

lpmake_subdir_(Dir, ConfigFiles, ConfigModule, Action) :-
	working_directory(PWD, PWD),
	cd(Dir),
	list(ConfigFiles, register_module),
	push_active_config(ConfigModule),
	make(Action),
	list(~reverse(ConfigFiles), unregister_module),
	pop_active_config,
	cd(PWD).

% ---------------------------------------------------------------------------

% TODO: move to makedir_aux?
% TODO: WRONG NAMES; CommandAction should be Options... and Action is also an option...
:- export(make_subdir/5).
make_subdir(Cmd, Dir, Params, Action, CommandAction) :-
	make_subdir_(Cmd, Dir, Params, Action, action(CommandAction)).

% ---------------------------------------------------------------------------

% TODO: move to makedir_aux?
% TODO: Only for 'ant' in ./ciaopp/makedir/makedir_part_ciaopp.pl
:- export(make_subdir/7).
make_subdir(Cmd, Dir, Params, Action, CommandAction, OutputFile, ErrorFile) :-
	make_subdir_(Cmd, Dir, Params, Action, action_output(CommandAction, OutputFile, ErrorFile)).

make_subdir_(Cmd, Dir, PreParams, Action, CommandOptions) :-
	working_directory(PWD, PWD),
	shorten_string(30, "...", ~atom_codes(PWD), PWDDisplay),
	shorten_string(30, "...", ~atom_codes(Dir), DirDisplay),
	format(":: Entering `~s' (from `~s')\n", [DirDisplay, PWDDisplay]),
	atom_concat([PreParams, ' ', Cmd, ' ', Action], Command),
	%
	format("   Executing `~w'\n", [Command]),
	do_command(CommandOptions, Dir, Command),
	%
	format(":: Leaving  `~s'\n", [DirDisplay]).

do_command(action(CommandAction), Dir, Command) :-
	do(['cd ', Dir, ' && ', Command], CommandAction).
do_command(action_output(CommandAction, OutputFile, ErrorFile), Dir, Command) :-
	do(['cd ', Dir, ' && ', Command], OutputFile, ErrorFile, CommandAction).

shorten_string(MaxLength, Preffix, Input, Output) :-
	length(Input,   N),
	length(Preffix, P),
	( N > MaxLength + P ->
	    Cut is N - MaxLength,
	    cutstring(Cut, Input, Output1),
	    append(Preffix, Output1, Output)
	;
	    Output = Input
	).

cutstring(0, S,         S).
cutstring(N, [_|Input], Output) :-
	N2 is N - 1,
	cutstring(N2, Input, Output).

% ===========================================================================

:- doc(section, "Console Display Tools").

% TODO: may not be very portable
% The output is a terminal (not a file)
using_tty :-
	popen('(stty > /dev/null) 2>&1', read, S),
	stream_to_string(S, "").

tty(X) :-
	( using_tty ->
	    X = using_tty
	; X = no_tty
	).

newline_code(using_tty, '\r').
newline_code(no_tty,    '\n').


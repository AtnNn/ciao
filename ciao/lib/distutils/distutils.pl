:- module(distutils,
	    [
		delete_files_and_dirs/1, % TODO: move to system_extra?
		delete_dir_rec/1, % TODO: move to system_extra?
		docsclean/1, % TODO: move to lpdoc? merge! 
		enum_distpkg_codeitem_contents/6, % TODO: move to some distpkg module?
		list_filter_files_rec/7,
		list_filter_files_rec_dest/8,
		lpmake_subdir/3,
		lpmake_subdir/4,
		doc_subdir/4,
		make_subdir/6,
		make_subdir/8,
		make_subdirs/6,
		make_subdirs/8,
		copy_dir_rec/2,
		copy_dir_rec/3,
		copy_dir_rec/4,
		copy_dir_rec/8,
		copy_dir_rec/9,
		do_str/3,
		do_str_without_nl/3,
		usersrc/1,
		backup/1,
		secure_write/2,
		pwd/1,
		compile_collected_modules/2,
		compile_modules/3,
		compile_module_list/3,
		autolibs/2,
		configure_script/3,
		uninstall_script/2,
		start_message/4,
		end_message/4,
		string_to_term/2,
		strings_to_terms/2,
		return_code/2,
		using_tty/0,
		set_owner_rec/2,
		path_name/2,

		build_executable/3,

		atomdelim/3,
		atomdelim_list/3,

		stop_command_option/2,

		copy_files_or_dirs/2,
		copy_files_or_dirs/3,
		copy_file_or_dir/3,
		build_root/1,
% 		regtypes:
		distpkg_codeitem_type/1
	    ],
	    [
		runtime_ops, % required to allow reading of operators
		assertions,
		argnames,
		dcg,
		make,
		fsyntax,
		regtypes,
		basicmodes,
		hiord,
		regexp
	    ]).



:- reexport(library(distutils(find))).
:- reexport(library(distutils(shell_string))).
:- reexport(library(distutils(listfiles))).

:- use_module(library(hiordlib)).
:- use_module(library(distutils(setperms))).
:- use_module(library(make(system_extra))).
:- use_module(library(sort)).
:- use_module(library(aggregates)).
:- use_module(library(read)).
:- use_module(library(write), []).
:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(library(make(make_rt))).
:- use_module(library(format)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [length/2, append/3, reverse/2]).
:- use_module(library(compiler(exemaker)), [make_exec/2]).
:- use_module(library(distutils(skip_settings))).
:- use_module(library(autoconfig)).
:- use_module(library(compiler(c_itf_internal)), [cleanup_itf_cache/0]).

:- reexport(library(distutils(configutils))).
:- reexport(library(distutils(collect_modules))).

:- doc(author, "Edison Mera").
:- doc(module, "Distribution and installation utilities.").

string_to_term(String, Term) :-
	pipe(ReadFrom, WriteTo),
	format(WriteTo, "~s.", [String]),
	close(WriteTo),
	read_term(ReadFrom, Term, []),
	close(ReadFrom).

strings_to_terms(Ss, Ts) :-
	map(Ss, string_to_term, Ts).

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

stop_command_option(yes, halt).
stop_command_option(no,  nofail).

%pwd := ~current_env('PWD').

pwd(A) :- working_directory(A, A).

doc_subdir(Dir, ConfigFiles, ConfigModule, Action) :-
	( get_value(build_root, _) ->
	    true
	;
	    working_directory(CWD, CWD),
	    atom_concat([CWD, '/', Dir, '/'], ADir),
	    add_name_value(build_root, ADir)
	),
	add_name_value(lpdoclib, ~atom_concat([~component_src(lpdoc),
		    '/lib/'])),
	lpmake_subdir(Dir, ConfigFiles, ConfigModule, Action),
	del_name_value(build_root).

lpmake_subdir(Dir, ConfigModule, Action) :-
	atom_concat(['makedir/', ConfigModule, '.pl'], ConfigFile),
	lpmake_subdir(Dir, [ConfigFile], ConfigModule, Action).

lpmake_subdir(Dir, ConfigFiles, ConfigModule, Action) :-
	pwd(PWD),
	cd(Dir),
	list(ConfigFiles, register_module),
	push_active_config(ConfigModule),
	make(Action),
	list(~reverse(ConfigFiles), unregister_module),
	pop_active_config,
	cd(PWD).

make_subdir_(MakeFrom, MakeTo, Dir, PreParams, Action, CommandOptions) :-
	pwd(PWD),
	shorten_string(30, "...", ~atom_codes(PWD), PWDDisplay),
	(
	    MakeFrom = [_, MakeFromDisplay] -> true
	;
	    MakeFromDisplay = MakeFrom
	),
	(
	    MakeTo = [_, MakeToDisplay] ->
	    atom_concat(MakeTo, MakeToFile)
	;
	    MakeToFile = MakeTo,
	    MakeToDisplay = MakeTo
	),
	shorten_string(30, "...", ~atom_codes(Dir), DirDisplay),
	format("~w -> ~w: Entering `~s/~s`\n", [MakeFromDisplay,
		MakeToDisplay, PWDDisplay, DirDisplay]),
	atom_concat([PreParams, ' ', MakeToFile, ' ', Action], Command),
	command_subdir(Dir, Command, CommandOptions),
	format("~w <- ~w: Leaving  `~s/~s`\n", [MakeFromDisplay,
		MakeToDisplay, PWDDisplay, DirDisplay]).

make_subdir(MakeFrom, MakeTo, Dir, Params, Action, CommandAction) :-
	make_subdir_(MakeFrom, MakeTo, Dir, Params, Action,
	    action(CommandAction)).

make_subdir(MakeFrom, MakeTo, Dir, Params, Action, CommandAction, OutputFile,
	    ErrorFile) :-
	make_subdir_(MakeFrom, MakeTo, Dir, Params, Action,
	    action_output(CommandAction, OutputFile, ErrorFile)).

make_subdirs(Dirs, From, To, PreParams, Action, CommandAction) :-
	make_subdirs_(Dirs, From, To, PreParams, Action,
	    action(CommandAction)).

make_subdirs(Dirs, From, To, PreParams, Action, CommandAction, OutputFile,
	    ErrorFile) :-
	make_subdirs_(Dirs, From, To, PreParams, Action,
	    action_output(CommandAction, OutputFile, ErrorFile)).

make_subdirs_([],         _MakeFrom, _MakeTo, _PreParams, _Action,
	    _CommandOptions).
make_subdirs_([Dir|Dirs], MakeFrom,  MakeTo,  PreParams,  Action,
	    CommandOptions) :-
	make_subdir_(MakeFrom, MakeTo, Dir, PreParams, Action,
	    CommandOptions),
	make_subdirs_(Dirs, MakeFrom, MakeTo, PreParams, Action,
	    CommandOptions).

command_subdir(Dir, Command, CommandOptions) :-
	display(~atom_concat([Dir, ': Executing `', Command, '`\n'])),
	do_command(CommandOptions, Dir, Command).

do_command(action(CommandAction), Dir, Command) :-
	do(['cd ', Dir, ' && ', Command], CommandAction).
do_command(action_output(CommandAction, OutputFile, ErrorFile), Dir,
	    Command) :-
	do(['cd ', Dir, ' && ', Command], OutputFile, ErrorFile,
	    CommandAction).

:- push_prolog_flag(multi_arity_warnings, off).

:- pred path_name(A, B) : atm * atm

# "@var{B} is @var{A} concatenating '/' if needed. ".

path_name(Dir, Dir) :-
	atom_concat(_, '/', Dir),
	!.
path_name(Dir, DirName) :-
%	display_list(['{WARNING: specified ',Dir, ' without ending /}\n']),
	atom_concat(Dir, '/', DirName).

/*
list_all_files_rec(BaseDir0, Type, BaseList) :-
	path_name(BaseDir0, BaseDir),
	findall(FileName,
	    current_find(BaseDir, dir_before_dummy_, dir_after_dummy_,
		Type, _Dir, _File, FileName),
	    BaseList).

list_all_files_rec(BaseDir0, DestDir0, Type, DestList) :-
	path_name(BaseDir0, BaseDir),
	path_name(DestDir0, DestDir),
	findall(DestFile,
	    (
		current_find(BaseDir, dir_before_dummy_, dir_after_dummy_,
		    Type, _Dir, _File, FileName),
		atom_concat(BaseDir, BaseName, FileName),
		atom_concat(DestDir, BaseName, DestFile)
	    ),
	    DestList).
*/

:- regtype distpkg_codeitem_type/1 # "The types of files that contains
code in a @index{distribution package}.".
distpkg_codeitem_type(src). % Source files
distpkg_codeitem_type(noa). % Platform independent binary files: Not Architecture
distpkg_codeitem_type(bin). % Binary files, including platform dependent files
distpkg_codeitem_type(raw). % Almost all files, minimal number of ignored files

enum_distpkg_codeitem_contents(PackageType, BaseDir, SkipSubDir, Dir, File, FileName) :-
	current_filter_files(BaseDir, '*', ~skip_dist_files_bak(PackageType),
	    ~skip_dirs, SkipSubDir, ~nodist_dirs, Dir, File, FileName).

list_filter_files_rec_dest(BaseDir, DestDir, Pattern, SkipFiles, SkipDirs,
	    SkipSubDirs, NDFiles, DestList) :-
	list_filter_files_rec(BaseDir, Pattern, SkipFiles, SkipDirs,
	    SkipSubDirs, NDFiles, BaseList),
	add_preffix(List, BaseDir, BaseList),
	add_preffix(List, DestDir, DestList).

list_filter_files_rec(BaseDir0, Pattern, SkipFiles, SkipDirs, SkipSubDirs,
	    NDFiles, Result) :-
	path_name(BaseDir0, BaseDir),
	findall(FileName, current_filter_files(BaseDir, Pattern, SkipFiles,
		SkipDirs, SkipSubDirs, NDFiles, _, _, FileName), Result).

% ----------------------------------------------------------------------------

copy_files_or_dirs(FilesOrDirs, Dir) :-
	copy_files_or_dirs(FilesOrDirs, Dir, [overwrite, timestamp]).

copy_files_or_dirs([],           _DestDir, _CopyOptions).
copy_files_or_dirs([File|Files], DestDir,  CopyOptions) :-
	copy_file_or_dir(File, DestDir, CopyOptions),
	copy_files_or_dirs(Files, DestDir, CopyOptions).

copy_file_or_dir(FileOrDir, DestDir, CopyOptions) :-
	is_dir(FileOrDir),
	!,
	dir_path(DestDir, T0),
	file_dir_name(FileOrDir, _Dir, Name),
	atom_concat(T0, Name, T1),
	file_property(FileOrDir, mode(Mode)),
	make_dirpath(T1, Mode),
	copy_dir_rec(FileOrDir, T1, CopyOptions).
copy_file_or_dir(File, DestDir, CopyOptions) :-
	copy_file(File, DestDir, CopyOptions).

copy_dir_rec(BaseDir0, DestDir0, Pattern, SkipFiles, SkipDirs,
	    SkipSubDirs, NDFiles, CopyOptions) :-
	copy_dir_rec(BaseDir0, DestDir0, _, Pattern,
	    SkipFiles, SkipDirs, SkipSubDirs, NDFiles, CopyOptions).

copy_dir_rec(BaseDir0, DestDir0, Perms, Pattern, SkipFiles, SkipDirs,
	    SkipSubDirs, NDFiles, CopyOptions) :-
	path_name(BaseDir0, BaseDir),
	path_name(DestDir0, DestDir),
	mkdir_perm(DestDir, Perms),
	find(BaseDir, copy_file_(BaseDir, DestDir, Pattern, SkipFiles,
		CopyOptions, Perms), copy_dir_condition_(BaseDir,
		SkipDirs, SkipSubDirs, NDFiles, DestDir, Perms),
	    dir_after_dummy_).

copy_file_(CurrBaseDir, BaseDir, DestDir, Pattern, SkipFiles, CopyOptions,
	    Perms, File) :-
	(
	    match_pred(SkipFiles, File) ->
	    fail
	;
	    match_pred(Pattern, File) ->
	    atom_concat(BaseDir,       RelDir, CurrBaseDir),
	    atom_concat(DestDir,       RelDir, CurrTargetDir),
	    atom_concat(CurrBaseDir,   File,   FileName),
	    atom_concat(CurrTargetDir, File,   CurrTargetFile),
	    -copy_file(FileName, CurrTargetFile, CopyOptions),
	    ( nonvar(Perms) ->
		- set_perms(FileName, Perms)
	    ;
		true
	    )
	;
	    true
	),
	!,
% 	fail to avoid the File be added to the list
	fail.

copy_dir_condition_(CurrBaseDir, BaseDir, SkipDirs, SkipSubDirs,
	    NDFiles, DestDir, Perms, Dir) :-
	is_distributable_dir(CurrBaseDir, BaseDir, SkipDirs, SkipSubDirs,
	    NDFiles, Dir),
	atom_concat(BaseDir,       RelDir, CurrBaseDir),
	atom_concat(DestDir,       RelDir, TargetBaseDir),
	atom_concat(TargetBaseDir, Dir,    TargetDir),
	mkdir_perm(TargetDir, Perms).

copy_dir_rec(BaseDir, DestDir) :-
	copy_dir_rec(BaseDir, DestDir, [overwrite, timestamp]).

copy_dir_rec(BaseDir, DestDir, Perms, CopyOptions) :-
	copy_dir_rec(BaseDir, DestDir, Perms, '*', '', '', '', [],
	    CopyOptions).

copy_dir_rec(BaseDir, DestDir, CopyOptions) :-
	copy_dir_rec(BaseDir, DestDir, '*', '', '', '', [], CopyOptions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Script manipulation predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_backup_filename(FileName, I, B) :-
	get_backup_filename(FileName, 0, I, B).

compose_backup_filename(FileName, I, B) :-
	atom_number(IA, I),
	atom_concat([FileName, '.bak~', IA, '~'], B).

get_backup_filename(FileName, I, I, B) :-
	compose_backup_filename(FileName, I, B),
	\+ file_exists(B),
	!.
get_backup_filename(FileName, I0, I, B) :-
	I1 is I0 + 1,
	get_backup_filename(FileName, I1, I, B).

backup(FileName) :-
	(
	    file_exists(FileName) ->
	    get_backup_filename(FileName, I, B),
	    (
		I > 0,
		I0 is I - 1,
		compose_backup_filename(FileName, I0, B0),
		file_to_string(FileName, FileNameS),
		file_to_string(B0,       BS),
		BS = FileNameS ->
		true
	    ;
		--delete_file(B),
		copy_file(FileName, B)
	    )
	;
	    true
	).
secure_write(FileName, T) :-
	backup(FileName),
	string_to_file(T, FileName).

uninstall_script(File, Comment) :-
	file_exists(File) ->
	file_to_string(File, String, Tail),
	( Tail = [],
	    script_undo(Comment, _Key, NewString, After, _, _, After, [],
		String, []) ->
	    secure_write(File, NewString)
	; true
	)
    ;
	true.

% ----------------------------------------------------------------------------
script_undo(Comment, Key, Before, BeforeTail, Bash, BashTail, After,
	    AfterTail) -->
	any_string(Before, BeforeTail),
	start_message(Comment, Key),
	any_string(Bash, BashTail),
	end_message(Comment, Key),
	any_string(After, AfterTail).

% ----------------------------------------------------------------------------
any_string(BTail, BTail) --> "".
any_string([Char|Head], BTail, [Char|Chars], Tail) :-
	any_string(Head, BTail, Chars, Tail).

genkey(Min, Max, Value) :-
	time(Time),
	Value is Min + (Time mod (Max - Min + 1)).

:- meta_predicate configure_script(?, ?, pred(3)).

configure_script(File, Comment, PredLines) :-
	( file_exists(File) ->
	    file_to_string(File, String, Tail)
	; String = Tail
	),
	(
	    Tail = [],
	    script_undo(Comment, Key, NewString, BTail, _, _, After, [],
		String, []) ->
	    PredLines(Key, BTail, After)
	;
	    PredLines(~number_codes(~genkey(10000000, 99999999)), Tail, []),
	    NewString = String
	),
	secure_write(File, NewString).

this_word(Word, S, T) :-
	append(Word, T, S).

start_message(Comment, Key) -->
	"\n", this_word(Comment), " @begin(", key_message(Key),
	")@ - Do not edit these lines - added automatically!\n".

end_message(Comment, Key) -->
	this_word(Comment), " @end(", key_message(Key),
	")@ - End of automatically added lines.\n".

key_message("") --> "".
key_message([D|H]) --> digit(D), key_message(H).

digit(0'0) --> "0".
digit(0'1) --> "1".
digit(0'2) --> "2".
digit(0'3) --> "3".
digit(0'4) --> "4".
digit(0'5) --> "5".
digit(0'6) --> "6".
digit(0'7) --> "7".
digit(0'8) --> "8".
digit(0'9) --> "9".

usersrc(UserSrc) :-
	current_env('USERSRC', UserSrc).

% ----------------------------------------------------------------------------

:- pop_prolog_flag(multi_arity_warnings).

% --- Delete routines

:- doc(delete_dir_rec(Directory), "Deletes the file or directory
   @var{Directory} and all the files and subdirectories
   recursively.").

delete_dir_rec(Dir) :-
	is_dir(Dir) ->
	path_name(Dir, Dir1),
	find(Dir1, find_delete_file_, dir_before_dummy_,
	    find_delete_directory_),
	-delete_directory(Dir)
    ;
	file_exists(Dir) ->
	delete_file(Dir)
    ;
	true.

find_delete_file_(BaseDir, File) :-
	-delete_file(~atom_concat(BaseDir, File)),
	fail.

find_delete_directory_(BaseDir, File) :-
	-delete_directory(~atom_concat(BaseDir, File)).

using_tty :-
	popen('(stty > /dev/null) 2>&1', read, S),
	stream_to_string(S, "").

return_code(using_tty, '\r').
return_code(no_tty,    '\n').

tty(using_tty) :- using_tty, !.
tty(no_tty).

show_duplicates(Modules) :-
	sort_by_file(Modules, Modules2),
	dump_duplicates(Modules2).

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

:- meta_predicate compile_modules(?, pred(3), list(pred(1))).
compile_modules(BaseDir, Collector, Options) :-
	findall(m(Dir, File, FileName),
	    Collector(Dir, File, FileName), ModulesU),
	sort(ModulesU, Modules),
	show_duplicates(Modules),
	compile_module_list(Modules, BaseDir, Options).

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
	format(user_error, "~w   Compiled ~w modules\n",
	    [~return_code(UsingTTY), N]).

display_compiling_msg(using_tty, I, N) :-
	format(user_error, "\r   Compiling ~w/~w ", [I, N]).
display_compiling_msg(no_tty, _, _) :-
	format(user_error, "\n   Compiling ", []).


:- meta_predicate apply_pred(pred(1), ?).

apply_pred(Option, FileName) :-
	Option(FileName).

:- meta_predicate compile_mods_(?, list(pred(1)), ?, ?, ?, ?).

compile_mod(m(_, _, FileName), Options, BaseDirP, UsingTTY, N, I, I1) :-
	atom_concat(BaseDirP, File, FileName),
	display_compiling_msg(UsingTTY, I, N),
	format(user_error, "~w ", [File]),
	list(Options, apply_pred(File)),
	cleanup_itf_cache,
	I1 is I + 1.

% Delete auto generated docs by lpdoc
docsclean(MainFile) :-
	specific_docclean(html,       MainFile),
	specific_docclean(texi,       MainFile),
	specific_docclean(dvi,        MainFile),
	specific_docclean(ps,         MainFile),
	specific_docclean(pdf,        MainFile),
	specific_docclean(txt,        MainFile),
	specific_docclean(ascii,      MainFile),
	specific_docclean(txt,        MainFile),
	specific_docclean(htmlindex,  MainFile),
	specific_docclean(htmlbullet, MainFile),
	specific_docclean(manl,       MainFile),
	specific_docclean(info,       MainFile),
	additional_docclean(MainFile).

additional_docclean(MainFile) :-
	specific_docclean(htmlsumm, MainFile),
	specific_docclean(l,        MainFile),
	del_files_nofail(~ls(~atom_concat([MainFile,
			'autofig*.ppm|autofig*.jpg|']))).

specific_docclean(html, MainFile) :-
	clean_html_dir(MainFile),
	!.
specific_docclean(info, MainFile) :-
	del_files_nofail(~ls(~atom_concat([MainFile, '*.info*']))),
	!.
specific_docclean(DocFormat, MainFile) :-
	del_files_nofail(~ls(~atom_concat([MainFile, '*.', DocFormat]))),
	!.

clean_html_dir(MainFile) :-
	atom_concat([MainFile, '.html/'], HtmlDir),
	delete_dir_rec(HtmlDir).

atomdelim(AD, D, A) :-
	atom_concat([A0, D, ADs], AD) ->
	(
	    A = A0
	;
	    atomdelim(ADs, D, A)
	)
    ;
	A = AD.

atomdelim_list(A, D, [L|Ls]) :-
	atom_concat([L, D, As], A),
	!,
	atomdelim_list(As, D, Ls).
atomdelim_list(A, _, [A]).

:- pred delete_files_and_dirs(FilesOrDirs)
	: list(FilesOrDirs, atm)

# "Delete Files and/or Direcories.".

delete_files_and_dirs(Dirs) :- list(Dirs, delete_file_or_dir).

:- pred delete_file_or_dir/1 : atm.
delete_file_or_dir(Dir) :-
	( is_dir(Dir) ->
	    delete_dir_rec(Dir)
	;
	    delete_file(Dir)
	).

:- pred build_executable(AliasName, Version, TargetDir) # "Build an
   executable from the source file @var{AliasName} in the target
   directory @var{TargetDir}, and creates a link to it.  The name of
   the executable is obtained adding to the base name of the source
   file the character - and the version @var{Version}.  The link name
   is the source file base name.".

build_executable(AliasName, Version, TargetDir) :-
	absolute_file_name(AliasName, '_opt', '.pl', '.', FileName, FileBase,
	    AbsDir),
	atom_concat(AbsDir, PBase, FileBase),
	get_ciao_ext(Ext),
	atom_concat([TargetDir, PBase, '-', Version, Ext], FileExec),
	atom_concat('/', Base, PBase),
	atom_concat([Base, '-', Version, Ext], BaseExec),
	make_exec([FileName], FileExec),
	-copy_file(BaseExec, ~atom_concat(TargetDir, PBase),
	    [overwrite, symlink]).

:- pred build_root(BuildRoot) # "Unifies @var{BuildRoot} with the
   value of the environment variable BUILD_ROOT, which is used for
   package generators to do a fake installation in the specified
   directory.".

build_root(R) :-
	( current_env('BUILD_ROOT', S) -> true
	; S = ''
	),
	S = R.

% TODO: Do in other way...
do_str(Command, Fail, String) :-
	do(~append(Command, [' > ciaostr.tmp']), Fail),
	file_exists('ciaostr.tmp') ->
	readf('ciaostr.tmp', String),
	delete_file('ciaostr.tmp').

do_str_without_nl(Command, Fail, String) :-
	do_str(Command, Fail, StringNl),
	( append(String, [0'\n], StringNl) -> true
	; String = StringNl ).

:- pred set_owner_rec(Dir, Group) # "Fix recursively the group to
@var{Group} for all the directory @var{Dir}.".

set_owner_rec(Dir, Owner) :-
	( nonvar(Owner), Owner = grp(_, _) ->
	    (
		set_owner(Dir, Owner),
		path_name(Dir, FDir),
		current_find(FDir, dir_before_dummy_, dir_after_dummy_, _,
		    _, _, FileName),
		set_owner(FileName, Owner),
		fail
	    ;
		true
	    )
	;
	    throw(error(instantiation_error, set_perm_rec/2 -2))
	).

:- reexport(library(file_utils), [output_to_file/2]).

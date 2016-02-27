:- module(_, [
		del_dir_if_empty/1,
		move_files/2,
		move_file/2,
		copy_files/2,
		copy_files/3,
		copy_files_nofail/3,
		cat/2, cat_append/2,
		symbolic_link/2, symbolic_link/3,
		delete_files/1,
		del_files_nofail/1,
		del_file_nofail/1,
		del_file_nofail/2,
		del_endings_nofail/2,
		ls/3, ls/2,
		filter_alist_pattern/3,
		'-'/1,
		'--'/1,
		do/2,
		do/3,
		do/4,
		do/5,
		try_finally/3,
		set_owner/2,
		readf/2,
		datime_string/1,
		datime_string/2,
		datime_atom/1,
		datime_atom/2,
		no_tr_nl/2,
		replace_strings/3,
		replace_strings_in_file/3,
		writef/3,
		writef/2,
		add_suffix/3,
		add_preffix/3,
		writef_list/3,
		writef_list/2,
		etags/2,
		any_to_term/2,
		touch/1
	    ],
	    [assertions, regtypes, isomodes, hiord, regexp]).

:- doc(title, "Additional operating system utilities").

:- doc(author, "Manuel Hermenegildo").

:- doc(module, "This is a (temporary) extension to library
   @lib{system} (which it reexports). It implements functionality that
   is often convenient in @file{Makefile}s. Much of this should
   probably end up eventually in @lib{system}, but once we have worked
   out the best interface and, in some cases, the proper
   implementation (the implementations in here are in some cases just
   calls to Un*x shell primitives or commands).").

:- reexport(library(system)).
%%  [datime/9,working_directory/2,file_exists/1,
%%   file_exists/2,file_property/2,chmod/2,system/2,delete_file/1,
%%   directory_files/2,cd/1]

:- use_module(library(messages)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [list_concat/2, append/3]).
:- use_module(library(llists)).
:- use_module(library(sort), [sort/2]).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(file_utils)).

:- reexport(library(distutils(setperms))).

:- use_module(engine(basiccontrol), ['$metachoice'/1]).

%% IDEA: where they used to take an atom, all now take in addition a 
%% list of atoms, which are concatenated (saves all the 
%% calls to atom_concat).

%% -------------------------------------------------------------------------
%% These are preds from the SICStus lib that probably need to be implemented 
%% -------------------------------------------------------------------------

%% In ciao it is called pause/1.
%% `sleep(+SECONDS)'
%%      Puts the SICStus Prolog process asleep for SECOND seconds.

%%    Some predicates are described as invoking the default shell.
%% Specifically this means invoking `/bin/sh' on UNIX platforms. On MSDOS,
%% Windows and OS/2, the command interpreter given by the environment
%% variable `COMSPEC' is invoked.

%% Needs to be added 
%% :- doc(delete_file(FileName,Options), "@var{FileName} is the
%%    name of an existing file or directory.  @var{Options} is a list of
%%    options. Possible options are @tt{directory}, @tt{recursive} or
%%    @tt{ignore}.
%%    If @var{FileName} is not a directory it is deleted, otherwise if
%%    the option @tt{directory} is specified but not @tt{recursive}, the
%%    directory will be deleted if it is empty. If @tt{recursive} is
%%    specified and @var{FileName} is a directory, the directory and all
%%    its subdirectories and files will be deleted.  If the operation
%%    fails, an exception is raised unless the @tt{ignore} option is
%%    specified.").
%% 
%% :- true pred delete_file(+atm,+list(delete_file_option)).
%% 
%% delete_file(FileName,[recursive]) :- delete_file(FileName,[recursive])
%% 
%% 
%% 
%% :- doc(delete_file(FileName), "Equivalent to
%%    @tt{delete_file(FileName,[recursive])}.").
%% 
%% :- true pred delete_file(+atm).
%% 
%% delete_file(FileName) :- delete_file(FileName,[recursive])
%% 
%% :- regtype delete_file_option(X) # "@var{X} is an option controlling
%%    file deletion".
%% 
%% delete_file_option(directory).
%% delete_file_option(recursive).
%% delete_file_option(ignore).

del_dir_if_empty(Dir) :-
	working_directory(CD, CD),
	( file_exists(Dir)
	-> cd(Dir),
	    ( ls('*', ['..', '.']) %% empty dir!
	    -> delete_directory(Dir)
	    ; true )
	; true ),
	cd(CD).

%% Note name and type change, and it enumerates.
%% `environ(?VAR, ?VALUE)'
%%      VAR is the name of an environment variable, and VALUE is its
%%      value.  Both are atoms.  Can be used to enumerate all current
%%      environment variables.


%% Note options:
%% `exec(+COMMAND, [+STDIN,+STDOUT,+STDERR], -PID)'
%%      Passes COMMAND to a new default shell process for execution.  The
%%      standard I/O streams of the new process are connected according to
%%      what is specified by the terms +STDIN, +STDOUT, and +STDERR
%%      respectively.  Possible values are:
%% 
%%     `null'
%%           Connected to `/dev/null' or equivalent.
%% 
%%     `std'
%%           The standard stream is shared with the calling process. Note
%%           that the standard stream may not be referring to a console if
%%           the calling process is "windowed". To portably print the
%%           output from the subprocess on the Prolog console, `pipe/1'
%%           must be used and the program must explicitly read the pipe
%%           and write to the console. Similarly for the input to the
%%           subprocess.
%% 
%%     `pipe(-STREAM)'
%%           A pipe is created which connects the Prolog stream STREAM to
%%           the standard stream of the new process. It must be closed
%%           using `close/1'; it is not closed automatically when the
%%           process dies.
%% 
%%      PID is the process identifier of the new process.
%% 
%%      On UNIX, the subprocess will be detached provided none of its
%%      standard streams is specified as `std'. This means it will not
%%      receive an interruption signal as a result of C being typed.

%% Note atom-based options:
%% `file_exists(+FILENAME, +PERMISSIONS)'
%%      FILENAME is the name of an existing file or directory which can be
%%      accessed according to PERMISSIONS.  PERMISSIONS is an atom, an
%%      integer (see access(2)), or a list of atoms and/or integers.  The
%%      atoms must be drawn from the list `[read,write,search,exists]'.
%% 

%% These, somewhat incompatible
%% `host_id(-HID)'
%%      HID is the unique identifier, represented by an atom, of the host
%%      executing the current SICStus Prolog process.
%% 
%% `host_name(-HOSTNAME)'
%%      HOSTNAME is the standard host name of the host executing the
%%      current SICStus Prolog process.
%% 
%% `pid(-PID)'
%%      PID is the identifier of the current SICStus Prolog process.

%% `kill(+PID, +SIGNAL)'
%%      Sends the signal SIGNAL to process PID.


:- doc(move_files(Files, Dir), "Move @var{Files} to directory
	@var{Dir} (note that to move only one file to a directory,
	@pred{rename_file/2} can be used).").

:- true pred move_files(+list(atm), +atm).

%% Need to do this better of course...

move_files(Files, Dir0) :-
	dir_path(Dir0, Dir),
	move_files_(Files, Dir).

move_files_([],           _Dir).
move_files_([File|Files], Dir) :-
	move_file(File, Dir),
	move_files_(Files, Dir).

move_file(File, Dir) :-
	atom_concat(Dir, File, Target),
	rename_file(File, Target).

:- doc(copy_files(Files, Dir), "Copy @var{Files} to directory
	@var{Dir} (note that to move only one file to a directory,
	@pred{rename_file/2} can be used).").

:- true pred copy_files(+list(atm), +atm).

%% Need to do this better of course...
copy_files(Files, Dir) :-
	copy_files(Files, Dir, []).

copy_files([],           _DestDir, _CopyOptions).
copy_files([File|Files], DestDir,  CopyOptions) :-
	copy_file(File, DestDir, CopyOptions),
	copy_files(Files, DestDir, CopyOptions).

copy_files_nofail([],           _DestDir, _CopyOptions).
copy_files_nofail([File|Files], DestDir,  CopyOptions) :-
	--copy_file(File, DestDir, CopyOptions),
	copy_files_nofail(Files, DestDir, CopyOptions).

% copy_file_dir(File,Dir) :-
% 	atom_concat([Dir,'/',File],Target),
% 	copy_file(File,Target).

%% Must be done using OS -- this is way too slow...
%copy_file(File,Dir) :-
%	file_exists(File),
%	file_property(File, type(directory)),
%	!,
%	atom_concat([Dir,'/',File],Target),
%	cat(File,Target).
%copy_file(File,Target) :-
%	cat(File,Target).

%% This one missing (simple to add?)
%% `system'
%%      Starts a new interactive default shell process.  The control is
%%      returned to Prolog upon termination of the shell process.
%% 

%% In sicstus, fails if return not zero, i.e., should be:
%% system(Path) :- system(Path, 0).?????
%% 
%% :- doc(system(Command), "Executes @var{Command} using the shell
%%         @apl{/bin/sh}.").
%% 
%% :- true pred system(+atm).
%% 
%% system(Path) :- system(Path, _Status).
%% 

%% `tmpnam(-FILENAME)'
%%      Interface to the ANSI C function tmpnam(3).  A unique file name is
%%      created and unified with FILENAME.

%% `wait(+PID, -STATUS)'
%%      Waits for the child process PID to terminate. The exit status is
%%      returned in STATUS. The function is similar to that of the UNIX
%%      function `waitpid(3)'.


:- push_prolog_flag(multi_arity_warnings, off).

:- pred symbolic_link(Source, Dir) # "Create a symbolic link in
   @var{Dir} pointing to file or directory @var{Source} (performs a
   copy in Windows).".

%% Needs to be implemented...
symbolic_link(Source, Dir) :-
	warning_message("symbolic_link(Source,Dir) is deprecated.   "||
	    "Use copy_file(Source, Dir, [symlink]) instead.", []),
	do(['cd ', Dir, ' ; ln -s ', Source], nofail).

:- pred symbolic_link(Source, Dir, NewName) # "Create a symbolic link in
   @var{Dir} pointing to file or directory @var{Source} and give it
   name @var{NewName} (performs a copy in Windows).".

%% Needs to be implemented...
symbolic_link(Source, Dir, NewName) :-
	warning_message("symbolic_link(Source,Dir,NewName) is deprecated.  "||
	    "Use copy_file(Source, ~~atom_concat(Dir,\'/\',NewName), " ||
	    "[symlink]) instead.", []),
	do(['cd ', Dir, ' ; ln -s ', Source, ' ', NewName], nofail).

:- pop_prolog_flag(multi_arity_warnings).

%% -------------------------------------------------------------------------
%% Very useful predicates
%% -------------------------------------------------------------------------

:- push_prolog_flag(multi_arity_warnings, off).

:- doc(ls(Directory, Pattern, FileList), "@var{FileList} is
        the unordered list of entries (files, directories, etc.) in
        @var{Directory} whose names match @var{Pattern}.If
        @var{Directory} does not exist @var{FileList} is empty.").

:- export(pattern/1).
:- regtype pattern/1.

pattern(A) :-
	atom(A).

:- true pred ls(+atm, +pattern, -list(atm)).

ls(Directory, Pattern, SFileList) :-
	file_exists(Directory),
	!,
	directory_files(Directory, Files),
	filter_alist_pattern(Files, Pattern, FileList),
	sort(FileList, SFileList).
ls(_Directory, _Pattern, []).

:- doc(ls(Pattern, FileList),
"@var{FileList} is the unordered list of entries (files,
        directories, etc.) in the current directory whose names match
        @var{Pattern} (same as
        @tt{ls('.',Pattern,FileList)}).").

:- true pred ls(+pattern, -list(atm)).

ls(Pattern, FileList) :-
	ls('.', Pattern, FileList).

etag(AbsFile, TagFile) :-
	do(['etags -a -l prolog ', AbsFile, ' -o ', TagFile], nofail).

etags([],                    _TagFile).
etags([AbsFile|AbsFileList], TagFile) :-
	etag(AbsFile, TagFile),
	etags(AbsFileList, TagFile).

:- pop_prolog_flag(multi_arity_warnings).

add_suffix([],     _Suffix, []).
add_suffix([L|Ls], Suffix,  [R|Rs]) :-
	atom_concat(L, Suffix, R),
	add_suffix(Ls, Suffix, Rs).

add_preffix([],     _Preffix, []).
add_preffix([L|Ls], Preffix,  [R|Rs]) :-
	atom_concat(Preffix, L, R),
	add_preffix(Ls, Preffix, Rs).

% get_dirs(Dir, Dirs) :-
% 	directory_files(Dir, Files),
% 	filter_dirs(Dir, Files, Dirs).

% filter_dirs(_Dir, [], []).
% filter_dirs(Dir, ['.'|Files], Dirs) :-
% 	filter_dirs(Dir, Files, Dirs).
% filter_dirs(Dir, ['..'|Files], Dirs) :-
% 	filter_dirs(Dir, Files, Dirs).
% filter_dirs(Dir, [File|Files], [File|Dirs]) :-
% 	atom_concat([Dir, File],File2),
% 	file_property(File2, type(directory)), !,
% 	filter_dirs(Dir, Files, Dirs).
% filter_dirs(Dir, [_File|Files], Dirs) :-
% 	filter_dirs(Dir, Files, Dirs).

:- doc(touch(File), "Updates the access and modification time of
   @var{File} to current time.").

touch(File) :-
	time(Time),
	modif_time(File, Time).


:- doc(filter_alist_pattern(UnFiltered, Pattern, Filtered),
"@var{Filtered} contains the elements of @var{UnFiltered}
         which match with @var{Pattern}.").

:- true pred filter_alist_pattern(+list(atm), +pattern, -list(atm)).

filter_alist_pattern([],     _,       []).
filter_alist_pattern([T|Ts], Pattern, O) :-
	( match_pred(Pattern, T) ->
	    O = [T|NTs]
	; O = NTs
	),
	filter_alist_pattern(Ts, Pattern, NTs).

%% -------------------------------------------------------------------------

:- meta_predicate(-(goal)).

-(G) :- catch(G, Error, ( warning_message("in -/1, goal ~w has raised the "||
		    "exception ~w", [G, Error]) )), !.
-(G) :- warning_message("in -/1, could not complete goal ~w", [G]).

:- meta_predicate(--(goal)).

--(G) :- catch(G, _Error, true), !.
--(_G).


:- meta_predicate try_finally(goal, goal, goal).

:- pred try_finally(Start, Goal, End) # "Calls initialization goal
	@var{Start} and then calls Goal @var{Goal}, but always
	continues with the evaluation of @var{End}.  If @var{Goal} is
	non-deterministic, in case of backtracking @var{Start} is called
	again before redoing @var{Goal}.".

try_finally(Start, Goal, End) :-
	(call(Start) ; call(End), fail),
	'$metachoice'(C1),
	catch(Goal, E, (!, call(End), throw(E))),
	'$metachoice'(C2),
	(call(End) ; call(Start), fail),
	(C1 == C2 -> ! ;          true).

set_owner([], _) :-
	!.
set_owner([F|Fs], O) :-
	!,
	set_owner(F,  O),
	set_owner(Fs, O).
set_owner(File, grp(User, Group)) :-
	!,
	do(['chown ', User, ':', Group, ' ', File], nofail).
set_owner(File, User) :-
	do(['chown ', User, ' ', File], nofail).

% TODO: Wrong name!
del_endings_nofail([],               _FileBase).
del_endings_nofail([Ending|Endings], FileBase) :-
	del_file_nofail(FileBase, Ending),
	del_endings_nofail(Endings, FileBase).

:- push_prolog_flag(multi_arity_warnings, off).

% del_file_nofail(File) :-
% 	del_file_nofail(File,'').

del_file_nofail(File) :-
	--delete_file(File).

del_file_nofail(FileBase, Ending) :-
	atom_concat(FileBase, Ending, File),
	file_exists(File, 2), % exists and writeable
	!,
	delete_file(File).
del_file_nofail(FileBase, Ending) :-
	atom_concat(FileBase, Ending, File),
	file_exists(File), % exists but not writeable
	!,
	warning_message("Could not delete file ~w~w", [FileBase, Ending]).
del_file_nofail(_FileBase, _Ending).
%% 	!,
%% 	note_message("File ~w~w not deleted (does not exist)",
%% 	             [FileBase,Ending]).

:- pop_prolog_flag(multi_arity_warnings).

del_files_nofail([]).
del_files_nofail([File|Files]) :-
	del_file_nofail(File),
	del_files_nofail(Files).

delete_files([]).
delete_files([File|Files]) :-
	atom(File), !,
	delete_file(File),
	delete_files(Files).
delete_files([File|Files]) :-
	list(File, atom), !,
	atom_concat(File, FileAtom),
	delete_file(FileAtom),
	delete_files(Files).


:- pred do(Command, OutputFile, ErrorFile, Action, ReturnCode)
	: ( list(Command, atm),
	    atm(OutputFile), atm(ErrorFile),
	    list(Action, do_options) )
	=> num(ReturnCode)

# "Executes @var{Command} redirecting standard output to
  @var{OutputFile} and standard error to
  @var{ErrorFile}. @var{ReturnCode} is the code returned by the
  execution of @var{Command}. @var{Action} is a list of atoms that
  specify the actions to be completed in case the @var{Command}
  fails. Three of these options: @tt{fail}, @tt{exception}, and
  @tt{nofail} are mutually exclusive. The rest of the options are
  flags that mean (type @pred{do_options/1}):

@begin{description}

@item @tt{inform_nofail}: informs about the error code returned by the
execution of the command.

@item @tt{show_output_on_error}: shows the content of @var{OutputFile}
in case of error.

@item @tt{show_error}: shows the content of @var{ErrorFile} in case of
error.

@item @tt{silent}: do not print any error message. The option
@tt{inform_nofail} overrides this option in case of @tt{fail}.

@item @tt{verbose_command}: shows the command before being
executed. Useful for tracing.

@item @tt{verbose}: @tt{verbose_command} plus overrides the error and
output file settings and outputs everything to @tt{user_output} and
@tt{user_error}.

@end{description}".

:- export(do_options/1).
:- regtype do_options/1.

do_options(fail).
do_options(nofail).
do_options(silent).
do_options(exception).
do_options(halt).
do_options(inform_nofail).
do_options(show_output_on_error).
do_options(show_error_on_error).
do_options(verbose).
do_options(verbose_command).


do(Command, OutputFile, ErrorFile, Action, ReturnCode) :-
	( member(verbose, Action) ->
	    do_command(Command, Action, ACommand, ReturnCode),
	    process_return_code(ReturnCode, Action, ACommand)
	; ComposeCommand = ['(', Command, ') >', OutputFile, ' 2>', ErrorFile],
	    do_command(ComposeCommand, Action, ACommand, ReturnCode),
	    show_error_if_required(ReturnCode, Action, OutputFile, ErrorFile),
	    process_return_code(ReturnCode, Action, ACommand) ).

show_error_if_required(0, _, _OutputFile, _ErrorFile) :-
	!.
show_error_if_required(_ReturnCode, Action, OutputFile, ErrorFile) :-
	( member(show_output_on_error, Action) ->
	    file_to_string(OutputFile, OutputString),
	    display_string(OutputString)
	; true ),
	( member(show_error_on_error, Action) ->
	    file_to_string(ErrorFile, ErrorString),
	    display_string(ErrorString)
	;  true ).

% 	exec(Command, [], SExecI, SExecO, SErrO, wait, _PID,
% 	    ErrCode),
% 	close(SExecI),
% 	stream_to_string(SExecO, Output),
% 	stream_to_string(SErrO,  Error).

:- pred do(Command, OutputFile, ErrorFile, Action)
	: ( list(Command, atm),
	    atm(OutputFile), atm(ErrorFile),
	    list(Action, do_options) )

# "Same as @pred{do/5} but omitting the returned code.".

do(Command, OutputFile, ErrorFile, Action) :-
	do(Command, OutputFile, ErrorFile, Action, _ReturnCode).


:- pred do(Command, Action)
	: ( list(Command, atm),
	    list(Action, do_options) )

# "Same as @pred{do/3} but omitting the return code.".

do(Command, Action) :-
	do(Command, Action, _ReturnCode).


:- pred do(Command, Action, ReturnCode)
	: ( list(Command, atm),
	    list(Action, do_options) )
	=> num(ReturnCode)

# "Same as @pred{do/5} but omitting the files.".

do(Command, Action, ReturnCode) :-
	do_command(Command, Action, ACommand, ReturnCode),
	process_return_code(ReturnCode, Action, ACommand).

do_command(Command, Action, ACommand, ReturnCode) :-
	flatten(Command, FCommand),
	atom_concat(FCommand, ACommand),
	( ( member(verbose_command, Action)
	    ; member(verbose, Action) )
	-> display_string("{OS command: "),
	    write(ACommand),
	    display_string(" }\n")
	; true ),
	system(ACommand, ReturnStatus),
	ReturnCode is (ReturnStatus /\ 0xFF00) >> 8.

process_return_code(0,          _,      _) :- !.
process_return_code(ReturnCode, Action, Command) :-
	atom(Action),
	!,
	process_action([Action], ReturnCode, Command).
process_return_code(ReturnCode, Action, Command) :-
	process_action(Action, ReturnCode, Command).

process_action(L, ReturnCode, Command) :-
	member(fail, L),
	!,
	( member(silent, L) -> true
	; error_message("~w returned code ~w", [Command, ReturnCode]) ),
	fail.
process_action(L, ReturnCode, Command) :-
	member(nofail, L),
	!,
	( (member(inform_nofail, L) ; \+ member(silent, L))
	-> error_message("~w returned code ~w", [Command, ReturnCode])
	; true ).
process_action(L, ReturnCode, Command) :-
	member(halt, L),
	!,
	( member(silent, L) -> true
	; error_message("~w returned code ~w. Halting system.",
		[Command, ReturnCode]) ),
	halt(ReturnCode).
process_action(L, ReturnCode, Command) :-
	member(exception, L),
	!,
% --- DTM: Who understands this exception???
	throw(error(system_error(ReturnCode, Command))).
process_action(_, _ReturnCode, _Command).


cat(Sources, Target) :-
	( file_exists(Target)
	-> delete_file(Target)
	; true ),
	cat_append(Sources, Target).

cat_append(Sources, Target) :-
	open(Target, append, O),
	( cat_append_stream(Sources, O)
	-> close(O)
	; close(O) ).

cat_append_stream([], _O) :-
	!.
cat_append_stream([Source|Sources], O) :-
	!,
	cat_append_stream_one(Source, O),
	cat_append_stream(Sources, O).
cat_append_stream(Source, O) :-
	cat_append_stream_one(Source, O).

cat_append_stream_one(Source, O) :-
	atom(Source),
	Source \== [],
	!,
	open(Source, read, I),
	copy_stream(I, O),
	close(I).

copy_stream(I, O) :-
	get_code(I, Code),
	(Code = -1 -> true ; put_code(O, Code), copy_stream(I, O)).


readf(Files, List) :-
	do_readf(Files, List, []).

do_readf([], T, T) :-
	!.
do_readf([Source|Sources], H, T) :-
	!,
	readf_one(Source, H, T1),
	do_readf(Sources, T1, T).
do_readf(Source, H, T) :-
	readf_one(Source, H, T).

readf_one(Source, H, T) :-
	atom(Source),
	Source \== [],
	!,
	open(Source, read, I),
	copy_stream_list(I, H, T),
	close(I).

copy_stream_list(I, H, T) :-
	get_code(I, Code),
	(Code = -1 -> H=T ; H=[Code|R], copy_stream_list(I, R, T)).

datime_atom(T) :-
	datime_string(S),
	atom_codes(T, S).

datime_atom(D, T) :-
	datime_string(D, S),
	atom_codes(T, S).

datime_string(S) :- datime_string(_, S).
datime_string(T, S) :-
	datime(T, Year, Month, Day, Hour, Min, Sec, _WeekDay, _YearDay),
	datime_to_string(datime(Year, Month, Day, Hour, Min, Sec), S).

datime_to_string(datime(Year, Month, Day, Hour, Min, Sec), S) :-
	number_codes(Day,  DayS), number_codes(Month, MonthS),
	number_codes(Year, YearS), number_codes(Hour, HourS),
	number_codes(Min,  MinS), number_codes(Sec, SecS),
	list_concat([DayS, "/", MonthS, "/", YearS, " ", HourS, ":",
		MinS, ":", SecS], S).

no_tr_nl(L, NL) :-
	append(NL, [10], L),
	!.
no_tr_nl(L, L).

replace_strings_in_file(Ss, F1, F2) :-
	readf(F1, F1S),
	replace_strings(Ss, F1S, F2S),
	writef(F2S, F2).

:- push_prolog_flag(multi_arity_warnings, off).

%% Not really necessary... (simply open output and display...)
%% Also, there is file_to_string, etc. => unify

writef(Codes, File) :-
	writef(Codes, write, File).

writef(Codes, _Mode, _File) :-
	(\+ (Codes = [_|_] ; Codes = [])),
	!,
	throw(error(domain_error(string, Codes), writef/3 -1)).
writef(_Codes, Mode, _File) :-
	(\+ (Mode = write ; Mode = append)),
	!,
	throw(error(domain_error(write_or_append, Mode), writef/3 -2)).
writef(_Codes, _Mode, File) :-
	(\+ atom(File) ; File = []),
	!,
	throw(error(domain_error(filename, File), writef/3 -3)).
writef(Codes, Mode, File) :-
	open(File, Mode, O),
	codes_to_stream(Codes, O),
	close(O).

:- pop_prolog_flag(multi_arity_warnings).

% some extensions to writef:
:- push_prolog_flag(multi_arity_warnings, off).

writef_list(A, Config) :-
	list_concat(A, B),
	writef(B, Config).

writef_list(A, Option, Config) :-
	list_concat(A, B),
	writef(B, Option, Config).

:- pop_prolog_flag(multi_arity_warnings).


codes_to_stream([],    _O).
codes_to_stream([H|T], O) :-
	put_code(O, H),
	codes_to_stream(T, O).

replace_strings([], O, O).
%replace_strings([S1-S2|Ss],I,O) :-
%	replace_string(I,S1,S2,TO),
%	replace_strings(Ss,TO,O).

replace_strings([[S1, S2]|Ss], I, O) :-
	replace_string(I, S1, S2, TO),
	replace_strings(Ss, TO, O).

replace_string(_I, S1, _S2, _TO) :-
	atom(S1),
	!,
	throw(error(domain_error(string, atom), replace_string/4 -2)).
replace_string(I, S1, "", TO) :-
	!,
	do_replace_string(I, S1, "", TO).
replace_string(_I, _S1, S2, _TO) :-
	atom(S2),
	!,
	throw(error(domain_error(string, atom), replace_string/4 -3)).
replace_string(I, S1, S2, TO) :-
	do_replace_string(I, S1, S2, TO).

do_replace_string([], _S1, _S2, []) :- !.
do_replace_string(I,  S1,  S2,  O) :-
	match(S1, I, RI),
	!,
	append(S2, NO, O),
	do_replace_string(RI, S1, S2, NO).
do_replace_string([H|RI], S1, S2, [H|RO]) :-
	do_replace_string(RI, S1, S2, RO).


match([],    I,      I).
match([H|T], [H|IT], RI) :-
	match(T, IT, RI).

:- doc(any_to_term(Any, Term), "Interprets the result of print
   @var{Any} as a term @var{Term}").

any_to_term(Any, Term) :-
	pipe(ReadFrom, WriteTo),
	write(WriteTo, Any),
	write(WriteTo, '.'),
	close(WriteTo),
	read_term(ReadFrom, Term, []),
	close(ReadFrom).

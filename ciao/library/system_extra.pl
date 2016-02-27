:- module(_, [], [assertions, regtypes, isomodes, hiord, regexp]).

:- doc(title, "Additional operating system utilities").

:- doc(author, "Manuel Hermenegildo").

:- doc(module, "This is a (temporary) extension to library
   @lib{system} (which it reexports). Much of this should
   probably end up eventually in @lib{system}, but once we have worked
   out the best interface and, in some cases, the proper
   implementation (the implementations in here are in some cases just
   calls to Un*x shell primitives or commands).
").
%% (I believe that this comment may be confusing -- JFMC)
% It implements functionality that is often convenient in
% @file{Makefile}s.

:- reexport(library(system)).
%%  [datime/9,working_directory/2,file_exists/1,
%%   file_exists/2,file_property/2,chmod/2,system/2,delete_file/1,
%%   directory_files/2,cd/1]

:- use_module(library(messages)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [list_concat/2, append/3]).
:- use_module(library(llists)).
:- use_module(library(sort), [sort/2]).
:- use_module(library(write)).
:- use_module(library(file_utils)).

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

% TODO: Improve this implementation
:- export(del_dir_if_empty/1).
del_dir_if_empty(Dir) :-
	working_directory(CD, CD),
	( file_exists(Dir) ->
	    cd(Dir),
	    ( ls('*', ['..', '.']) ->
	        %% empty dir!
	        delete_directory(Dir)
	    ; true
	    )
	; true
	),
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


:- export(move_files/2).
:- pred move_files(Files, Dir) # "Move @var{Files} to directory
	@var{Dir} (note that to move only one file to a directory,
	@pred{rename_file/2} can be used).".

:- true pred move_files(+list(atm), +atm).

%% Need to do this better of course...

move_files(Files, Dir0) :-
	dir_path(Dir0, Dir),
	move_files_(Files, Dir).

move_files_([],           _Dir).
move_files_([File|Files], Dir) :-
	move_file(File, Dir),
	move_files_(Files, Dir).

:- export(move_file/2).
:- pred move_file(File, Dir) # "Move @var{File} to directory
   @var{Dir}".
move_file(File, Dir) :-
	atom_concat(Dir, File, Target),
	rename_file(File, Target).

:- export(copy_files/2).
:- pred copy_files(Files, Dir) # "Like @pred{copy_files/3}, with empty
   options list.".

:- true pred copy_files(+list(atm), +atm).

%% Need to do this better of course...
copy_files(Files, Dir) :-
	copy_files(Files, Dir, []).

:- export(copy_files/3).
:- pred copy_files(Files, Dir, Opts) # "Copy @var{Files} to directory
   @var{Dir}, using @var{Opts} as the option list for copy. See
   @pred{copy_file/3} for the list of options. Note that to move only
   one file to a directory, @pred{rename_file/2} can be used.".

copy_files([],           _DestDir, _CopyOptions).
copy_files([File|Files], DestDir,  CopyOptions) :-
	copy_file(File, DestDir, CopyOptions),
	copy_files(Files, DestDir, CopyOptions).

:- export(copy_files_nofail/3).
:- pred copy_files_nofail(Files, Dir, Opts) # "Like @pred{copy_files/3},
   but do not fail in case of errors.".

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

:- export(symbolic_link/2).
:- pred symbolic_link(Source, Dir) # "Create a symbolic link in
   @var{Dir} pointing to file or directory @var{Source} (performs a
   copy in Windows).".

%% Needs to be implemented...
symbolic_link(Source, Dir) :-
	warning_message("symbolic_link(Source,Dir) is deprecated.   "||
	    "Use copy_file(Source, Dir, [symlink]) instead.", []),
	do(['cd ', Dir, ' ; ln -s ', Source], nofail).

:- export(symbolic_link/3).
:- pred symbolic_link(Source, Dir, NewName) # "Create a symbolic link in
   @var{Dir} pointing to file or directory @var{Source} and give it
   name @var{NewName} (performs a copy in Windows).".

%% Needs to be implemented...
symbolic_link(Source, Dir, NewName) :-
	warning_message("symbolic_link(Source,Dir,NewName) is deprecated.  "||
	    "Use copy_file(Source, ~~atom_concat(Dir,\'/\',NewName), " ||
	    "[symlink]) instead.", []),
	do(['cd ', Dir, ' ; ln -s ', Source, ' ', NewName], nofail).

%% -------------------------------------------------------------------------
%% Very useful predicates
%% -------------------------------------------------------------------------

:- export(ls/3).
:- pred ls(Directory, Pattern, FileList) #
   "@var{FileList} is the unordered list of entries (files,
   directories, etc.) in @var{Directory} whose names match
   @var{Pattern}.If @var{Directory} does not exist @var{FileList} is
   empty.".

:- export(pattern/1).
:- regtype pattern/1.

pattern(A) :-
	atom(A).

:- export(ls/2).
:- true pred ls(+atm, +pattern, -list(atm)).

ls(Directory, Pattern, SFileList) :-
	file_exists(Directory),
	!,
	directory_files(Directory, Files),
	filter_alist_pattern(Files, Pattern, FileList),
	sort(FileList, SFileList).
ls(_Directory, _Pattern, []).

:- pred ls(Pattern, FileList) #
   "@var{FileList} is the unordered list of entries (files,
   directories, etc.) in the current directory whose names match
   @var{Pattern} (same as @tt{ls('.',Pattern,FileList)}).".

:- true pred ls(+pattern, -list(atm)).

ls(Pattern, FileList) :-
	ls('.', Pattern, FileList).

etag(AbsFile, TagFile) :-
	do(['etags -a -l prolog ', AbsFile, ' -o ', TagFile], nofail).

:- export(etags/2).
etags([],                    _TagFile).
etags([AbsFile|AbsFileList], TagFile) :-
	etag(AbsFile, TagFile),
	etags(AbsFileList, TagFile).

:- export(add_suffix/3).
add_suffix([],     _Suffix, []).
add_suffix([L|Ls], Suffix,  [R|Rs]) :-
	atom_concat(L, Suffix, R),
	add_suffix(Ls, Suffix, Rs).

:- export(add_prefix/3).
add_prefix([],     _Preffix, []).
add_prefix([L|Ls], Preffix,  [R|Rs]) :-
	atom_concat(Preffix, L, R),
	add_prefix(Ls, Preffix, Rs).

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

:- export(filter_alist_pattern/3).
:- pred filter_alist_pattern(UnFiltered, Pattern, Filtered) #
   "@var{Filtered} contains the elements of @var{UnFiltered} which
    match with @var{Pattern}.".

:- true pred filter_alist_pattern(+list(atm), +pattern, -list(atm)).

filter_alist_pattern([],     _,       []).
filter_alist_pattern([T|Ts], Pattern, O) :-
	( match_pred(Pattern, T) ->
	    O = [T|NTs]
	; O = NTs
	),
	filter_alist_pattern(Ts, Pattern, NTs).

%% -------------------------------------------------------------------------

:- export('-'/1).
:- meta_predicate(-(goal)).

% Call G and show warning messages if something went wrong (failure
% and exceptions)
-(G) :- catch(G, Error, ( warning_message("in -/1, goal ~w has raised the "||
		    "exception ~w", [G, Error]) )), !.
-(G) :- warning_message("in -/1, could not complete goal ~w", [G]).

:- export('--'/1).
:- meta_predicate(--(goal)).

% Call G and ignore if something went wrong (failure and exceptions)
--(G) :- catch(G, _Error, true), !.
--(_G).


:- export(try_finally/3).
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

:- export(set_owner/2).
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

:- export(del_endings_nofail/2).
% TODO: Wrong name!
del_endings_nofail([],               _FileBase).
del_endings_nofail([Ending|Endings], FileBase) :-
	del_file_nofail(FileBase, Ending),
	del_endings_nofail(Endings, FileBase).

% del_file_nofail(File) :-
% 	del_file_nofail(File,'').

:- export(del_file_nofail/1).
del_file_nofail(File) :-
	--delete_file(File).

:- export(del_file_nofail/2).
% TODO: strange...
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

:- export(del_files_nofail/1).
del_files_nofail([]).
del_files_nofail([File|Files]) :-
	del_file_nofail(File),
	del_files_nofail(Files).

:- export(delete_files/1).
% TODO: remove, is this just flatten composed with del_files?
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


:- export(do/5).
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
	;
	    ( ErrorFile = '&1' -> ErrorFile0 = [ErrorFile]
	    ; ErrorFile0 = ['"', ErrorFile, '"'] ),
	    ComposeCommand = ['(', Command, ') >>"', OutputFile, '" 2>' |
	        ErrorFile0],
	    
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

:- export(do/4).
% TODO: Action is not an action but a list of options, fix!
:- pred do(Command, OutputFile, ErrorFile, Action)
	: ( list(Command, atm),
	    atm(OutputFile), atm(ErrorFile),
	    list(Action, do_options) )

# "Same as @pred{do/5} but omitting the returned code.".

do(Command, OutputFile, ErrorFile, Action) :-
	do(Command, OutputFile, ErrorFile, Action, _ReturnCode).


:- export(do/2).
:- pred do(Command, Action)
	: ( list(Command, atm),
	    list(Action, do_options) )

# "Same as @pred{do/3} but omitting the return code.".

do(Command, Action) :-
	do(Command, Action, _ReturnCode).


:- export(do/3).
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


% TODO: Avoid creating the ciaostr.tmp file. At least, put it in another place with another name
:- export(do_str/3).
do_str(Command, Fail, String) :-
	( append(Command, [' > ciaostr.tmp'], Cmd),
	  do(Cmd, Fail),
	  file_exists('ciaostr.tmp') ->
	    readf('ciaostr.tmp', String),
	    delete_file('ciaostr.tmp')
	; fail
	).

% TODO: improve...
:- export(do_str_without_nl/3).
do_str_without_nl(Command, Fail, String) :-
	do_str(Command, Fail, StringNl),
	no_tr_nl(StringNl, String).

% TODO: merge with do_str_without_nl?
:- export(do_str_without_nl__popen/2).
do_str_without_nl__popen(Command, String) :-
	popen(Command, read, Stream),
	stream_to_string(Stream, String0),
	no_tr_nl(String0, String).

% Execute @var{Command} and read each line from the output as an atom
% TODO: merge with do (with options)
:- export(do_atmlist__popen/2).
do_atmlist__popen(Command, Xs) :-
	popen(Command, read, Stream),
	read_lines(Stream, Xs).

% read each line as individual atoms
read_lines(Stream, Xs) :-
	get_line(Stream, L),
	!,
	(
	    L = end_of_file
	->
	    Xs = []
	;
	    atom_codes(X, L),
	    Xs = [X|Xs0],
	    read_lines(Stream, Xs0)
	).
read_lines(_, []).

:- use_module(library(strings), [get_line/2]).

% ---------------------------------------------------------------------------

:- export(cat/2).
% TODO: isn't it just copy?
cat(Sources, Target) :-
	( file_exists(Target)
	-> delete_file(Target)
	; true ),
	cat_append(Sources, Target).

:- export(cat_append/2).
% TODO: isn't it something like file_append?
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


:- export(readf/2). % TODO: like file_to_string?
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

:- export(datime_atom/1).
datime_atom(T) :-
	datime_string(S),
	atom_codes(T, S).

:- export(datime_atom/2).
datime_atom(D, T) :-
	datime_string(D, S),
	atom_codes(T, S).

:- export(datime_string/1).
% datime, as a string
datime_string(S) :- datime_string(_, S).

:- export(datime_string/2).
% datime, as a string
datime_string(T, S) :-
	datime(T, Year, Month, Day, Hour, Min, Sec, _WeekDay, _YearDay),
	datime_to_string(datime(Year, Month, Day, Hour, Min, Sec), S).

:- export(datime_to_string/2).
% From datime to string representation
datime_to_string(datime(Year, Month, Day, Hour, Min, Sec), S) :-
	number_codes(Day,  DayS), number_codes(Month, MonthS),
	number_codes(Year, YearS), number_codes(Hour, HourS),
	number_codes(Min,  MinS), number_codes(Sec, SecS),
	list_concat([DayS, "/", MonthS, "/", YearS, " ", HourS, ":",
		MinS, ":", SecS], S).

:- export(no_tr_nl/2).
no_tr_nl(L, NL) :-
	append(NL, [0'\n], L),
	!.
no_tr_nl(L, L).

:- export(replace_strings_in_file/3).
replace_strings_in_file(Ss, F1, F2) :-
	readf(F1, F1S),
	replace_strings(Ss, F1S, F2S),
	writef(F2S, F2).

:- export(replace_params_in_file/3).
replace_params_in_file(Ss, F1, F2) :-
	readf(F1, F1S),
	replace_params(Ss, F1S, F2S),
	writef(F2S, F2).

%% Not really necessary... (simply open output and display...)
%% Also, there is file_to_string, etc. => unify

:- export(writef/2). % TODO: like string_to_file?
writef(Codes, File) :-
	writef(Codes, write, File).

:- export(writef/3). % TODO: like string_to_file?
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

% some extensions to writef:
:- export(writef_list/2).
writef_list(A, Config) :-
	list_concat(A, B),
	writef(B, Config).

:- export(writef_list/3).
writef_list(A, Option, Config) :-
	list_concat(A, B),
	writef(B, Option, Config).

codes_to_stream([],    _O).
codes_to_stream([H|T], O) :-
	put_code(O, H),
	codes_to_stream(T, O).

:- export(replace_strings/3).
% TODO: This implementation does several passes over the string,
%       see replace_params/3.
% TODO: Move to some string related module?
replace_strings([], O, O).
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

% ---------------------------------------------------------------------------

:- export(replace_params/3).
:- pred replace_params(Subst, Str, Str2) # "Replace @tt{<v>Key</v>}
   strings from the input @var{Str} string by values input by values
   stored in @var{Subst}".

replace_params(Subst, Str, Str2) :-
	params_to_dic(Subst, Dic),
	replace_params_(Dic, Str, Str2).

:- use_module(library(dict)).

params_to_dic([], _).
params_to_dic([K=V|KVs], Dic) :-
	dic_lookup(Dic, K, V),
	params_to_dic(KVs, Dic).

% Replace "<v>Key</v>" strings in input by values from Dic
replace_params_(Dic, Str, Str2) :-
 	parse_key(Str, Before, Key, After), !,
 	( dic_get(Dic, Key, Value) ->
	    % TODO: what about Value=[]?
 	    ( string(Value) -> ValueStr = Value
 	    ; atom(Value) -> atom_codes(Value, ValueStr)
 	    ; throw(unknown_value_in_replace_params(Value)) % TODO: good error?
 	    ),
 	    append(Before, S0, Str2),
 	    append(ValueStr, S1, S0)
 	; % not found, leave unchanged
 	  % TODO: should it complain instead?
 	  append(Before, S0, Str2),
	  atom_codes(Key, KeyStr),
 	  append("<v>"||KeyStr, "</v>"||S1, S0)
	),
 	replace_params_(Dic, After, S1).
replace_params_(_, Str, Str).

% Find the first match of <v>Key</v> in Str
parse_key(Str, Before, Key, After) :-
	append(Before, "<v>"||Str0, Str),
	append(KeyStr, "</v>"||After, Str0),
 	atom_codes(Key, KeyStr),
 	!.

% ===========================================================================

:- doc(section, "File Permissions").

:- export(get_perms/2).
get_perms(File, perm(User, Group, Others)) :-
	fmode(File, P),
	convert_permissions(User, Group, Others, P).

:- export(set_perms/2).
set_perms(Files, Perm) :-
	set_exec_perms_(Files, 0, _, Perm).

:- export(set_exec_perms/2).
set_exec_perms(Files, Perm) :-
	set_exec_perms_(Files, Exec, Exec, Perm).

:- export(mkdir_perm/2).
mkdir_perm(Dir, Perms) :-
	var(Perms),
	!,
	mkdir_mode(Dir, 0o777).
mkdir_perm(Dir, Perms) :-
	Perms = perm(User, Group, Others),
	execute_permissions(User, Group, Others, Exec),
	convert_permissions(User, Group, Others, Perm) ->
	Mode is Perm \/ Exec,
	mkdir_mode(Dir, Mode).

mkdir_mode(Dir, Mode) :-
	catch(make_dirpath(Dir, Mode),
	    Error,
	    (
		show_message(error, "Error calling ~w",
		    [make_dirpath(Dir, Mode)]),
		throw(Error)
	    )
	).

set_exec_perms_(Files, PermExec, Exec, perm(User, Group, Others)) :-
	(
	    execute_permissions(User, Group, Others, Exec),
	    convert_permissions(User, Group, Others, Perm) ->
	    set_perms_(Files, Perm \/ PermExec, Exec)
	;
	    error_message(
		"invalid permission (should be perm(User,Group,Others))", [])
	).

%% Files can have paths
set_perms_([], _Perm, _Exec) :-
	!.
set_perms_([File|Files], Perm, Exec) :-
	!,
	set_perms_(File,  Perm, Exec),
	set_perms_(Files, Perm, Exec).

set_perms_(File, Perm, Exec) :-
	set_perm(File, Perm, Exec).

set_perm(File, Perm, Exec) :-
	(
	    file_exists(File) ->
	    (
		file_property(File, mode(OrigMode)),
		(
		    file_property(File, type(directory)) ->
		    Mode is Perm \/ Exec
		;
		    Mode is Perm \/ (Exec /\ OrigMode)
		),
		(
		    Mode == OrigMode -> % same mode, do nothing
		    true
		;
		    chmod(File, Mode)
		)
	    )
	;
	    error_message("In set_perms/2, file '~w' not found", [File])
	).

:- export(execute_permissions/2).
execute_permissions(perm(U, G, O), E) :-
	execute_permissions(U, G, O, E).

:- export(execute_permissions/4).
execute_permissions(U, G, O, E) :-
	exec_mask_perm(U, NU),
	exec_mask_perm(G, NG),
	exec_mask_perm(O, NO),
	E is NU << 6 + NG << 3 + NO.

:- export(convert_permissions/2).
convert_permissions(perm(U, G, O), P) :-
	convert_permissions(U, G, O, P).

:- export(convert_permissions/4).
convert_permissions(U, G, O, P) :-
	valid_mode(U, NU),
	valid_mode(G, NG),
	valid_mode(O, NO),
	P is NU << 6 + NG << 3 + NO.

exec_mask_perm(''  , 0).
exec_mask_perm(x   , 0).
exec_mask_perm(w   , 0).
exec_mask_perm(wx  , 0).
exec_mask_perm(r   , 0).
exec_mask_perm(rx  , 0).
exec_mask_perm(rw  , 0).
exec_mask_perm(rwx , 0).
exec_mask_perm('X' , 1).
exec_mask_perm(wX  , 1).
exec_mask_perm(rX  , 1).
exec_mask_perm(rwX , 1).

valid_mode(''  , 0).
valid_mode(x   , 1).
valid_mode(w   , 2).
valid_mode(wx  , 3).
valid_mode(r   , 4).
valid_mode(rx  , 5).
valid_mode(rw  , 6).
valid_mode(rwx , 7).
valid_mode('X' , 0).
valid_mode(wX  , 2).
valid_mode(rX  , 4).
valid_mode(rwX , 6).

% ===========================================================================

:- export(backup_file/1).
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
	    ; del_file_nofail(B),
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

:- doc(section, "Terminal Tools").

:- export(using_tty/0).
:- pred using_tty # "The standard input is an interactive terminal.".
% TODO: the implementation is not portable
% TODO: probabily, this should live in other module
using_tty :-
	popen('(stty > /dev/null) 2>&1', read, S),
	stream_to_string(S, "").

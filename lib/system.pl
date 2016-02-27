:- module(system, 
        [
            pause/1,
            time/1,
            datime/1,
            datime/9,
            datime_struct/1,
            getenvstr/2,
            extract_paths/2,
            get_pid/1,
            current_host/1,
            current_executable/1,
            umask/2,
            make_directory/2,
            make_directory/1,
	    make_dirpath/2,
	    make_dirpath/1,
            working_directory/2,
            cd/1,
            shell/0,
            shell/1,
            shell/2,
            system/1,
            system/2,
            popen/3,
            popen_mode/1,
            exec/4,
            exec/3,
            directory_files/2,
            mktemp/2,
            file_exists/1,
            file_exists/2,
            file_property/2,
            file_properties/6,
            modif_time/2,
            modif_time0/2,
            fmode/2,
            chmod/2,
            chmod/3,
            delete_file/1,
            delete_directory/1,
            rename_file/2,
	    cyg2win/3
        ],
	[assertions, isomodes, regtypes]).

:- comment(title, "Operating system utilities").

:- comment(author, "Daniel Cabeza").
:- comment(author, "Manuel Carro").

:- comment(module, "This module contains predicates for invoking
   services which are typically provided by the operating system.
   Note that the predicates which take names of files or directories
   as arguments in this module expect atoms, not @concept{path
   alias}es. I.e., generally these predicates will not call
   @pred{absolute_file_name/2} on names of files or directories taken
   as arguments.").

:- use_module(engine(internals), ['$unix_popen'/3, '$exec'/4]).
:- use_module(library(lists), [append/3]).

:- impl_defined([
        working_directory/2, directory_files/2, pause/1, time/1, datime/9,
        current_host/1, getenvstr/2, get_pid/1, 
        current_executable/1,
        shell/0, shell/2, system/2, mktemp/2, file_exists/2,
        file_properties/6, chmod/2, umask/2, 
        delete_file/1, delete_directory/1, rename_file/2, make_directory/2]).


:- multifile define_flag/3.

%% This flag determines which path style is returned by the system
%% predicates which can return paths.  'os_dependent' means paths 
%% will be returned in the forma0-t the OS accepts it.  'posix' means
%% using a POSIX-style, with slashes, no drive letters or UNC paths,
%% and semicolons as separators.

define_flag(path_style, [posix, os_dependent], os_dependent).


:- comment(pause(Seconds), "Make this thread sleep for some @var{Seconds}.").

:- true pred pause(+int).

:- comment(time(Time), "@var{Time} is unified with the number of seconds
     elapsed since January, 1, 1970 (UTC).").

:- true pred time(?int).

 %% :- comment(walltime(Time),"@var{Time} is unified with the time in
 %%      milliseconds elapsed in the real world since the last call to
 %%      @pred{walltime/1}. The first call returns a meaningless number.").
 %% 
 %% :- true pred walltime(?int).

:- comment(datime(Datime), "@var{Datime} is unified with a term of the
     form @tt{datime(Year,Month,Day,Hour,Minute,Second)} which contains
     the current date and time.").

:- true pred datime(?datime_struct).

datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
        datime(_, Year, Month, Day, Hour, Min, Sec, _, _).

:- true regtype datime_struct/1.

datime_struct(datime(Year,Month,Day,Hour,Min,Sec)) :-
        int(Year), int(Month), int(Day), int(Hour), int(Min), int(Sec).

:- comment(datime(Time,Year,Month,Day,Hour,Min,Sec,WeekDay,YearDay),
	"@var{Time} is as in @pred{time/1}. @var{WeekDay} is the number
	of days since Sunday, in the range 0 to 6.  @var{YearDay} is the
	number of days since January 1, in the range 0 to 365.").

:- true pred datime(+int,?int,?int,?int,?int,?int,?int,?int,?int)
        # "If @var{Time} is given, the rest of the arguments are unified
        with the date and time to which the @var{Time} argument refers.".

:- true pred datime(-int,?int,?int,?int,?int,?int,?int,?int,?int)
	# "Bound @var{Time} to current time and the rest of the
	arguments refer to current time.".

:- comment(getenvstr(Name, Value), "The environment variable @var{Name}
    has @var{Value}.  Fails if variable @var{Name} is not defined.").

:- true pred getenvstr(+atm, ?string).

:- comment(extract_paths(String, Paths), "Interpret @var{String} as the
   value of a UNIX environment variable holding a list of paths and
   return in @var{Paths} the list of the paths.  Paths in @var{String}
   are separated by colons, and an empty path is considered a shorthand
   for '.' (current path).  The most typical environment variable with this
   format is PATH.  For example, this is a typical use:
@begin{verbatim}
?- set_prolog_flag(write_strings, on).

yes
?- getenvstr('PATH', PATH), extract_paths(PATH, Paths).

PATH = "":/home/bardo/bin:/home/clip/bin:/opt/bin/:/bin"",
Paths = [""."",""/home/bardo/bin"",""/home/clip/bin"",""/opt/bin/"",""/bin""] ?

yes
?- 
@end{verbatim}
").

:- true pred extract_paths(+string, ?list(string)).

extract_paths([], ["."]).
extract_paths([C|Cs], [Path|Paths]) :-
        extract_path(C, Cs, ".", Path, Cs_),
        extract_paths_(Cs_, Paths).

extract_paths_([], []).
extract_paths_([_|Cs], Paths) :- % skip ":"
        extract_paths(Cs, Paths).

extract_path(0':, Cs, Path, Path, [0':|Cs]) :- !.
extract_path(C, [], _, [C], []) :- !.
extract_path(C, [D|Cs], _, [C|Path], Cs_) :-
        extract_path(D, Cs, [], Path, Cs_).


:- comment(get_pid(Pid), "Unifies @var{Pid} with the process
     identificator of the current process or thread.").

:- true pred get_pid(?int).

:- comment(current_host(Hostname), "@var{Hostname} is unified with the
        fully qualified name of the host.").

:- true pred current_host(?atm).

:- comment(current_executable(Path), "Unifies @var{Path} with the path
        to the current executable.").

:- true pred current_executable(?atm).

:- comment(umask(OldMask, NewMask), "The process file creation mask was
    @var{OldMask}, and it is changed to @var{NewMask}.").

:- true pred umask(?int, +int).

:- true pred umask(OldMask, NewMask)
        : (var(OldMask), var(NewMask), OldMask == NewMask)
       => (int(OldMask), int(NewMask))
        # "Gets the process file creation mask without changing it.".

:- comment(working_directory(OldDir, NewDir),"Unifies current working
     directory with @var{OldDir}, and then changes the working
     directory to @var{NewDir}. Calling
     @tt{working_directory(Dir,Dir)} simply unifies @tt{Dir} with the
     current working directory without changing anything else.").

:- true pred working_directory(?atm, +atm).

:- true pred working_directory(OldDir, NewDir)
        : (var(OldDir), var(NewDir), OldDir == NewDir) => atm * atm
        # "Gets current working directory.".

:- comment(cd(Path), "Changes working directory to @var{Path}.").

:- true pred cd(+atm).

cd(Dir) :- working_directory(_, Dir).

:- true pred shell # "Execs the shell specified by the environment
   variable @tt{SHELL}. When the shell process terminates, control is
   returned to Prolog.".

:- comment(shell(Command), "@var{Command} is executed in the shell
    specified by the environment variable @tt{SHELL}. It succeeds if
    the exit code is zero and fails otherwise.").

%%      On MSDOS or Windows, if `SHELL' is defined it is expected to
%%      name a UNIX like shell which will be invoked with the argument `-c
%%      COMMAND'. If `SHELL' is undefined, the shell named by `COMSPEC'
%%      will be invoked with the argument `/C COMMAND'.

:- true pred shell(+atm).

shell(Path) :- shell(Path, 0).

:- comment(shell(Command, ReturnCode), "Executes @var{Command} in the
     shell specified by the environment variable @tt{SHELL} and stores
     the exit code in @var{ReturnCode}.").

:- true pred shell(+atm, ?int).

:- comment(system(Command), "Executes @var{Command} using the shell
        @apl{/bin/sh}.").

:- true pred system(+atm).

system(Path) :- system(Path, _Status).

:- comment(system(Command, ReturnCode), "Executes @var{Command} in the
     @apl{/bin/sh} shell and stores the exit code in @var{ReturnCode}.").

:- true pred system(+atm, ?int).


:- comment(exec(Command, StdIn, StdOut, StdErr), "Starts the process
@var{Command} and returns the standart I/O streams of the process in
@var{StdIn}, @var{StdOut}, and @var{StdErr}.").

:- true pred exec(+atm, -stream, -stream, -stream).

exec(Command, StdIn, StdOut, StdErr):- 
        '$exec'(Command, StdIn, StdOut, StdErr).

:- comment(exec(Command, StdIn, StdOut), "Starts the process
@var{Command} and returns the standart I/O streams of the process in
@var{StdIn} and @var{StdOut}. @tt{Standard error} is connected to
whichever the parent process had it connected to.").

:- true pred exec(+atm, -stream, -stream).

exec(Command, StdIn, StdOut):- 
        '$exec'(Command, StdIn, StdOut, []).

 %% :- comment(bug, "When reading from a exec'ed process, the 'end of
 %% file' condition (when the launched process finishes) somehow
 %% propagates to the standard input of the Ciao Prolog process, thus
 %% causing subsequent Ciao Prolog reads to return the 'end of file'
 %% condition.").

:- comment(popen(Command, Mode, Stream), "Open a pipe to process
    @var{Command} in a new shell with a given @var{Mode} and return a
    communication @var{Stream} (as in UNIX @tt{popen(3)}). If
    @var{Mode} is @tt{read} the output from the process is sent to
    @var{Stream}. If @var{Mode} is @tt{write}, @tt{Stream} is sent as
    input to the process. @var{Stream} may be read from or written
    into using the ordinary stream I/O predicates. @var{Stream} must
    be closed explicitly using @pred{close/1}, i.e., it is not closed
    automatically when the process dies.

").

:- true pred popen(+atm, +popen_mode, -stream).

popen(Command, Mode, S) :-
	nonvar(Mode),
	popen_mode(Mode),
	atom(Command), !,
	'$unix_popen'(Command, Mode, S0), !, S=S0.

:- regtype popen_mode(M)
  # "@var{M} is 'read' or 'write'.".

popen_mode(read).
popen_mode(write).

:- comment(directory_files(Directory, FileList), "@var{FileList} is the
        unordered list of entries (files, directories, etc.) in
        @var{Directory}.").

:- true pred directory_files(+atm,?list(atm)).

:- comment(mktemp(Template, Filename), "Returns a unique @var{Filename}
    based on @var{Template}: @var{Template} must be a valid file name
    with six trailing X, which are substituted to create a new file
    name.").

:- true pred mktemp(+atm, ?atm).

:- comment(file_exists(File), "Succeeds if @var{File} (a file or
        directory) exists (and is accessible).").

:- true pred file_exists(+atm).

file_exists(Path) :- file_exists(Path, 0).

:- comment(file_exists(File, Mode), "@var{File} (a file or directory)
        exists and it is accessible with @var{Mode}, as in the Unix
        call @tt{access(2)}. Typically, @var{Mode} is 4 for read
        permission, 2 for write permission and 1 for execute
        permission.").

:- true pred file_exists(+atm, +int).

:- comment(file_property(File, Property), "@var{File} has the property
   @var{Property}. The possible properties are:

@begin{description}

@item{type(@var{Type})} @var{Type} is one of @tt{regular}, @tt{directory},
      @tt{symlink}, @tt{fifo}, @tt{socket} or @tt{unknown}.

@item{linkto(@var{Linkto})} If @var{File} is a symbolic link,
      @var{Linkto} is the file pointed to by the link (and the other
      properties come from that file, not from the link itself).

@item{mod_time(@var{ModTime})} @var{ModTime} is the time of last
      modification (seconds since January, 1, 1970).

@item{mode(@var{Protection})} @var{Protection} is the protection mode.

@item{size(@var{Size})} @var{Size} is the size.

@end{description}

   If @var{Property} is uninstantiated, the predicate will enumerate the
   properties on backtracking.").

:- true pred file_property(+atm, ?struct).

file_property(Path, Property) :-
        file_property_(Property, Path).

file_property_(Property, Path) :-
        var(Property), !,
        file_properties(Path, Type, Linkto, Time, Protection, Size),
        ( Property = type(Type)
        ; Linkto \== '', Property = linkto(Linkto)
        ; Property = mod_time(Time)
        ; Property = mode(Protection)
        ; Property = size(Size)
        ).
file_property_(type(Type), Path) :- !,
        file_properties(Path, Type0, [], [], [], []),
        Type = Type0.
file_property_(linkto(File), Path) :- !,
        file_properties(Path, [], File0, [], [], []),
        File0 \== '',
        File = File0.
file_property_(mod_time(Time), Path) :- !,
        file_properties(Path, [], [], Time, [], []).
file_property_(mode(Protection), Path) :- !,
        file_properties(Path, [], [], [], Protection, []).
file_property_(size(Size), Path) :- !,
        file_properties(Path, [], [], [], [], Size).
file_property_(Other, _) :-
        throw(error(domain_error(file_property_type,Other),
                    file_property/2-2)).

:- comment(file_properties(Path, Type, Linkto, Time, Protection, Size),
        "The file @var{Path} has the following properties:

@begin{itemize} 

@item File type @var{Type} (one of @tt{regular}, @tt{directory},
      @tt{symlink}, @tt{fifo}, @tt{socket} or @tt{unknown}).

@item If @var{Path} is a symbolic link, @var{Linkto} is the file pointed
      to.  All other properties come from the file pointed, not the
      link.  @var{Linkto} is '' if @var{Path} is not a symbolic link.

@item Time of last modification @var{Time} (seconds since January, 1,
      1970).

@item Protection mode @var{Protection}.

@item Size in bytes @var{Size}.

@end{itemize}
").

:- true pred file_properties(+atm, ?atm, ?atm, ?int, ?int, ?int).

:- comment(modif_time(File, Time), "The file @var{File} was last
     modified at @var{Time}, which is in seconds since January, 1,
     1970. Fails if @var{File} does not exist.").

:- true pred modif_time(+atm, ?int).

modif_time(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], Time, [], []) ->
            set_prolog_flag(fileerrors, OldFE)
        ; set_prolog_flag(fileerrors, OldFE),
          fail
        ).

:- comment(modif_time0(File, Time), "If @var{File} exists, @var{Time} is
      its latest modification time, as in @pred{modif_time/2}.
      Otherwise, if @var{File} does not exist, @var{Time} is zero.").


:- true pred modif_time0(+atm, ?int).

modif_time0(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], T, [], []), !
        ; T = 0
        ),
        set_prolog_flag(fileerrors, OldFE),
        Time = T.

:- comment(fmode(File, Mode), "The file @var{File} has protection mode
        @var{Mode}.").

:- true pred fmode(+atm, ?int).

fmode(Path, Mode) :-
        file_properties(Path, [], [], [], Mode, []).

:- comment(chmod(File, NewMode), "Change the protection mode of file
        @var{File} to @var{NewMode}.").

:- true pred chmod(+atm, +int).

:- comment(chmod(File, OldMode, NewMode), "The file @var{File} has
    protection mode @var{OldMode} and it is changed to @var{NewMode}.").

:- true pred chmod(+atm, ?int, +int).

:- true pred chmod(File, OldMode, NewMode)
        : (atm(File), var(OldMode), var(NewMode), OldMode == NewMode)
        => atm * atm * atm
        # "Equivalent to fmode(@var{File},@var{OldMode})".

chmod(Path, OldMode, NewMode) :-
        OldMode == NewMode, !,
        fmode(Path, OldMode).
chmod(Path, OldMode, NewMode) :-
        fmode(Path, OldMode),
        chmod(Path, NewMode).


:- comment(delete_directory(File), "Delete the directory @var{Directory}.").

:- pred delete_directory(+atm).


:- comment(delete_file(File), "Delete the file @var{File}.").

:- pred delete_file(+atm).


:- comment(rename_file(File1, File2), 
        "Change the name of  @var{File1} to @var{File2}.").

:- pred rename_file(+atm,+atm).

:- comment(make_directory(DirName, Mode), 
        "Creates the directory @var{DirName} with a given @var{Mode}.  This is, as usual, operated against the current umask value.").

:- pred make_directory(+atm, +int).

:- comment(make_directory(DirName),
        "Equivalent to @tt{make_directory(D,0o777)}.").

:- pred make_directory(+atm).

make_directory(D) :-
        make_directory(D,0o777).

:- pred make_dirpath(+atm, +int).

:- comment(make_dirpath(Path, Mode),
        "Creates the whole @var{Path} for a given directory with a given @var{Mode}. As an example, @tt{make_dirpath('/tmp/var/mydir/otherdir')}."). 

make_dirpath(Path, Mode) :-
        atom_codes(Path, PathCodes),
        (          % If relative, transform it into absolute
            PathCodes = "/"||_ ->
            AbsPathCodes = PathCodes
        ;
            working_directory(CurrentDir, CurrentDir),
            atom_codes(CurrentDir, CurrentDirCodes),
            append(CurrentDirCodes, "/"||PathCodes, AbsPathCodes)
        ),
        make_abs_dir(AbsPathCodes, '', Mode).
% 
% Making the intermediate directories: instead of cd'ing to
% directories (which is something which can break and modify the
% program state), we construct incrementally the intermediate directories.
% The idea here is to traverse the absolute path and construct 
% partial paths, which are appended to an atom which represents the
% (initially empty) path.  Depending on the implementation of
% atom_concat/3 (and of the atoms), this can be done quite fast if the
% first argument does not need to be completely traversed.

% End of path
make_abs_dir("", _, _Mode):- !.
% The recursive case: perform a step in the recursion and construct the
% intermediate directory.
make_abs_dir(Path, IncPath, Mode):-
        decompose(Path, RestPath, Component-[]),
        % Transform into atom and add to partial path
        atom_codes(PartComp, Component),
        atom_concat(IncPath, PartComp, NewPath),
        (
            file_exists(NewPath) ->
            true
        ;
            make_directory(NewPath, Mode)
        ),
        make_abs_dir(RestPath, NewPath, Mode).

% Collapse double slashes into only one (that is the Linux/Unix convention)
decompose("//"||PathWOSlash, RestPath, Queue):- !, 
        decompose("/"||PathWOSlash, RestPath, Queue).
decompose("/"||PathWOSlash, RestPath, "/"||Queue-TailQ):-
        decompose_aux(PathWOSlash, RestPath, Queue-TailQ).
decompose_aux("", "", Q-Q).
decompose_aux("/"||P, "/"||P, Q-Q):- !.
decompose_aux([P|Ps], RestP, [P|RestQ]-TailQ):- 
        decompose_aux(Ps, RestP, RestQ-TailQ).



 %% make_dirpath(Path, Mode) :-
 %% 	working_directory(CurrentDir, CurrentDir),
 %% 	make_dirpath_aux(Path, Mode),
 %% 	working_directory(_, CurrentDir).
 %% 
 %% make_dirpath_aux(Path, Mode) :-
 %% 	atom_concat(Head, Tail, Path),
 %% 	atom_concat('/', SubTail, Tail), !,
 %% 	(Head = '' ->
 %% 	 working_directory(CurrentDir, '/')
 %% 	;
 %% 	 ((file_exists(Head), file_property(Head, type(directory)))
 %% 	  ;
 %% 	   make_directory(Head, Mode) 
 %% 	 ),
 %% 	 working_directory(CurrentDir, Head)),
 %% 	make_dirpath_aux(SubTail, Mode).
 %% make_dirpath_aux(Dir, Mode) :-
 %% 	make_directory(Dir, Mode).
 %% 	 



:- pred make_dirpath(+atm).

:- comment(make_dirpath(Path),
                "Equivalent to @tt{make_dirpath(D,0o777)}.").

:- pred make_dirpath(+atm).
	  
make_dirpath(Path) :-
        make_dirpath(Path, 0o777) .

:- comment(cyg2win(+CygWinPath, ?WindowsPath, +SpawSlash), "Converts a
path in the CygWin style to a Windows-style path, rewriting the driver
part.  If @var{SwapSlash} is @tt{swap}, slashes are converted in to
backslash.  If it is @tt{noswap}, they are preserved.").

cyg2win("/cygdrive/"||[D,0'/ | Dir], [D,0':| Path],Swap) :- !,
						             % New Drive notat.
        swapslash(Swap,[0'/ | Dir],Path).
cyg2win("//"||[D,0'/ | Dir], [D,0':,0'\\ | Path],Swap) :- !, % Drive letter
        swapslash(Swap,Dir,Path).
cyg2win("//"||Dir, "\\\\"||Path,Swap) :- !,                  % Network drive
        swapslash(Swap,Dir,Path).
cyg2win(Dir,  [D,0': |Path],Swap) :-                         % Default drive
        swapslash(Swap,Dir,Path),
        default_drive(D).

default_drive(D) :-
        getenvstr('COMSPEC',[D|_]), !. % Shell command interpreter' drive
default_drive(0'C).                    % If not defined, assume C

swapslash(noswap,Dir,Dir) :-
	!.
swapslash(swap,Dir,Path) :-
	do_swapslash(Dir,Path).

do_swapslash([],[]).
do_swapslash([0'/|D],[0'\\|ND]) :- !,
        do_swapslash(D,ND).
do_swapslash([C|D],[C|ND]) :-
        do_swapslash(D,ND).


:- comment(version_maintenance,dir('../version')).

:- comment(version(1*7+181,2002/01/25,20:14*06+'Hora estándar
   romance'), "cyg2win/3 moved here.  ()").

:- comment(version(1*7+169,2002/01/03,17:57*18+'CET'), "Changed
   make_dirpath to make it completely deterministic.  (MCL)").

:- comment(version(1*7+158,2001/11/27,10:12*07+'CET'), "make_dirpath/1 and
   make_dirpath/2 added to system (Jose Manuel Gomez Perez)").

:- comment(version(1*3+50,1999/09/08,22:35*15+'MEST'), "Commented and
   exported. Other minor changes in comments. @prop{datime_struct/1}.
   (Manuel Hermenegildo)").

:- comment(version(1*3+34,1999/07/14,20:09*38+'MEST'), "Added datime/1
   and datime/9.  (Daniel Cabeza Gras)").

:- comment(version(0*9+73,1999/04/30,13:27*15+'MEST'), "Fixed minor
   bug in comment.  (Manuel Hermenegildo)").

:- comment(version(0*9+54,1999/04/21,21:54*24+'MEST'), "Added
   extract_paths/2 predicate.  (Daniel Cabeza Gras)").

:- comment(version(0*9+43,1999/04/08,20:26*58+'MEST'), "Added
   file_property/2 and file_properties/6 predicates, last one
   implemented in emulator (Daniel Cabeza Gras)").

:- comment(version(0*7+24,1998/10/22,15:50*41+'MEST'), "Implemented
   delete_file/1 in emulator.  (Daniel Cabeza Gras)").

:- comment(version(0*7+23,1998/10/22,13:31*37+'MEST'), "Added chmod/2
   and fmode/2 and changed chmod/3 to avoid a bug when getting modes
   from other users files.  (Daniel Cabeza Gras)").

:- comment(version(0*7+1,1998/09/15,16:00*12+'MEST'), "Added
   documentation for all the predicates in the module (MCL).").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").


:- module(dirutils, [], [hiord, assertions]).

:- doc(title, "Common Operations on Directories").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jose F. Morales").

:- doc(bug, "Both the interface of this module and the implementation need some work").

:- use_module(library(terms)).
:- use_module(library(lists), [append/3, list_concat/2, reverse/2]).
:- use_module(library(messages)).
:- use_module(library(system), [working_directory/2, file_exists/1,
		file_properties/6]).
:- use_module(library(filenames),  [atom_or_str/1]).
%:- use_module(library(make(system_extra)), [do_atmlist__popen/2]).
:- use_module(library(make(system_extra))).

% Note: we do not need the whole @tt{regexp} package, just this predicate
:- use_module(library(regexp(regexp_code)), [match_pred/2]).

:- reexport(library(system), [file_exists/1]).

% ===========================================================================

:- doc(section, "Paths to Files and Directories").

% TODO: Reimplement as path_concat? (or similar?)
:- export(path_name/2).
:- pred path_name(A, B) : atm * atm
   # "@var{B} is @var{A} concatenating '/' if needed. ".

path_name('', Dir) :- !, Dir = ''.
path_name(Dir, DirName) :- atom_concat(_, '/', Dir), !, DirName = Dir.
path_name(Dir, DirName) :- atom_concat(Dir, '/', DirName).

:- export(path_concat/3).
path_concat(Dir, '.', Dir).
path_concat(A,   B,   R  ) :-
	atom_concat(_, '/', A),
	!,
	atom_concat(A, B, R).
path_concat(A, B, R) :-
	atom_concat(A,  '/', A0),
	atom_concat(A0, B,   R ).

:- export(path_list_concat/2).
% TODO: this is not tail-recursive, fix
path_list_concat([], '') :- !.
path_list_concat([X], X) :- !.
path_list_concat([X|Xs], R) :-
	path_list_concat(Xs, B),
	path_concat(X, B, R).

% TODO: Document
:- export(clean_path/2).
clean_path(P, PC) :-
	atom(P),
	!,
	atom_codes(P, AC),
	clean_path(AC, ACC),
	atom_codes(PC, ACC).
clean_path(P, PC) :-
	reverse(P, PR),
	clean_path__(PR, PR1),
	reverse(PR1, PC).

clean_path__(P, PC) :-
	append(B4,      "../" || AF, P),
	append(_DELETE, "/" || REST, AF),
	!,
	clean_path__(REST, REST_CLEAN),
	append(B4, REST_CLEAN, PC).
clean_path__(A, A).

:- export(split_path_and_name/3).
:- pred split_path_and_name(CompletePath, Path, FileName)
	:: atom_or_str * atom_or_str * atom_or_str
# "@var{FileName} and @{Path} are the file corresponding file
           name and (directory) path to the given @var{CompletePath}.
Example:
@begin{verbatim}
?- split_path_and_name( '/mydir/myfile' , Path , File ).

File = myfile,
Path = '/mydir/' ? ;

no
?- split_path_and_name( '/mydir/myfile.txt' , Path , File ).

File = 'myfile.txt',
Path = '/mydir/' ? ;

no
?- split_path_and_name( '/mydir/myfile.txt/' , Path , File ).

File = '',
Path = '/mydir/myfile.txt/' ? 

yes
?- split_path_and_name( 'myfile.txt' , Path , File ).

File = 'myfile.txt',
Path = ./ ? ;

no
?- split_path_and_name( '/mydir/../myfile.txt' , Path , File ).

File = 'myfile.txt',
Path = '/mydir/../' ? ;

no
?- split_path_and_name( '/mydir/..myfile.txt' , Path , File ).

File = '..myfile.txt',
Path = '/mydir/' ? ;

no
?- split_path_and_name( '/mydir/..myfile.txt/' , Path , File ).

File = '',
Path = '/mydir/..myfile.txt/' ? ;

no
@end{verbatim}".

split_path_and_name(CP, P, F) :-
	atom(CP),
	atom_codes(CP, PasStr),
	!,
	split_path_and_name_str(PasStr, PathStr, FasStr),
	clean_path(PathStr, PathCleanStr),
	atom_codes(P, PathCleanStr),
	atom_codes(F, FasStr).
split_path_and_name(CP, PC, F) :-
	split_path_and_name_str(CP, P, F),
	clean_path(P, PC).

split_path_and_name_str([],         "./",  []).
split_path_and_name_str(".." || CP, RPath, RFile) :-
	!,
	split_path_and_name_str(CP, Path, File),
	(
	    Path = "./"
	->
	    RPath = Path,
	    RFile = ".." || File
	;
	    RPath = ".." || Path,
	    RFile = File
	).
split_path_and_name_str([0'/|CP], RPath, RFile) :-
	!,
	split_path_and_name_str(CP, Path, File),
	(
	    Path = "./"
	->
	    RPath = [0'/]
	;
	    RPath = [0'/|Path]
	),
	(RFile = File -> true ; true).
split_path_and_name_str([C|CP], RPath, RFile) :-
	split_path_and_name_str(CP, Path, File),
	(
	    Path = "./"
	->
	    RPath = Path,
	    RFile = [C|File]
	;
	    RPath = [C|Path],
	    RFile = File
	).

:- export(filename_ext/2).
:- pred filename_ext(FileName, Ext)
	: atom_or_str(FileName)
	=> atom_or_str(Ext)
# "@var{Ext} is the extension for a given @var{FileName} filename. It
  is specilized version of @var{basename/3}".

filename_ext(FileName, Ext) :-
	atom(FileName),
	!,
	atom_codes(FileName, FileNameStr),
	filename_ext_str(FileNameStr, ExtStr),
	atom_codes(Ext, ExtStr).
filename_ext(FileName, Ext) :-
	filename_ext_str(FileName, Ext).

filename_ext_str(FileName, Ext) :-
	filename_ext_str__(FileName, Ext),
	(Ext= "" -> true ; true).

filename_ext_str__([0'., FC|R], Ext) :-
	!,
	filename_ext_str__(R, Ext),
	(
	    (FC = 0'/ ; FC = 0'\\ ; FC = 0'.)
	->
	    true
	;
	    (Ext = [FC|R] -> true ; true)
	).
filename_ext_str__([FC|R], Ext) :-
	(FC = 0'/ ; FC = 0'\\),
	!,
	filename_ext_str__(R, Ext),
	(Ext = [] -> true ; true).
% Path ends with a dot
filename_ext_str__([0'.], []) :-
	!.
filename_ext_str__([_|Cs], Ext) :-
	filename_ext_str__(Cs, Ext).
filename_ext_str__([], _).

:- export(basename_ext/3).
:- pred basename_ext(FileNamePath, BaseName, Ext)
	: atom_or_str(FileNamePath)
	=> (atom_or_str(BaseName), atom_or_str(Ext))
# "Concatenating @var{BaseName} and @var{Ext} (with a dot in middle if
  needed) we obtain @var{FileNamePath}. @var{Ext} is the possible
  extension of @var{FileNamePath}. Examples:

@begin{verbatim}
?- basename_ext( \"../mydir.tet/myfile.ext./\" , Base , Ext ).

Base = \"../mydir.tet/myfile.ext./\",
Ext = [] ? ;

no
?- basename_ext( \"../mydir.tet/myfile.ext.\" , Base , Ext ).

Base = \"../mydir.tet/myfile.ext\",
Ext = [] ? ;

no
?- basename_ext( \"../mydir.tet/myfile.ext\" , Base , Ext ).

Base = \"../mydir.tet/myfile\",
Ext = \"ext\" ? ;

no
?- basename_ext( \"../mydir.tet/myfile.\" , Base , Ext ).

Base = \"../mydir.tet/myfile\",
Ext = [] ? ;

no
?- basename_ext( \"/mydir.tet/myfile.\" , Base , Ext ).

Base = \"/mydir.tet/myfile\",
Ext = [] ? ;

no
?- basename_ext( \"mydir.tet/myfile.\" , Base , Ext ).

Base = \"mydir.tet/myfile\",
Ext = [] ? ;

no
?- basename_ext( \"mydir.tet/myfile.tar.gz\" , Base , Ext ).

Base = \"mydir.tet/myfile.tar\",
Ext = \"gz\" ? ;

no
@end{verbatim}".

basename_ext(FileNamePath, BaseName, Ext) :-
	atom(FileNamePath),
	!,
	atom_codes(FileNamePath, FileNamePathStr),
	basename_ext_str(FileNamePathStr, BaseNameStr, ExtStr),
	atom_codes(BaseName, BaseNameStr),
	atom_codes(Ext,      ExtStr).
basename_ext(FileNamePath, BaseName, Ext) :-
	basename_ext_str(FileNamePath, BaseName, Ext).


basename_ext_str(Path, BaseName, Ext) :-
	basename_ext_str__(Path, BaseName, Ext),
	(Path = BaseName -> true ; true),
	(Ext = "" ->        true ; true).

basename_ext_str__([0'., FC|R], BaseName, Ext) :-
	R \== [],
	!,
	basename_ext_str__(R, BN, Ext),
	(
	    (FC = 0'/ ; FC = 0'\\ ; FC = 0'.)
	->
	    BaseName = [0'., FC|BN]
	;
	    (
		Ext = [FC|R]
	    ->
		BaseName = []
	    ;
		BaseName = [0'., FC|BN]
	    )
	).
basename_ext_str__([FC|R], BaseName, Ext) :-
	(FC = 0'/ ; FC = 0'\\),
	!,
	basename_ext_str__(R, BN, Ext),
	(Ext = [] -> true ; true),
	BaseName = [FC|BN].
% Path ends with a dot
basename_ext_str__([0'.], [], []) :-
	!.
basename_ext_str__([C|Cs], BaseName, Ext) :-
	basename_ext_str__(Cs, BN, Ext),
	(var(Ext) -> BaseName = BN ; BaseName = [C|BN]).
basename_ext_str__([], _, _).

:- export(basename/2).
:- pred basename(FileNamePath, BaseName)
# "Equivalent to @tt{basename_ext( FileNamePath , BaseName , _ )}.".

basename(FileNamePath, BaseName) :-
	basename_ext(FileNamePath, BaseName, _).

% ===========================================================================

:- doc(section, "Directory Checks").

:- export(is_dir/1).
is_dir(FileName) :-
	\+(file_property(FileName, linkto(_))),
	file_exists(FileName),
	file_property(FileName, type(directory)).

:- export(is_dir/2).
is_dir(Dir, Path) :-
	( Dir = '.' -> true
	; Dir = './'
	),
	file_properties(Path, regular, _Linkto, _Time, _Protection, _Size),
	!.
is_dir(Dir, _File) :-
	file_properties(Dir, directory, _Linkto, _Time, _Protection, _Size).

% ===========================================================================

:- doc(section, "Path Normalization"). % (from relative)

is_absolute(File) :-
	atom_codes(File, [47|_]). % 47 = /

is_home(File) :-
	atom_codes(File, [0'~|_]).

expand_home(File, Path) :-
	list_files(File, [Path]),
	!.

get_abs_or_rel_path(File, Path) :-
	get_path_to_create(File, Path),
	file_exists(Path).

:- export(get_abs_path/2).
get_abs_path(File, Path) :-
	get_abs_path_no_check(File, Path),
	file_exists(Path).

:- export(get_abs_path_no_check/2).
get_abs_path_no_check(File, Path) :-
	( is_absolute(File) ->
	    Path = File
	; ( is_home(File) ->
	      expand_home(File, Path)
	  ; working_directory(CWD0, CWD0),
	    path_name(CWD0, CWD),
	    atom_concat(CWD, File, Path)
	  )
	).

:- export(get_abs_or_rel_path_with_wildcards/2).
get_abs_or_rel_path_with_wildcards(File, Path) :-
	get_abs_or_rel_path(File, Path),
	!.
get_abs_or_rel_path_with_wildcards(Pattern, Path) :-
	working_directory(CWD0, CWD0),
	path_name(CWD0, CWD),
	list_files(CWD, Pattern, Files),
	member(File, Files),
	get_abs_path(File, Path).

:- export(get_path_to_create/2).
get_path_to_create(File, Home) :-
	is_home(File),
	!,
	expand_home(File, Home).
get_path_to_create(File, File) :-
	is_absolute(File),
	!.
get_path_to_create(File, PathClean) :-
	working_directory(CWD0, CWD0),
	path_name(CWD0, CWD),
	atom_concat(CWD, File, Path),
	clean_path(Path, PathClean).

% ===========================================================================

:- doc(section, "Search in Directories").

:- export(has_wildcards/1).
has_wildcards(A) :-
	( atom(A) ->
	    atom_codes(A, AC),
	    member(0'*, AC)
	; member(0'*, AC)
	),
	!.

:- export(list_files/2).
:- pred list_files(DirPattern, Files) : atm(DirPattern) => list(Files, atm)
# "List files in @var{Dir} (with may contain a path pattern
  @var{Pattern}). Return a list of atom in @var{Files} with the
  files".

list_files(DirPattern, Files) :-
% 	atom_concat('ls -d ', DirPattern, Command),
	% TODO: This needs bash!
	atom_concat([
		'for i in ', DirPattern, ' ; do ',
		' if [ -e \"$i\" ] ; then ',
		'  echo "$i" ; ',
		' fi; ',
		'done'], Command),
%	display( Command ),nl,
	do_atmlist__popen(Command, Files).

:- export(list_files/3).
list_files(CWD, DirPattern, Files) :-
	% TODO: This should be an operation on itself...
	(
	    (is_absolute(DirPattern) ; is_home(DirPattern))
	->
	    Path = DirPattern
	;
	    atom_concat(CWD, DirPattern, Path)
	),
	%
	list_files(Path, Files).

% ===========================================================================

% TODO: Unused
%% :- export(copy_files_or_dirs/2).
%% copy_files_or_dirs(FilesOrDirs, Dir) :-
%% 	copy_files_or_dirs(FilesOrDirs, Dir, [overwrite, timestamp]).
%% 
%% :- export(copy_files_or_dirs/3).
%% copy_files_or_dirs([],           _DestDir, _CopyOptions).
%% copy_files_or_dirs([File|Files], DestDir,  CopyOptions) :-
%% 	copy_file_or_dir(File, DestDir, CopyOptions),
%% 	copy_files_or_dirs(Files, DestDir, CopyOptions).

:- doc(section, "Delete Directory Contents").

% TODO: only used in lpdoc, but useful as a general predicate
:- export(delete_files_and_dirs/1).
:- pred delete_files_and_dirs(FilesOrDirs)
	: list(FilesOrDirs, atm)

# "Delete Files and/or Direcories.".

delete_files_and_dirs(Dirs) :- list(Dirs, delete_file_or_dir).

% TODO: This is equivalent to delete_dir_rec! Remove!
:- pred delete_file_or_dir/1 : atm.
delete_file_or_dir(Dir) :-
	( is_dir(Dir) ->
	    delete_dir_rec(Dir)
	;
	    delete_file(Dir)
	).

:- export(delete_dir_rec/1). % TODO: move to system_extra?
:- doc(delete_dir_rec(Directory), "Deletes the file or directory
   @var{Directory} and all the files and subdirectories
   recursively.").

delete_dir_rec(Dir) :-
	( is_dir(Dir) ->
	    path_name(Dir, Dir1),
	    find(Dir1, find_delete_file_, dir_before_dummy_, find_delete_directory_),
	    -delete_directory(Dir)
	; file_exists(Dir) ->
	    delete_file(Dir)
	; true
	).

find_delete_file_(BaseDir, File) :-
	atom_concat(BaseDir, File, F),
	-delete_file(F),
	fail.

find_delete_directory_(BaseDir, File) :-
	atom_concat(BaseDir, File, F),
	-delete_directory(F).

% ===========================================================================

:- doc(section, "Copy Directories").

% TODO: used only in makedir_component
:- export(copy_file_or_dir/3).
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

% TODO: Used in autodoc_html_resources and several makedir (this one is general)
:- export(copy_dir_rec/8).
copy_dir_rec(BaseDir0, DestDir0, Pattern, SkipFiles, SkipDirs,
	    SkipSubDirs, NDFiles, CopyOptions) :-
	copy_dir_rec(BaseDir0, DestDir0, _, Pattern,
	    SkipFiles, SkipDirs, SkipSubDirs, NDFiles, CopyOptions).

% TODO: Used in autodoc_html_resources and several makedir (this one is general)
:- export(copy_dir_rec/9).
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
	( match_pred(SkipFiles, File) ->
	    fail
	; match_pred(Pattern, File) ->
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
	; true
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

:- export(copy_dir_rec/2).
copy_dir_rec(BaseDir, DestDir) :-
	copy_dir_rec(BaseDir, DestDir, [overwrite, timestamp]).

:- export(copy_dir_rec/4).
copy_dir_rec(BaseDir, DestDir, Perms, CopyOptions) :-
	copy_dir_rec(BaseDir, DestDir, Perms, '*', '', '', '', [],
	    CopyOptions).

:- export(copy_dir_rec/3).
copy_dir_rec(BaseDir, DestDir, CopyOptions) :-
	copy_dir_rec(BaseDir, DestDir, '*', '', '', '', [], CopyOptions).

% ===========================================================================

:- doc(section, "Changing Attributes").

:- export(set_owner_rec/2).
:- pred set_owner_rec(Dir, Group) # "Set each file group to
   @var{Group} while traversing the directory @var{Dir} recursively".

set_owner_rec(Dir, Owner) :-
	( nonvar(Owner), Owner = grp(_, _) ->
	    (
		set_owner(Dir, Owner),
		path_name(Dir, FDir),
		current_find(FDir, dir_before_dummy_, dir_after_dummy_, _, _, _, FileName),
		set_owner(FileName, Owner),
		fail
	    ;
		true
	    )
	;
	    throw(error(instantiation_error, set_perm_rec/2 -2))
	).

% ===========================================================================

:- doc(section, "Finding Files (recursively)").

:- export(find/4).
:- meta_predicate find(?, pred(2), pred(2), pred(2)).

find(BaseDir, FileCondition, DirBefore, DirAfter) :-
	( current_find(BaseDir, DirBefore, DirAfter, file, CurrentDir,
	    CurrentFile, _CurrentFileName),
	  FileCondition(CurrentDir, CurrentFile),
	  fail
        ; true
	).

:- export(dir_before_dummy_/2).
dir_before_dummy_(_, _).
:- export(dir_after_dummy_/2).
dir_after_dummy_(_, _).

:- export(dir_before_display_/2).
dir_before_display_(BaseDir, File) :-
	display_list(['Entering ', BaseDir, File, '\n']).

:- export(dir_after_display_/2).
dir_after_display_(BaseDir, File) :-
	display_list(['Leaving  ', BaseDir, File, '\n']).

% TODO: not currently used?
:- export(file_display_/2).
file_display_(BaseDir, File) :-
	display_list(['Process  ', BaseDir, File, '\n']).

:- export(current_find/7).
:- meta_predicate current_find(?, pred(2), pred(2), ?, ?, ?, ?).
current_find(BaseDir0, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName) :-
	path_concat(BaseDir0, '', BaseDir),
	current_find_(BaseDir, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName).

:- meta_predicate current_find_(?, pred(2), pred(2), ?, ?, ?, ?).
current_find_(BaseDir, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName) :-
	directory_files(BaseDir, Files),
	member(File, Files),
	\+ member(File, ['.', '..']),
	current_find_rec(BaseDir, File, DirBefore, DirAfter,
	    CurrentType, CurrentDir, CurrentFile, CurrentFileName).

:- export(current_find_rec/8).
% TODO: missing meta_predicate (wrong behaviour due to local calls; bad performance)
current_find_rec(BaseDir, File, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName) :-
	atom_concat(BaseDir, File, FileName),
	(
	    is_dir(FileName) ->
	    (
		CurrentType = dir,
		CurrentDir = BaseDir,
		CurrentFile = File,
		CurrentFileName = FileName
	    ;
		atom_concat(FileName, '/', BaseSubDir),
		current_find_dir(BaseDir, File, BaseSubDir, DirBefore,
		    DirAfter, CurrentType, CurrentDir, CurrentFile,
		    CurrentFileName)
	    )
	;
	    CurrentType = file,
	    CurrentDir = BaseDir,
	    CurrentFile = File,
	    CurrentFileName = FileName
	).

:- export(current_find_dir/9).
% TODO: missing meta_predicate (wrong behaviour due to local calls; bad performance)
current_find_dir(BaseDir, File, BaseSubDir, DirBefore, DirAfter,
	    CurrentType, CurrentDir, CurrentFile, CurrentFileName) :-
	DirBefore(BaseDir, File),
	(
	    current_find_(BaseSubDir, DirBefore, DirAfter, CurrentType,
	        CurrentDir, CurrentFile, CurrentFileName)
	;
	    DirAfter(BaseDir, File),
	    fail
	).

% find('/home/edison/mnt/workspace/JavaTest/',file_display_, dir_before_display_, dir_after_display_).

% ===========================================================================

% TODO: Rename
:- export(is_distributable_dir/6).
is_distributable_dir(CurrBaseDir, BaseDir, SkipDirs, SkipSubDirs, NDFiles, Dir) :-
	( member(NDFile, NDFiles),
	  atom_concat([CurrBaseDir, Dir, '/', NDFile], NDFileName),
	  file_exists(NDFileName) ->
	  fail
	; atom_concat(BaseDir,   RelSubDir, CurrBaseDir),
	  atom_concat(RelSubDir, Dir,       SubDir),
	  \+(match_pred(SkipDirs,    Dir)),
	  \+(match_pred(SkipSubDirs, SubDir))
	).

:- export(current_filter_files/9).
current_filter_files(BaseDir, Pattern, SkipFiles, SkipDirs, SkipSubDirs,
	    NDFiles, Dir, File, FileName) :-
	current_find(BaseDir, is_distributable_dir(BaseDir, SkipDirs,
		SkipSubDirs, NDFiles), dir_after_dummy_, file, Dir, File,
	    FileName),
	list_filter_file_(Dir, File, Pattern, SkipFiles).

list_filter_file_(Dir, File, Pattern, SkipFiles) :-
	atom_concat(Dir, File, FileName),
	\+(file_property(FileName, linkto(_))),
	\+(match_pred(SkipFiles,   File)),
	match_pred(Pattern, File),
	!.
	

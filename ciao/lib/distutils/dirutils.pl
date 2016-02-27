:- module(dirutils, [
		has_wildcards/1,
		get_abs_path/2,
		get_abs_path_no_check/2,
		clean_path/2,
		get_path_to_create/2,
		split_path_and_name/3,
		update_cwd/1,
		get_cwd/1,
		get_abs_or_rel_path_with_wildcards/2,
		basename/2,
		basename_ext/3,
		filename_ext/2,
		list_files/3,
		list_files/2,
		is_dir/2
	    ],
	    [assertions]).

:- doc(module, "Several directory utilities.").

:- doc(bug, "This functionality should be rewritten to be part of the Ciao libraries").

:- use_module(library(terms)).
:- use_module(library(lists), [append/3, list_concat/2, reverse/2]).
:- use_module(library(messages)).
:- use_module(library(system), [working_directory/2, file_exists/1,
		file_properties/6]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(filenames),  [atom_or_str/1]).

:- reexport(library(system), [file_exists/1]).

% ---------------------------------------------------------------------------
% File work

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

:- pred basename(FileNamePath, BaseName)
# "Equivalent to @tt{basename_ext( FileNamePath , BaseName , _ )}.".

basename(FileNamePath, BaseName) :-
	basename_ext(FileNamePath, BaseName, _).

% ---------------------------------------------------------------------------

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

get_abs_path(File, Path) :-
	get_abs_path_no_check(File, Path),
	file_exists(Path).

get_abs_path_no_check(File, Path) :-
	(
	    is_absolute(File)
	->
	    Path = File
	;
	    (
		is_home(File)
	    ->
		expand_home(File, Path)
	    ;
		get_cwd(CWD),
		atom_concat(CWD, File, Path)
	    )
	).

get_abs_or_rel_path_with_wildcards(File, Path) :-
	get_abs_or_rel_path(File, Path),
	!.
get_abs_or_rel_path_with_wildcards(Pattern, Path) :-
	get_cwd(CWD),
	list_files(CWD, Pattern, Files),
	member(File, Files),
	get_abs_path(File, Path).

get_path_to_create(File, Home) :-
	is_home(File),
	!,
	expand_home(File, Home).
get_path_to_create(File, File) :-
	is_absolute(File),
	!.
get_path_to_create(File, PathClean) :-
	get_cwd(CWD),
	atom_concat(CWD, File, Path),
	clean_path(Path, PathClean).

:- data cwd/1.

is_dir(Dir, Path) :-
	( Dir = '.' -> true
	; Dir = './'
	),
	file_properties(Path, regular, _Linkto, _Time, _Protection, _Size),
	!.
is_dir(Dir, _File) :-
	file_properties(Dir, directory, _Linkto, _Time, _Protection, _Size).

update_cwd(CWD) :-
	atom(CWD),
	!,
	retractall_fact(cwd(_)),
%	display( set_cwd( CWD ) ),nl,
	asserta_fact(cwd(CWD)).
update_cwd(CWD) :-
	message(error, ['update_cwd: ', CWD, ' is not an atom']),
	fail.

get_cwd(CWD) :-
	current_fact(cwd(CWD)),
	!.
get_cwd(CWD2) :-
	working_directory(CWD, CWD),
	atom_concat(CWD, '/', CWD2).
% was:
%get_cwd( './' ).

% save_and_get_path( File , FileC , OldCWD ) :-
% 	get_cwd( OldCWD ),
% 	atom_concat( OldCWD , File , FileC ),
% 	save_path( FileC ),
% %	display( reading_file( FileC ) ), nl,
% 	!.
%
% save_and_get_path( File , _ , _ ) :-
% 	get_cwd( CWD ),
% 	error_message( "Specified File ~w~w does not exists" , [ CWD , File ] ),
% 	update_cwd( '' ),
% 	fail.

has_wildcards(A) :-
	(
	    atom(A)
	->
	    atom_codes(A, AC),
	    member(0'*, AC)
	;
	    member(0'*, AC)
	),
	!.

:- use_module(library(system)).
:- use_module(library(strings)).

:- set_prolog_flag(multi_arity_warnings, off).

:- pred list_files(DirPattern, Files) : atm(DirPatetrn) => list(Files, atm)
# "List files in @var{Dir} (with may contain a math pattern
  @var{Pattern}). Return a list of atom in @var{Files} with the
  files".

list_files(DirPattern, Files) :-
% 	atom_concat('ls -d ', DirPattern, Command),
	atom_concat([
		'for i in ', DirPattern, ' ; do ',
		' if [ -e \"$i\" ] ; then ',
		'  echo "$i" ; ',
		' fi; ',
		'done'], Command),
%	display( Command ),nl,
	popen(Command, read, Stream),
	read_files(Stream, Files).
list_files(CWD, DirPattern, Files) :-
	(
	    (is_absolute(DirPattern) ; is_home(DirPattern))
	->
	    Path = DirPattern
	;
	    atom_concat(CWD, DirPattern, Path)
	),
	list_files(Path, Files).

:- set_prolog_flag(multi_arity_warnings, on).

read_files(Stream, Files) :-
	get_line(Stream, F),
	!,
	(
	    F = end_of_file
	->
	    Files = []
	;
	    atom_codes(AF, F),
	    Files = [AF|Fs],
	    read_files(Stream, Fs)
	).
read_files(_, []).


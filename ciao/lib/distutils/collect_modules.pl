:- module(collect_modules, [collect_modules/2, current_dir_module/2,
		current_dir_module/4, current_dir_module/6],
	    [assertions, regexp]).


:- use_module(library(distutils(find))).
:- use_module(library(distutils(skip_settings))).

:- use_module(library(aggregates)).
:- use_module(library(read),       [read/2]).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(system),     [file_exists/1]).
:- use_module(library(lists),      [append/3]).
:- use_module(library(llists),     [append/2]).
:- use_module(library(file_utils), [file_to_string/2]).


:- pred collect_modules(BaseDir, Modules) : atm(BaseDir) => list(Modules, atm)

# "Returns in @var{Modules} a list of all modules found in
	@var{BaseDir} directory. Modules from directories containing a
	@tt{NOCOMPILE} file or whose name match with any of the
	patterns listed in the file @tt{NOCOMPILEFILES} will not be
	taken into account. ".

collect_modules(BaseDir, Modules) :-
	findall(FileName, current_dir_module(BaseDir, FileName), Modules).


:- pred current_dir_module(BaseDir, Module) : atm(BaseDir) => atm(Modules)

# "Same as @pred{collect_modules/2} but returns modules in
  @var{Module} by fail.".

current_dir_module(BaseDir, FileName) :-
	current_dir_module(BaseDir, _, _, FileName).

current_dir_module(BaseDir, Dir, File, FileName) :-
	nocompile_dirs(NoDirs),
	nocompile_files(NoFiles),
	current_dir_module(BaseDir, NoDirs, NoFiles, Dir, File, FileName).

current_dir_module(BaseDir, NoDirs, NoFiles, Dir, File, FileName) :-
	current_find(BaseDir, collect_modules_dircond(NoDirs),
	    dir_after_dummy_, file, Dir, File, FileName),
	match_pred('*.pl', File),
	collect_modules_filecond(Dir, NoFiles, File).

match_pred_list([Pattern|_], File) :-
	match_pred(Pattern, File),
	!.
match_pred_list([_|Patterns], File) :-
	match_pred_list(Patterns, File).

collect_modules_filecond(BaseDir, NoFiles, File) :-
	get_exclude_list(BaseDir, NoFiles, ExcludeList),
	\+ match_pred_list(ExcludeList, File),
	atom_concat(BaseDir, File, FileName),
	open(FileName, read, Stream),
	( catch(read(Stream, Term),
		E,
		warning(['File ', FileName,
			' raised the following exception:', E])
	    ) -> true ; true ),
	close(Stream),
	!,
	nonvar(Term),
	( (Term = (:- module(_, _, _)) ; Term = (:- module(_, _))) ->
	    true ).

get_exclude_list(BaseDir, NoFiles, ExcludeList) :-
	findall(ExcludeList0,
	    (
		member(NoFile, NoFiles),
		atom_concat(BaseDir, NoFile, AbsNoFiles),
		file_exists(AbsNoFiles),
		file_to_string(AbsNoFiles, ExcludeListS),
		strings_atoms(ExcludeListS, 0'\n, ExcludeList0)
	    ), ExcludeLists),
	append([['*_co.pl', 'SETTINGS.pl', 'Makefile.pl']
		|ExcludeLists],
	    ExcludeList).

collect_modules_dircond(BaseDir, NCFiles, Dir) :-
	\+ (skip_dirs(SkipDirT), match_pred(SkipDirT, Dir)),
	(
	    member(NCFile, NCFiles),
	    atom_concat([BaseDir, Dir, '/', NCFile], NCFileName),
	    file_exists(NCFileName) ->
	    fail
	;
	    true
	).

strings_atoms("", _, []) :-
	!.
strings_atoms(String, Separator, [Atom|List]) :-
	append(Line, [Separator|Tail], String),
	atom_codes(Atom, Line),
	!,
	strings_atoms(Tail, Separator, List).
strings_atoms(Line, _, [Atom]) :-
	atom_codes(Atom, Line).

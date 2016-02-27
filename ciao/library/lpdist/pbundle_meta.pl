:- module(pbundle_meta, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

:- use_module(library(read)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(terms), [atom_concat/2]).

:- doc(module, "Handling of meta-information files for pbundle"). 

% TODO: *********************************************************************
% TODO: * If the desc.tmpl format changes, older files need to be modified! *
% TODO: *********************************************************************

:- regtype pbundle_meta(M)
   # "@var{M} is the description of a @concept{packaged bundle}".
pbundle_meta(_). % TODO: define

% ===========================================================================
% Normalizing pbundle description reader

% TODO: Not really templates, do not use 'tmpl' name. They are plain
%       var/value lists.

:- export(pbundle_meta_load/2).
:- pred pbundle_meta_load(File, Meta) :: atm * pbundle_meta 
   # "@var{Meta} is the result of loading @var{File}".
pbundle_meta_load(AbsFile) := PMeta :-
	open(AbsFile, read, Stream),
	read_tmpl(Stream, PMeta0),
	close(Stream),
	( atom_concat(BaseDir, '/desc.tmpl', AbsFile) ->
	    PMeta = [basedir = BaseDir|PMeta0]
	; PMeta = PMeta0
	).

read_tmpl(Stream, Out) :-
	read(Stream, R),
	!,
	( R = end_of_file ->
	    Out = []
	; Out = [R|Rs],
	  read_tmpl(Stream, Rs)
	).

% ===========================================================================

:- export(pbundle_meta_attr/3).
pbundle_meta_attr(PMeta, A, V) :-
	member((A=V), PMeta),
	!.

:- export(pbundle_meta_has_name/2).
% (Name must be an atom)
pbundle_meta_has_name(PMeta, Name) :- atom(Name), !,
	BaseDir = ~pbundle_meta_attr(PMeta, basedir),
	atom_concat([_, '/', Name], BaseDir).

% ---------------------------------------------------------------------------

:- use_module(library(sort)).
:- use_module(library(format)).

% Sort a list of PMeta by its version number (decreasing)
:- export(sort_pbundle_metas_by_version/2).
sort_pbundle_metas_by_version(PMetas) := SortedPMetas :-
	KTs = ~add_version_key(PMetas),
	KTs2 = ~sort(KTs),
	KTs3 = ~reverse(KTs2),
	SortedPMetas = ~seconds(KTs3).

add_version_key([], []).
add_version_key([T|Ts], [(K,T)|KTs]) :-
	K = ~pkgmeta_version_key(T),
	add_version_key(Ts, KTs).

seconds([], []).
seconds([(_,X)|Xs], [X|Ys]) :- seconds(Xs, Ys).

pkgmeta_version_key(T) := K :-
	K = ~version_to_key(~pbundle_meta_attr(T, pbundle_version)).

% Decompose a version atom to obtain a 'key' string
% TODO: This key is only used to sort the packages. But I can sort in simpler ways!
version_to_key(Version) := Key :-
	( G='-' ; G='' ),
	( atom_concat([G, P, '.', SV, '.', Pa, '#', SVN], Version)
	; atom_concat([G, P, '.', SV, '.', Pa, '-', SVN], Version)
	; atom_concat([G, P, '.', SV, '.', Pa], Version), SVN = 0
	; atom_concat([G, P, '.', SV, 'p', Pa], Version), SVN = 0
	),
	!,
	fill_with_zero(P,  1, PS),
	fill_with_zero(SV, 3, SVS),
	fill_with_zero(Pa, 3, PaS),
	fill_with_zero(SVN, 6, SVNS),
	sformat(Str, "~s~s~s~s", [PS, SVS, PaS, SVNS]),
	number_codes(Key, Str).

fill_with_zero(V, Z, Key) :-
	sformat(Str, "~w", [V]),
	length(Str, L),
	M is Z - L,
	( M > 0 -> sformat(Key, "~*c~s", [M, 0'0, Str])
	; Key = Str
	).

% ---------------------------------------------------------------------------

:- use_module(library(system)).

% Time of the pbundle in days (since 'year zero')
:- export(pbundle_meta_time/2).
pbundle_meta_time(PMeta) := Time :-	
	PackageDate = ~pbundle_meta_attr(PMeta, pbundle_date),
	atom_concat([AYear, '-', AMonth, '-', ADay, ' ',
		AHour, ':', AMinute, ':', ASeconds], PackageDate),
	atom_number(AYear,    Year),
	atom_number(AMonth,   Month),
	atom_number(ADay,     Day),
	atom_number(AHour,    Hour),
	atom_number(AMinute,  Minute),
	atom_number(ASeconds, Seconds),
	datime(Time0, Year, Month, Day, Hour, Minute, Seconds, _, _),
	Time is Time0 // 86400. % seconds in a day

% ---------------------------------------------------------------------------

:- use_module(library(dirutils)).

:- doc(section, "Operations on Collections of pbundle_meta").

:- export(load_pbundle_metas/3).
% Load all the packaged bundle metadata found in a given directory
% @var{PDir} for a branch @var{Branch}
load_pbundle_metas(Branch, PDir0) := AllPMetas :-
	PDir = ~atom_concat([PDir0, '/', Branch]),
	AllPackageF = ~matching_files(~atom_concat(PDir, '/*/desc.tmpl')),
	AllPMetas = ~load_pbundle_metas_(AllPackageF, Branch).

load_pbundle_metas_([], _Branch, []).
load_pbundle_metas_([F|Fs], Branch, [V|Vs]) :-
	% TODO: is there a better way to adding branch metadata?
	V = ~append(~pbundle_meta_load(F), [branch = Branch]),
	load_pbundle_metas_(Fs, Branch, Vs).

:- use_module(library(lists), [append/3]).

matching_files(Pattern) := Files :-
	findall(File, enum_matching_file(Pattern, File), Files).

enum_matching_file(FileC, RealFile) :-
	has_wildcards(FileC),
	!,
	get_abs_or_rel_path_with_wildcards(FileC, RealFile),
	% Avoid */current/... because it is a symbolic link
	% TODO: get 'current' name from a single definition
	\+ atom_concat(_, '/current/desc.tmpl', RealFile).
enum_matching_file(FileC, RealFile) :-
	( get_abs_path(FileC, RealFile) ->
	    true
	; message(error, ['File ', FileC, ' not found\n'])
	).

% ---------------------------------------------------------------------------

:- export(newest_pbundle_meta/2).
newest_pbundle_meta(AllPMetas) := PMeta :-
	SortedPMetas = ~sort_pbundle_metas_by_version(AllPMetas),
	SortedPMetas = [PMeta|_].

% ---------------------------------------------------------------------------

% Lookup a pbundle_meta
:- export(lookup_pbundle_meta/3).
lookup_pbundle_meta(Rev, AllPMetas) := PMeta :-
	% Find a pbundle_meta whose 'basedir' param is Rev
	member(PMeta, AllPMetas),
	pbundle_meta_has_name(PMeta, Rev),
	!.

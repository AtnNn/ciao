:- module(distpkg_meta, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

:- use_module(library(read)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(terms), [atom_concat/2]).

:- doc(module, "Handling of meta-information files for distpkg"). 

% ===========================================================================
% Normalizing distpkg description reader
% TODO: Not really templates, but... var/value lists

% TODO: I added some normalization here for package description
%   metafiles Ideally, we should store packages in a normalized
%   way. (JF -- compat with old versions)

% TODO: ************************************************************************
% TODO: * Package description metafiles MUST be refined to avoid normalization *
% TODO: ************************************************************************

:- export(distpkg_load_meta/2).
distpkg_load_meta(AbsFile) := PkgMeta :-
	open(AbsFile, read, Stream),
	read_tmpl(Stream, PkgMeta0),
	close(Stream),
	( atom_concat(BaseDir, '/desc.tmpl', AbsFile) ->
	    PkgMeta = [basedir = BaseDir|PkgMeta0]
	; PkgMeta = PkgMeta0
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

:- export(distpkg_attr/3).
distpkg_attr(PkgMeta, A, V) :-
	member((A=V), PkgMeta),
	!.

:- export(distpkg_has_name/2).
% (Name must be an atom)
distpkg_has_name(PkgMeta, Name) :- atom(Name), !,
	BaseDir = ~distpkg_attr(PkgMeta, basedir),
	atom_concat([_, '/', Name], BaseDir).

% ---------------------------------------------------------------------------

:- use_module(library(sort)).
:- use_module(library(format)).

% Sort a list of PkgMeta by its version number (decreasing)
:- export(sort_pkgmetas_by_version/2).
sort_pkgmetas_by_version(PkgMetas) := SortedPkgMetas :-
	KTs = ~add_version_key(PkgMetas),
	KTs2 = ~sort(KTs),
	KTs3 = ~reverse(KTs2),
	SortedPkgMetas = ~seconds(KTs3).

add_version_key([], []).
add_version_key([T|Ts], [(K,T)|KTs]) :-
	K = ~pkgmeta_version_key(T),
	add_version_key(Ts, KTs).

seconds([], []).
seconds([(_,X)|Xs], [X|Ys]) :- seconds(Xs, Ys).

pkgmeta_version_key(T) := K :-
	K = ~version_to_key(~distpkg_attr(T, distpkg_version)).

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

% Time of the distpkg in days (since 'year zero')
:- export(distpkg_time/2).
distpkg_time(PkgMeta) := Time :-	
	PackageDate = ~distpkg_attr(PkgMeta, distpkg_date),
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

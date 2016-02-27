:- module(svn_tools, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

% TODO: what about contrib/subversion/subversion.pl?

:- doc(module, "SVN tools"). 

:- doc(summary, "This module defines predicate to interact with a
SVN repository").

:- use_module(library(lists)).
:- use_module(library(make(system_extra)), [do_str/3]).

% ---------------------------------------------------------------------------

:- export(svn_revision_time/3).
:- pred svn_revision_time(+string, +string, ?atm).
% TODO: This command should be simplified
svn_revision_time(Repository, Revision0, SvnTimeAtom) :-
	atom_codes(ARepository, Repository),
	just_revision_number(Revision0, Revision),
	atom_codes(ARevision, Revision),
	do_str([
		'svn info ', ARepository, ' --xml -r ', ARevision,
		' |grep \"<date>\" |',
		' sed -e s:\"<date>\"::g -e s:\"</date>\"::g'],
	    fail, SvnTime0),
	!,
	length(Date, 10),
	length(Time, 8),
	append(Date, ~append([_|Time], _), SvnTime0),
	append(Date, " " || Time,          SvnTime),
	%
	atom_codes(SvnTimeAtom, SvnTime).

% svntime(Repository, Revision, SvnTime) :-
% 	do_str(['svn info ', Repository], fail, SvnInfo),
% 	Pattern = [_, _, _, _|"-" || [_, _|"-" || [_, _|" "
% 		    || [_, _|":"|| [_, _|":"||[_, _|
% 				Tail]]]]]],
% 	Tail = " " || [_, _, _, _, _|_],
% 	append(_B,      Pattern, SvnInfo),
% 	append(SvnTime, Tail,    Pattern),
% 	!.
% svntime(_, "0000-00-00 00:00:00").

just_revision_number(Revision0, Revision) :-
	append(Revision, [C|_], Revision0),
	\+ is_digit(C),
	!.
just_revision_number(Revision, Revision).

is_digit(X) :- X >= 0'0, X =< 0'9.

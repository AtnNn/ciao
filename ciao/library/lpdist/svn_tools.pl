:- module(svn_tools, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

% TODO: what about contrib/subversion/subversion.pl?

:- doc(module, "SVN tools"). 

:- doc(summary, "This module defines predicate to interact with a
SVN repository").

:- use_module(library(lists)).
:- use_module(library(system_extra), [do_str/3]).

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

% ---------------------------------------------------------------------------

:- use_module(library(system_extra), [do_str_without_nl__popen/2]).
:- use_module(library(terms), [atom_concat/2]).

% TODO: avoid 'grep' and 'sed'
:- export(svn_repository_root/2).
:- pred svn_repository_root(+Path, ?Root) :: atom * string 
   # "The path @var{Path} is part of a working copy of the 
      repository @var{Root}.".

svn_repository_root(Path) :=
	~do_str_without_nl__popen(
           ~atom_concat(['svn info ',
	     Path,
	     ' 2>/dev/null|grep "URL: "|sed -e s/\'URL: \'//g'])).

% ---------------------------------------------------------------------------

:- export(svn_revision_string/2).
:- pred svn_revision_string(+Path, ?Rev) :: atom * string 
   # "Obtain the revision number @var{Rev} as a string".

% TODO: can we avoid 'which'?
svn_revision_string(Path) := Rev :-
	do_str_without_nl__popen(
          ~atom_concat([
	    'which svnversion > /dev/null 2>&1 && ',
	    'svnversion ', Path
	  ]), Rev0),
	%
	( Rev0 == "" -> Rev = "exported"
	; Rev = Rev0
	),
	% Check that we are in a SVN checkout.
	% This will match 'Unversioned...' or 'Uncommited...',
	% see 'svnversion' documentation for details.
	\+ Rev = "Un"||_.


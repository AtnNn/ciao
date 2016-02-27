:- module(svn_tools, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

% TODO: what about contrib/subversion/subversion.pl?

:- doc(title, "SVN tools"). 
:- doc(author, "Jose F. Morales").
:- doc(author, "Edison Mera (original author)").

:- doc(summary, "This module defines predicates to interact with a SVN
   repository").

:- use_module(library(lists)).
:- use_module(library(system_extra), [do_str/3]).
:- use_module(library(lpdist(datetime))).
:- use_module(library(system)).
:- use_module(library(system_extra), [do_str_without_nl__popen/2]).
:- use_module(library(terms), [atom_concat/2]).

% ---------------------------------------------------------------------------

:- export(svn_repo_at_dir/1).
% There is a SVN repository at Dir
svn_repo_at_dir(Dir) :-
	file_exists(~atom_concat(Dir, '/.svn')).

% ---------------------------------------------------------------------------

:- export(svn_revision_date/3).
:- pred svn_revision_date(+atm, +atm, ?string).
% (Revision date string is in ISO 8601 format)
% TODO: This command should be simplified
svn_revision_date(Repository, Revision0, Date) :-
	just_revision_number(Revision0, Revision),
	do_str([
		'svn info ', Repository, ' --xml -r ', Revision,
		' |grep \"<date>\" |',
		' sed -e s:\"<date>\"::g -e s:\"</date>\"::g'],
	    fail, Date0),
	!,
	Date = Date0.

just_revision_number(Revision0, Revision) :-
	Revision1 = ~atom_codes(Revision0),
	append(Revision2, [C|_], Revision1),
	\+ is_digit(C),
	!,
	atom_codes(Revision, Revision2).
just_revision_number(Revision, Revision).

is_digit(X) :- X >= 0'0, X =< 0'9.

% ---------------------------------------------------------------------------

% TODO: avoid 'grep' and 'sed'
:- export(svn_repository_root/2).
:- pred svn_repository_root(+Path, ?Root) :: atm * atm
   # "The path @var{Path} is part of a working copy of the 
      repository @var{Root}.".

svn_repository_root(Path) := R :-
	S = ~do_str_without_nl__popen(
               ~atom_concat(['svn info ',
	         Path,
	         ' 2>/dev/null|grep "URL: "|sed -e s/\'URL: \'//g'])),
	atom_codes(R, S).

% ---------------------------------------------------------------------------

:- export(svn_get_revision/2).
:- pred svn_get_revision(+Path, ?Rev) :: atm * atm
   # "Obtain the revision number @var{Rev}".

% TODO: can we avoid 'which'?
svn_get_revision(Path) := Rev :-
	do_str_without_nl__popen(
          ~atom_concat([
	    'which svnversion > /dev/null 2>&1 && ',
	    'svnversion ', Path
	  ]), Rev0),
	%
	( Rev0 == "" -> Rev = 'exported'
	; % Check that we are in a SVN checkout.
	  % This will match 'Unversioned...' or 'Uncommited...',
	  % see 'svnversion' documentation for details.
	  \+ Rev0 = "Un"||_,
	  atom_codes(Rev, Rev0)
	).


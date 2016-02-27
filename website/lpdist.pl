:- module(lpdist, [main/1], [ciaopaths, dcg, assertions, regtypes, basicmodes, make, fsyntax]).

:- doc(title, "The lpdist Packaged Bundle Management Tool").

%:- doc(subtitle, "Packaged Bundle Management Tool for (C)LP Systems").
%:- doc(subtitle_extra, "@bf{The Ciao System Documentation Series}").
%:- doc(subtitle_extra, "Technical Report CLIP 5/97.1").
%:- doc(subtitle_extra, "@em{Draft generated on:} @today{}").

:- doc(author, "Jose F. Morales").
:- doc(author, "The CLIP Group").
:- doc(address, "@tt{clip@@dia.fi.upm.es}").
:- doc(address, "@tt{http://www.clip.dia.fi.upm.es/}").
:- doc(address, "Facultad de Inform@'{a}tica").
:- doc(address, "Universidad Polit@'{e}cnica de Madrid").

:- doc(summary, "This tool manages @index{packaged bundle}s.").

:- doc(module, "@cartouche{To be writen}").

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(errhandle), [handle_error/2]).
:- use_module(library(dirutils)).
:- use_module(library(messages)).

:- use_module(.(lpdist_wipe)).

:- doc(bug, "This program is in a very alpha stage.").
:- doc(bug, "Breaks silently if desc.tmpl cannot be read (or contains wrong terms).").

% ---------------------------------------------------------------------------
% (Definitions for this application -- used in lpsettings_based_app)

:- include(library(lpsettings_based_app)).

% TODO: Many of those definitions should come from the documentation
%       of this file.

% The application name
app_name(lpdist).
% Version atom
version('0.1').
% Copyright
app_copyright("CLIP Group, T.U. of Madrid (UPM)").

app_options_message("
LPdist options:
    fetch Branch Rev   Fetch a revision Rev from the given Branch (e.g.,
                       master, 1.14, 1.14.0) in the
                       pbundle repository to the local storage.

    fetch-latest       Fetch the latest revision (just for 'master').
    query-latest       Query the latest revision from the repository
                       (just for 'master').

    wipe-list Branch   Obtain the list of pbundle to wipe from the local
                       storage for a given Branch
").

%% ----------------------------------------------------------------------------

default_branch('master').

start(['fetch', Branch, Rev]) :- !,
	pbundle_fetch(Branch, Rev).
%
start(['fetch-latest']) :- !,
	Branch = 'master', % TODO: branch is hardwired at this moment
	query_latest_rev(Rev),
	pbundle_fetch(Branch, Rev).
start(['query-latest']) :- !,
	query_latest_rev(_).
%
start(['wipe-list', Branch]) :- !, % TODO: only wipe in the 'master'
	CodeDir = ~get_value(pbundle_localpkgdir),
	DocsDir = ~get_value(pbundle_localdocdir),
	Keep = [], % TODO: Do not force keeping any revision
	pbundles_to_wipe(CodeDir, Branch, Keep, RDists),
	( % (failure-driven loop)
	  member(F, RDists),
	    atom_concat(CodeDir, R, F),
	    atom_concat(DocsDir, R, F2),
	    display(F), nl,
	    display(F2), nl,
	    fail
	; true
	).
start(_) :-
	error_message("Unknown command, use --help").

%% ----------------------------------------------------------------------------

:- use_module(ciaobot(ciaobot), [showlpv/1]).
:- use_module(library(system_extra), [do_str_without_nl/3]).

% TODO: Rewrite ciaobot in Prolog (at least the interface). 
% TODO: In order to support large processes without crashes,
%       introduce an simple method to invoke predicates as separte processes.
%       E.g. :- pred foo + own_process, or +spawn.

% TODO: Only query the latest revision for 'master' (it is hardwired in showlpv)
query_latest_rev(Latest) :-
	% Querying latest pbundle revision
	format(user_error, "Connecting to the Ciao Bot...~n", []),
	% TODO: fix directory
	ciaobot:showlpv(String),
	format(user_error, "Latest pbundle revision is '~s'~n", [String]),
	atom_codes(Latest, String).

%% ----------------------------------------------------------------------------

:- use_module(library(system_extra)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lpdist(pbundle_meta))).
:- use_module(library(lpdist(pbundle_generator)), [pbundle_file_kind_ext/2]).

% TODO: Allow symbolic names, not just revisions
% TODO: This is not publish; this is just fetch from the repo and move
%       to the bundle dir; we need to move to the HTML directory
%       instead.
pbundle_fetch(Branch, RevAtom) :-
	( Branch = '' -> throw(bad_branch) ; true ),
	check_var_exists(pbundle_repository),
	RepoDir = ~get_value(pbundle_repository),
	atom_concat([RepoDir, '/', Branch, '/', RevAtom], BDir),
	%
	pbundle_fetch_(BDir, Branch, RevAtom).

% Fetch a pbundle from BDir (it may be a SSH location)
pbundle_fetch_(BDir, Branch, RevAtom) :-
	% Fetch the metafile
	fetch_metafile(BDir, Branch, RevAtom, Result),
	( Result = new(Meta) ->
	    % Copy all pbundle elements specified in the metafile
	    ( % (failure driven loop)
	      enum_pbundle_item(Meta, D),
	        fetch_item(BDir, Branch, RevAtom, Meta, D),
	        fail
	    ; true
	    )
        ; format(user_error, "The revision had already been fetched.~n", [])
        ).

% Fetch the metafile and obtain the loaded metafile (@tt{new(Meta)})
% result or @tt{old} if the revision was already fetched.
fetch_metafile(BDir, Branch, RevAtom, Result) :-
	check_var_exists(pbundle_localpkgdir),
	LocalDir = ~get_value(pbundle_localpkgdir),
	atom_concat([LocalDir, '/', Branch, '/', RevAtom], TargetDir),
	%
	( file_exists(TargetDir) ->
	    Result = old
	; make_dirpath(TargetDir),
	  MetaName = 'desc.tmpl',
	  ssh_copy(MetaName, BDir, TargetDir),
	  MetaFile = ~atom_concat([TargetDir, '/', MetaName]),
	  pbundle_meta_load(MetaFile, Meta),
	  Result = new(Meta)
	).

% enumerate (on backtracking) all elements (files) that will be part
% of the pbundle
enum_pbundle_item(Meta, D) :-
	( Ds = ~pbundle_meta_attr(Meta, code)
	; Ds = ~pbundle_meta_attr(Meta, docs)
	),
	member(D, Ds).

fetch_item(BDir, Branch, RevAtom, Meta, D) :-
	D = pbundle_item(Kind, _, File),
	LocalDir = ~pbundle_root(Kind),
	atom_concat([LocalDir, '/', Branch, '/', RevAtom], TargetDir),
	make_dirpath(TargetDir),
	%
	format(user_error, "Fetching item ~w...~n", [File]),
	ssh_copy(File, BDir, TargetDir),
	create_item_links(Kind, LocalDir, Branch, RevAtom, Meta, File, TargetDir).

% Copy (with SSH) a file from SrcDir to TargetDir
ssh_copy(NameExt, SrcDir, TargetDir) :-
	do(['scp -r ', SrcDir, '/', NameExt, ' ', TargetDir, '/', NameExt], nofail).

% Create symbolic links to the item (for some item kinds and branches)
% TODO: create symbolic links in the documentation
% TODO: Either move to the webpage generation code or rewrite in the
%       form of an 'bundle activation' code.
%% create_item_links(Kind, LocalDir, Branch, RevAtom, _Meta, File, TargetDir) :-
%% 	display(create_item_links(Kind, LocalDir, Branch, RevAtom, File, TargetDir)), nl,
%% 	fail.
create_item_links(Kind, LocalDir, Branch, RevAtom, Meta, File, _TargetDir) :-
	% Link to the latest source pbundle
	Kind = tar_gz,
	Branch = master,
	!,
	pbundle_meta_attr(Meta, packname, PName), % TODO: This could be improved
        pbundle_file_kind_ext(Kind, Ext),
	% The link file
	atom_concat([LocalDir, '/', Branch, '/', PName, '-latest', '.', Ext], Link),
	% The original file (relative to the link)
	atom_concat([RevAtom, '/', File], Rel),
	%
	del_file_nofail(Link),
	copy_file(Rel, Link, [overwrite, symlink]).
create_item_links(Kind, LocalDir, Branch, RevAtom, _Meta, File, TargetDir) :-
	% Link to the latest HTML documentation
	Kind = manual_html,
	Branch = master,
	!,
	% Get main file for the manual
	atom_concat([TargetDir, '/', File], Manual),
	manual_mainfile(Manual, Main),
	% The link file
	atom_concat([LocalDir, '/', Main], Link),
	% The original file (relative to the link)
	atom_concat([Branch, '/', RevAtom, '/', File], Rel),
	%format(user_error, "Linking ~w to ~w~n", [Rel, Link]),
	%
	del_file_nofail(Link),
	copy_file(Rel, Link, [overwrite, symlink]).
create_item_links(_, _, _, _, _, _, _).

:- use_module(library(system_extra),
	[copy_file/3, del_file_nofail/1]).

% ===========================================================================
% (DUPLICATED CODE)
% TODO: duplicated in pbundle_download.pl

% TODO: Move to some pbundle module
pbundle_kind_is_doc(manual_html).
pbundle_kind_is_doc(manual_pdf).

% The absolute pbundle root directory
pbundle_root(Kind) := Path :-
	( pbundle_kind_is_doc(Kind) ->
	    Path = ~get_value(pbundle_localdocdir)
	; Path = ~get_value(pbundle_localpkgdir)
	).

% Obtain the main file for the given HTML documentation
manual_mainfile(Path, Main) :-
	Fs = ~ls(Path, '*.htmlmeta'),
	( Fs = [F],
	  F2 = ~atom_concat([Path, '/', F]),
	  file_to_string(F2, Str),
	  atom_codes(Main, Str) ->
	    true
	; throw(bug('main file for manual not found'))
	).

:- use_module(library(system_extra), [ls/3]).
:- use_module(library(file_utils), [file_to_string/2]).

% Default settings for all manuals in CiaoDE
:- module(_, [settings_value/2, perms/1, initvals/0, docdir/1, docformat/1,
		docformatdir/2, settings_auto/1, svn_repository/1, lpdoclib/1,
		distpkg_revision/1, mandir/1, infodir/1,
		bibfile/1, htmldir/1],
	    [ciaopaths, assertions, fsyntax]).

% TODO: distpkg_revision/1 should be defined here or in other module?

:- use_module(library(persvalue),          [load_file/2, current_value/3]).
:- use_module(library(terms),              [atom_concat/2]).
:- use_module(library(make(system_extra)), [no_tr_nl/2, do_str_without_nl__popen/2]).
:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(component_registry), [component_src/2]).

:- use_module(ciaodesrc(makedir('ConfigValues')), [build_root/1]).

:- initialization(initvals).

settings_auto := ~atom_concat(~component_src(ciao), '/SETTINGS_AUTO').

initvals :- load_file(ciaosrcsettings, ~settings_auto).

% Define this to be the permissions for installed execs/dirs and data files:

% TODO: What does this comment mean?
% --- DTM: A variable is missing here
% TODO: Duplicated in config_stage1.pl (what would be the right place to put it?)
perms(perm(rwX, rwX, rX)).

% TODO: also computed in lpdoc/src/lpdoc.pl (ensure_lpdoclib_defined/0) 
%       (this cannot be a configurable setting, lpdoc knows where it lives)
lpdoclib := ~atom_concat([~component_src(lpdoc), '/lib']).

docdir := ~atom_concat([~build_root, ~settings_value('DOCDIR'), '/']).
htmldir := ~atom_concat([~build_root, ~settings_value('HTMLDIR'), '/']).
% ~atom_concat(~build_root, ~settings_value('HTMLURL')).
mandir := ~atom_concat([~build_root, ~settings_value('MANDIR'), '/']).
infodir := ~atom_concat([~build_root, ~settings_value('INFODIR'), '/']).
%% .bib files now in a predefined location (JFMC)
%%bibfile := ~decompose(~settings_value('BIBFILES'), ',').
% Bibliography files for documentation (clip.bib and general.bib)
bibfile := ~atom_concat([~component_src(ciaode), '/doc/bibtex/clip'])
	| ~atom_concat([~component_src(ciaode), '/doc/bibtex/general']).

/*
decompose(Text0, Separator, Element) :-
	atom_concat([Element0, ',', Text], Text0) ->
	(
	    Element = Element0
	;
	    decompose(Text, Separator, Element)
	)
    ;
	Element = Text0.
*/

docformatdir(html, Dir) :- !, htmldir(Dir).
docformatdir(manl, Dir) :- !, mandir(Dir).
docformatdir(info, Dir) :- !, infodir(Dir).
docformatdir(_,    Dir) :- docdir(Dir).

:- meta_predicate settings_value(?, addmodule).
settings_value(Name, Value, _) :-
	current_value(ciaosrcsettings, Name, Value0),
	!,
	Value = Value0.
settings_value(Name, _Value, Module) :-
	message(error, ['In module ', Module,
		', Could not find enviroment variable ',
		Name, ' defined in ', ~settings_auto]),
	fail.


:- pred svn_repository/1 # "Specifies the subversion repository
	where the sources can be obtained, or the working copy.".

% svn_repository := ~component_src(ciaode).
svn_repository :=
	"svn+ssh://clip.dia.fi.upm.es/home/clip/SvnReps/Systems/CiaoDE/trunk".

distpkg_revision := ~ciaode_revision_string.

:- export(svn_revision_atom/1).
svn_revision_atom(A) :-
	ciaode_revision_string(S),
	atom_codes(A, S).

svn_revision_string(Path, String) :-
	do_str_without_nl__popen(~atom_concat([
		    'which svnversion > /dev/null 2>&1 && svnversion ',
		    Path]), String0),
	(
	    String0 == "" ->
	    String = "exported"
	;
	    String = String0
	).

:- export(revision_file/1).
revision_file := ~atom_concat(~component_src(ciaode), '/REVISION').

% Note: svnversion is done only over installer to avoid overhead
get_svn_revision(Revision) :-
	svn_revision_string(~atom_concat(~component_src(ciaode),
		'/makedir'),
	    Revision).

:- export(ciaode_revision_string/1).
ciaode_revision_string(Revision) :-
	( get_svn_revision(Revision) -> true
	; show_message(warning, "get_svn_revision/1 should not fail.")
	),
	nonvar(Revision),
	Revision \== "exported",
	!.
ciaode_revision_string(Revision) :-
	revision_file(RevisionFile),
	file_exists(RevisionFile),
	!,
	no_tr_nl(~file_to_string(RevisionFile), Revision).
ciaode_revision_string("Unknown").

docformat := pdf|manl|info|html. % | ps.

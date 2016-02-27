:- module(_, _, [ciaopaths, make, fsyntax, assertions, regtypes]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- use_module(library(terms), [atom_concat/2]).

% ----------------------------------------------------------------------------
% Paths and options for components

% (Default from the system)
systempath := ~library_directory.
:- multifile library_directory/1.
:- dynamic library_directory/1.

pathsfile := _ :- fail. % not used
 
startpage := 1.
papertype := afourpaper.

% ---------------------------------------------------------------------------

output_name(_) :- fail. % not used
datamode(_) :- fail. % not used
execmode(_) :- fail. % not used
libtexinfo := yes.

filepath := ~atom_concat(~website_root_dir, ~websitepathref).

websitepathref := '/src'.

doc_structure := 
    'index'-[
      phony('about')-[
        'maillist',
        'license'
      ],
      phony('download')-[
        'download_stable',
        'download_latest',
        'download_legacy'
      ],
      phony('documentation')-[
%        'ciao',
%        'lpdoc',
%        'ciaopp_desc',
        'manuals',
        'other_docs',
        'screenshots'
      ]
      % TODO: Temporally removed...
%      phony('core_developers')-[
%        'browse',
%	'compilation_farm'
%     ]
    ].

% No indices
index := _ :- fail. % concept|lib|pred|prop|regtype|decl|author|global.

doc_mainopts := no_patches|no_biblio|no_math|no_versioned_output.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_authors|no_biblio|no_math.

docformat := html. % we are a webpage!

% ===========================================================================

% target filesystem path 
htmldir := '<v>DistHtmlDir</v>'.
% target URL
htmlurl := '<v>DistHtmlURL</v>'.

bibfile := _ :- fail. % not used
docdir := _ :- fail. % not used
infodir := _ :- fail. % not used
mandir := _ :- fail. % not used
lpdoclib := _ :- fail. % use default

% Website source directory
website_root_dir := '<v>WebsiteDir</v>'.

% The website skeleton that is copied to the output directory
% (contains the CSS files, images, etc.)
website_skeleton := ~fsR((~website_root_dir)/'skel').

% TODO: generalize? document
html_layout := 'website_layout'.

%-----------------------------------------------------------------------------

% File permisions, owner, and group
perms := perm(rwX, rX, rX).
owner := '<v>DistOwner</v>'.
group := '<v>DistGroup</v>'.

% ===========================================================================
% Distributed files (pbundles)

% ---------------------------------------------------------------------------
% The source of the pbundles

%:- pred pbundledb_userhostdir/1 # "Specifies the repository with
%	precompiled packages to use.".
% TODO: Duplicated in ciaobot/ 

pbundledb_userhostdir := 'ciaobot@cliptest1.dia.fi.upm.es:/shared/pbundledb'.

% ---------------------------------------------------------------------------
% The local storage of packaged bundles (pbundle)
% (where we keep the copies)

% Packaged files (code)
% TODO: rename 'packages' by something else?
pbundle_localpkgdir := ~atom_concat([~('SETTINGS':htmldir), '/packages']).
pbundle_localpkgurl := 'packages'.

% Docs files (online and PDFs)
pbundle_localdocdir := ~atom_concat([~('SETTINGS':htmldir), '/docs']).
pbundle_localdocurl := 'docs'.

% ---------------------------------------------------------------------------
% The stable version distributed

% Note: Currently, downloadable files for stable versions are managed
%       by hand. That is, they must be deleted when no longer necessary
%       and fetched by hand from ciaobot.
%
% HOW TO FETCH A NEW STABLE VERSION:
%
%   Execute "./lpdist fetch 1.14 13619" from the website
%   directory at 'clip' machine.

stable_revision(rev_in_branch('1.14', '13646')).

% TODO: nicer alternative (not implemented)
%   just take the last one (tags allow naming particular revisions)
%   or obtain it from the repository metainformation (ciaobot_config?)
%   used by ciaobot.
%
% E.g.,
%   stable_revision(latest_in_branch('1.14')).
%   stable_revision(latest_in_branch('1.14.0')).



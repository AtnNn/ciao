:- module(_, _, [assertions, regtypes, fsyntax]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(autoconfig)).
:- reexport(ciaodesrc(makedir('DOCCOMMON'))).

%% Should add extrafiles, etc.

% ----------------------------------------------------------------------------

:- doc(title, "Configuration File for Document Generation").

% TODO: What this documentation says is wrong. It is either an example
%   or the settings file for LPDOC itself, but not both things.
%   (JFMC)

:- doc(module, "This is a sample configuration file for @apl{lpdoc}
   (as well as the documentation on configuration).  The defaults
   listed are typically suggestions and/or the ones used for local
   installation in the CLIP group machines.  These settings should be
   changed to suit your application. The set of variables are grouped
   by functionality.").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "David Trallero Mena").
:- doc(author, "Edison Mera").

% ----------------------------------------------------------------------------

:- doc(filetype, user).

% ---------------------------------------------------------------------------
% Distpkg (distribution package) definitions

distpkg_name := 'lpdoc'.
% TODO: this avoids duplicated code; language support would make it nicer
:- include(library(distutils(distpkg_versions__template))).
:- use_module(library(distutils(distpkg_versions))).

% ----------------------------------------------------------------------------
% DIRECTORIES
% ----------------------------------------------------------------------------

:- pred distpkg_root_dir(PackageRootDir) => atm(PackageRootDir)

# "@var{PackageRootDir} defines the root directory from where the
  package actions (generations of tars, documentation, version ...)
  will take place (do not forget the / at the end of the path).

  Example: distpkg_root_dir := '/home/clip/Systems/CiaoDE/'.".

% REVIEW
distpkg_root_dir := ~atom_concat(~component_src(lpdoc), '/').

% ----------------------------------------------------------------------------
% Here come variables that define how to build the documentation.
% These variables include: paths, document structure, file options,
% doc formats, buildroot, permissions and install paths
% ----------------------------------------------------------------------------

% ----------------------------------------------------------------------------
% PATHS
% ----------------------------------------------------------------------------

:- pred filepath/1 => dirpath

# "Defines the directories where the @tt{.pl} files to be documented
   can be found.  You also need to specify all the paths containing
   files which are used by the files being documented. For example,
   the paths to files included by an @tt{@@include} command or to
   figures.

   Example: filepath := '/home/clip/Systems/ciao/doc' |
                        '/home/clip/Systems/ciao/doc/common'.".
%   @includedef{filepath/1}".

% REVIEW
filepath := ~atom_concat([~component_src(lpdoc), '/src']).
filepath := ~atom_concat([~component_src(lpdoc), '/readmes']).
filepath := ~atom_concat([~component_src(lpdoc), '/examples']).
filepath := ~atom_concat([~component_src(ciao),  '/doc/common']).

:- doc(dirpath/1, "An atom describing a path to a
   directory. Should be a full, explicit path (i.e., not containing
   environment variables).").

:- regtype dirpath(P) # "@var{P} is a full path to a directory.".

dirpath(P) :- atm(P).


:- regtype filename(P) # "@var{P} is a full path to a file.".

filename(P) :- atm(P).


:- pred systempath/1 => dirpath

# "Defines the @bf{system} directories where @tt{.pl} files used are.
   You also need to specify all the paths containing files which are
   used by the files being documented.  For example, library
   subdirectories containing files used in the documentation should
   also be included, as well as figures, @tt{@@include}s, etc.  You
   can put these in @pred{filepath/1} instead. Putting them here only
   means that the corresponding files will be assumed to be
   @em{system} files (and labelled as such) in the documentation.

   Example: @includedef{systempath/1}".

% REVIEW
%% systempath := '/home/clip/Systems/CiaoDE/trunk/ciao/lib/'|
%% 	'/home/clip/Systems/CiaoDE/trunk/ciao/library/'|
%% % For Copyright.Manuals
%% 	'/home/clip/Systems/CiaoDE/trunk/ciao/doc/common/'.

systempath := ~atom_concat([~component_src(ciao), ~ciao_path]).

ciao_path := '/lib'|
	'/lib/assertions'|
	'/lib/metaprops'|
	'/lib/regtypes'|
	'/lib/engine'|
	'/lib/rtchecks'|
	'/lib/unittest'|
	'/library'|
	'/contrib'|
        '/doc/common'.


:- pred pathsfile(File) # "The argument denotes a file containing path
   alias definitions and oher compiler-related info used when
   compiling the files to be documented. This has the same
   functionality as the @tt{-u} option in @apl{ciaoc}. Simply leave
   undefined if you do not use path aliases, etc.

   Example: pathsfile := 'mypaths.pl' ". % @includedef{pathsfile/1}".

% Now that assertion checking is stronger, we need this kludge to
% avoid compilation errors:

pathsfile(_) :- fail. 
%% pathsfile := 'mypaths.pl'.

% ----------------------------------------------------------------------------

:- pred output_name(Base) => sourcename

# "Defines the base file name used to be part of the output name of
  the generated files. By default it is equal to the root file of the
  document structure @pred{doc_structure/1}.

  Example: @includedef{output_name/1}".

output_name := ~atom_concat([~distpkg_name, '-', ~distpkg_version_nice]).

% ----------------------------------------------------------------------------
% The document structure
% ----------------------------------------------------------------------------

doc_structure := 
        'lpdoc'-[
	  'Reference'-[
	    'Generating',
	    'comments',
	    'assertions_doc',
	    'assertions_props',
	    'regtypes_doc',
	    'basic_props',
	    'native_props',
	    'meta_props',
	    'lpdoc_examples',
	    'example_module',
	    'rtchecks_doc',
	    'unittest_doc',
	    'lpdoc_install'
          ],
	  'Internals'-[
	    'autodoc',
	    'autodoc_doctree',
	    'autodoc_structure',
	    'autodoc_settings',
	    % Backends
	    'Backends'-[
	      'autodoc_texinfo',
	      'autodoc_html'-[
	        'autodoc_html_resources',
	        'autodoc_html_template'
              ],
	      'autodoc_man'
            ],
	    % Miscellanea and other support code
	    'autodoc_filesystem',
	    'autodoc_index',
	    'autodoc_refsdb',
	    'autodoc_errors',
	    'autodoc_bibrefs',
	    'autodoc_aux',
	    'autodoc_images'
% TODO: Compute local modules that are not included in the internal documentation? Emit warning?
%	    'distpkg_download'
%	    'fastformat'
          ]
        ].

% ----------------------------------------------------------------------------
% Setting processing options for the different files
% ----------------------------------------------------------------------------

commonopts := '-nobugs'|'-nopatches'.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

% ----------------------------------------------------------------------------
% Default document formats
% ----------------------------------------------------------------------------

% :- pred docformat(Format) => supported_format

% # "Defines the documentation formats to be generated by default when
%    running @apl{lpdoc}, among the following (@em{they should be kept
%    in this order}): @includedef{docformat/1}".

% % docformat := texi.       
% docformat := ps.
% docformat := pdf.
% docformat := manl.
% docformat := info.
% docformat := html.

% :- regtype supported_format/1 
% # "Available formats: @includedef{supported_format/1}".

% supported_format := texi.
% supported_format := dvi.
% supported_format := ps.
% supported_format := pdf.
% supported_format := ascii.
% supported_format := manl.
% supported_format := info.
% supported_format := html.

% ----------------------------------------------------------------------------
% Indices to be generated
% ----------------------------------------------------------------------------

:- pred index(Format) => supported_index

# "Defines the indices to be generated by default when running
   @apl{lpdoc}, among the following: 

   @noindent @tt{concept lib apl pred prop regtype decl op modedef file global}

   Selecting @tt{all} generates all the supported indices However,
   note that this (as well as selecting many indices explicitely)
   exceeds the capacity of most texinfo installations. A good set of
   indices is:

   @includedef{index/1}".

index := concept|lib|pred|prop|regtype|decl|author|global.

:- regtype supported_index/1
# "Supported indexes: @includedef{supported_index/1}".

supported_index :=
	concept|
	lib|
	apl|
	pred|
	prop|
	regtype|
	decl|
	op|
	modedef|
	file|
	global|
	author|
	all.

% ----------------------------------------------------------------------------
% References
% ----------------------------------------------------------------------------

% :- pred bibfile(Format) => filename
% 
% # "If you are using bibliographic references, define in this way the
%    @tt{.bib} files to be used to find them. Defines the indices to be
%    generated by default when running @apl{lpdoc}, among the following:
% 
%    Example: @includedef{bibfile/1}".
% 
%bibfile :=
%	'/home/clip/bibtex/clip/clip'|
%	'/home/clip/bibtex/clip/others'.

% ----------------------------------------------------------------------------
% Other settings
% ----------------------------------------------------------------------------

:- pred startpage(PageNumber) => int

# "Setting this to a different value allows changing the page number of
   the first page of the manual. This can be useful if the manual is to
   be included in a larger document or set of manuals.
   Typically, this should be an odd number.

   Example: @includedef{startpage/1}".

startpage := 1.

:- pred papertype(PageNumber) => supported_papertype

# "Selects the type of paper/format for printed documents.  See also
   the @tt{-onesided} and @tt{-twosided} options for the main file.

   Example: @includedef{papertype/1}".

papertype := afourpaper.

:- regtype supported_papertype/1
# "Possible papertypes: @includedef{supported_papertype/1}".

supported_papertype := letterpaper.
supported_papertype := smallbook.
supported_papertype := afourpaper.
supported_papertype := afourlatex.
supported_papertype := afourwide.
supported_papertype := afourthesis.


:- pred libtexinfo/1 => yesno

# "If set to yes the @file{texinfo.tex} file that comes with the lpdoc
   distribution will be used when generating manuals in formats such
   as @tt{dvi} and @tt{ps}. Otherwise, the texinfo file that comes
   with your @apl{tex} installation will be used. It is highly
   recommended that you leave this set to @tt{'yes'} as below:

   Example: @includedef{libtexinfo/1}".

libtexinfo := 'yes'.

:- regtype yesno/1
# "Enumerated type: @includedef{yesno/1}".

yesno := yes|no.


% ----------------------------------------------------------------------------
% BUILD ROOT
% ----------------------------------------------------------------------------

:- pred doc_build_root(Dir) => atm(Dir)

# "@var{Dir} is the directory where documentation files will be
  placed. @var{Dir}/tmp will be used for temporary/cache files.".

% REVIEW?
doc_build_root := ~atom_concat(~distpkg_root_dir, 'doc/').

% ============================================================================
% ============================================================================
% Installation options
% (You only need to change these if you will be installing the docs somewhere)
% ============================================================================
% ============================================================================


% ----------------------------------------------------------------------------
%
% :- doc(title,"Configuration File for Package Generation").
%
% :- doc(module,"This is a sample configuration file for
%    @apl{lpdoc}.  The defaults listed are typically suggestions and/or
%    the ones used for local installation in the CLIP group machines.
%    These settings should be changed to suit your application.").
%
% :- doc(author,"David Trallero Mena").
%
% ----------------------------------------------------------------------------
%
% :- doc(filetype,user).

%-----------------------------------------------------------------------------

:- regtype group_term(P) # "User and Group names:
                            @includedef{group_term/1}.".

group_term(grp(User, Group)) :-
	atm(User),
	atm(Group).

% ----------------------------------------------------------------------------
% The settings below are important only for generating html and info *indices*
% ----------------------------------------------------------------------------

:- pred infodir_headfile/1 => filename

# "Define this to be file containing the header for the @apl{info}
   '@tt{dir}' index.  '@tt{dir}' entries generated by @apl{lpdoc}
   are placed in a @tt{dir} file that starts with
   @pred{infodir_headfile/1} and finishes with
   @pred{infodir_tailfile/1}.

   Example: @includedef{infodir_headfile/1}".

infodir_headfile := ~atom_concat([~component_src(lpdoc),
		'/lib/Head_clip.info']).

:- pred infodir_tailfile/1 => filename

# "Define this to be file containing tail for the for the @apl{info}
   '@tt{dir}' index.  '@tt{dir}' entries generated by @apl{lpdoc}
   are placed in a @tt{dir} file that starts with
   @pred{infodir_headfile/1} and finishes with
   @pred{infodir_tailfile/1}.

   Example: @includedef{infodir_tailfile/1}".

infodir_tailfile := ~atom_concat([~component_src(lpdoc),
		'/lib/Tail_clip.info']).

% End of SETTINGS

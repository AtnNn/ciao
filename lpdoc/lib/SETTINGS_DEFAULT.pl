:- module(_, _, [assertions, regtypes, fsyntax]).

:- use_module(library(system)).
:- use_module(library(terms),                   [atom_concat/2]).
:- use_module(lpdocsrc(makedir('LPDOCSHARED')), [versionmain/1]).
:- use_module(engine(system_info),              [get_ciao_ext/1]).


%% Should add extrafiles, etc.

% ----------------------------------------------------------------------------

:- doc(title, "Configuration File for Document Generation").

:- doc(module, "This is a sample configuration file for
   @apl{lpdoc} (as well as the documentation on configuration).  The
   defaults listed are typically suggestions and/or the ones used for
   local installation in the CLIP group machines.  These settings
   should be changed to suit your application.").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "David Trallero Mena").

% ----------------------------------------------------------------------------

:- doc(filetype, user).

% ----------------------------------------------------------------------------
% PATHS
% ----------------------------------------------------------------------------

:- pred filepath/1 => dirpath

# "Defines the directories where the @tt{.pl} files to be documented
   can be found.  You also need to specify all the paths containing
   files which are used by the files being documented. For example,
   the paths to files included by an @tt{@@include} command or to
   figures.

   Example:
   @includedef{filepath/1}".

filepath := '/home/clip/Systems/lpdoc/doc'|'/home/clip/Systems/ciao/doc/common'.

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

   Example:
   @includedef{systempath/1}".

systempath := '/home/clip/Systems/ciao/lib'|'/home/clip/Systems/ciao/library'|'/home/clip/Systems/ciao/contrib'.

:- pred pathsfile(File) # "The argument denotes a file containing path
   alias definitions and oher compiler-related info used when
   compiling the files to be documented. This has the same
   functionality as the @tt{-u} option in @apl{ciaoc}. Simply leave
   undefined if you do not use path aliases, etc.

   Example: @includedef{pathsfile/1}".

pathsfile(_) :- fail.  % kludge to avoid compilation error

%% pathsfile := 'mypaths.pl'.

% ----------------------------------------------------------------------------

:- pred output_name(Base) => sourcename

# "Defines the base file name used to be part of the output name of
  the generated files. By default it is equal to the root file of the
  document structure @pred{doc_structure/1}.

  Example: @includedef{output_name/1}".

output_name := 'my-doc-1.0'.

% ----------------------------------------------------------------------------
% The document structure
% ----------------------------------------------------------------------------

:- pred doc_structure(Term)
# "Defines the document structure as a tree. The tree is defined as a
   root node with optional childs. Nodes can be atoms or pairs
   (@tt{N-Cs}), where @tt{Cs} is a list of nodes. The root of the tree
   is the main file of the manual, i.e., the file which determines the
   manual's cover page, and first chapter. The child files are used as
   components, i.e., which will constitute the subsequent chapters of
   the manual.

   Example: @includedef{doc_structure/1}".

doc_structure := '<v>distpkg_name</v>_doc'.
%doc_structure := mod0-['mod1'-['mod11','mod12'], 'mod2'].

% TODO: outdated?
% or @tt{.src} files (manually produced files in @apl{texinfo} format).

% ----------------------------------------------------------------------------
% Setting processing options for the different files
% ----------------------------------------------------------------------------

:- pred doc_mainopts(Option) :: supported_option
# "@var{Option} is a processing option which should be activated when
   processing the main file.

   Example: @includedef{doc_mainopts/1}".

:- pred doc_compopts(Option) :: supported_option
# "@var{Option} is a processing option which should be activated when
   processing the secondary files (all except the main file).

   Example: @includedef{doc_mainopts/1}".

commonopts := '-nobugs'|'-nopatches'.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

:- regtype supported_option/1 #
	"Possible options: @includedef{supported_option/1}".

:- doc(supported_option/1, "In general, selecting none of these
	options generates the most verbose manuals, i.e., each option
	generally supresses the production of a certain kind of output
	(on the other hand, @tt{'-v'} selects verbose output from
	ciaoc when processing the file).").

supported_option := '-v'.
supported_option := '-nobugs'.
supported_option := '-noauthors'.
supported_option := '-noversion'.
supported_option := '-nochangelog'.
supported_option := '-nopatches'.
supported_option := '-modes'.
supported_option := '-headprops'.
supported_option := '-literalprops'.
supported_option := '-nopropnames'.
supported_option := '-noundefined'.
supported_option := '-nopropsepln'.
supported_option := '-nobiblio'.
supported_option := '-nobullet'.
supported_option := '-nosysmods'.
supported_option := '-noengmods'.
supported_option := '-noisoline'.
supported_option := '-propmods'.
supported_option := '-shorttoc'.

% ----------------------------------------------------------------------------
% Default document formats
% ----------------------------------------------------------------------------

:- pred docformat(Format) => supported_format

# "Defines the documentation formats to be generated by default when
   running @apl{lpdoc}, among the following (@em{they should be kept
   in this order}): @includedef{docformat/1}".

docformat := texi.
docformat := ps.
docformat := pdf.
docformat := manl.
docformat := info.
docformat := html.

:- regtype supported_format/1
# "Available formats: @includedef{supported_format/1}".

supported_format := texi.
supported_format := dvi.
supported_format := ps.
supported_format := pdf.
supported_format := ascii.
supported_format := manl.
supported_format := info.
supported_format := html.

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

index := concept.
index := pred.
index := prop.
index := regtype.
index := modedef.
index := global.

:- regtype supported_index/1
# "Supported indexes: @includedef{supported_index/1}".

supported_index := concept.
supported_index := lib.
supported_index := apl.
supported_index := pred.
supported_index := prop.
supported_index := regtype.
supported_index := decl.
supported_index := op.
supported_index := modedef.
supported_index := file.
supported_index := global.
supported_index := all.

% ----------------------------------------------------------------------------
% References
% ----------------------------------------------------------------------------

:- pred bibfile(Format) => filename

# "If you are using bibliographic references, define in this way the
   @tt{.bib} files to be used to find them. Defines the indices to be
   generated by default when running @apl{lpdoc}, among the following:

   Example: @includedef{bibfile/1}".

bibfile := '/home/clip/bibtex/clip/clip'.
bibfile := '/home/clip/bibtex/clip/others'.


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

% TODO: Deprecated, see website_skel, etc.
% :- pred htmlstyle/1 => filename
% 
% # "Define this to point to a style sheet to be used for any html pages
%    generated (a copy of the style sheet will be made). Set it to
%    @tt{no} to avoid using any style sheet. 
% 
%    Example: @includedef{htmlstyle/1}".
% 
% htmlstyle := ~atom_concat([~lpdoclib, '/default.css']).

:- pred libtexinfo/1 => yesno

# "If set to yes the @file{texinfo.tex} file that comes with the
   lpdoc distribution will be used when generating manuals in
   formats such as @tt{dvi} and @tt{ps}. Otherwise, the texinfo file
   that comes with your @apl{tex} installation will be used. It is
   recommended that you leave this set to @tt{'yes'} as below:

   Example: @includedef{libtexinfo/1}".

libtexinfo := 'yes'.

:- regtype yesno/1
# "Enumerated type: @includedef{yesno/1}".

yesno := yes|no.

% ============================================================================
% Installation options
% (You only need to change these if you will be installing the docs somewhere)
% ============================================================================

%-----------------------------------------------------------------------------
% WHERE MANUALS WILL BE INSTALLED
%-----------------------------------------------------------------------------

:- pred htmldir/1 => dirpath # "Directory where the @tt{html} manual
   will be generated.".

htmldir := '/home/clip/public_html/Local/lpdoc_docs'.

:- pred docdir/1 => dirpath

# "Define this to be the dir in which you want the document(s) installed. 

   Example: @includedef{docdir/1}".

docdir := '/home/clip/public_html/Local/lpdoc_docs'.

:- pred infodir/1 => dirpath # "Define this to be the dir in which you
   want the info file installed".

infodir := '/home/clip/public_html/Local/lpdoc_docs'.

:- pred mandir/1 => dirpath # "This is the directory in which the man
   file will be installed.".

mandir := '/home/clip/public_html/Local/lpdoc_docs'.


:- pred compresscommand/1 => filename

# "If you would like files containing the manuals (@tt{.dvi},
   @tt{.ps}, etc.) to be compressed on installation, set this to
   the command to be used for such compression.

   Example: @includedef{compresscommand/1}".

compresscommand := 'gzip -f'.


:- pred compressext/1 => atm

# "If you have defined a compression command, define this to be the
  suffix (extension) given by this command to compressed files.

   Example: @includedef{compressext/1}".

compressext := 'gz'.


:- pred datamode(DataPermissions)
	=> permission_term

# "Define this to be the mode for automatically generated data
   files.

   Example: @includedef{datamode/1}".

datamode(perm(rw, rw, r)).


:- pred execmode(ExecPermissions)
	=> permission_term

# "Define this to be the mode for automatically generated
   directories and executable files.

   Example: @includedef{execmode/1}".

execmode(perm(rwx, rwx, rx)).

:- regtype permission_term/1 # "Permisions: @includedef{permission_term/1}.".

permission_term(perm(User, Group, Others)) :-
	permission(User),
	permission(Group),
	permission(Others).

:- regtype permission(P) # "Possible permisions:
                            @includedef{permission/1}.".

permission := x.
permission := w.
permission := wx.
permission := r.
permission := rx.
permission := rX.
permission := rw.
permission := rwx.
permission := 'X'.
permission := wX.
permission := rwX.

% ----------------------------------------------------------------------------
% The settings below are important only for generating info *indices*
% ----------------------------------------------------------------------------

:- pred infodir_headfile/1 => filename

# "Define this to be file containing the header for the @apl{info}
   '@tt{dir}' index.  '@tt{dir}' entries generated by @apl{lpdoc}
   are placed in a @tt{dir} file that starts with
   @pred{infodir_headfile/1} and finishes with
   @pred{infodir_tailfile/1}.

   Example: @includedef{infodir_headfile/1}".

infodir_headfile := ~atom_concat([~lpdoclib, '/Head_clip.info']).

:- pred infodir_tailfile/1 => filename

# "Define this to be file containing tail for the for the @pl{info}
   '@tt{dir}' index.  '@tt{dir}' entries generated by @apl{lpdoc}
   are placed in a @tt{dir} file that starts with
   @pred{infodir_headfile/1} and finishes with
   @pred{infodir_tailfile/1}.

   Example: @includedef{infodir_tailfile/1}".

infodir_tailfile := ~atom_concat([~lpdoclib, '/Tail_clip.info']).

% ----------------------------------------------------------------------------
% This is set during installation
% ----------------------------------------------------------------------------

:- pred lpdoclib/1 => dirpath

# "This points to the directory where the lpdoc library is
   installed. This library contains a number of style files,
   templates, and applications used by lpdoc (see the definitions of
   other predicates in this file).

   Example: @includedef{lpdoclib/1}".



% NOTE: the definition below is written in a single line because the
% procedure by which it is adjusted when generating per-file
% documentation from Emacs does not work wirh multi-line definitions.

lpdoclib_def := ~atom_concat(['/usr/local/lib/lpdoc/', ~versionmain,
		~get_ciao_ext]).

lpdoclib := ~lpdoclib_def.

% ----------------------------------------------------------------------------
% End of SETTINGS

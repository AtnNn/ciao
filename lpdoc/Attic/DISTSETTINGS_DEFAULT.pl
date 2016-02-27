% ===========================================================================
:- module(_,_,[make, fsyntax, regtypes]).

% ===========================================================================
:- comment(title,    "<v>package_name</v> Distribution SETTINGS").
:- comment(subtitle, "").
:- comment(author,   "David Trallero Mena").
:- comment(author,   "Edison Mera").

:- comment(module,"This is a distribution settings configuration
   sample file for @apl{lpdoc}.  The defaults listed are typically
   suggestions and/or the ones used for local installation in the CLIP
   group machines.  These settings should be changed to suit your
   application.").

:- comment(summary, "These are DISTRIBUTIONS SETTINGS for building the
   distribution.").

% ===========================================================================

:- use_module(library(distutils)).
:- use_module(library(distutils(package_generator))).

:- use_module(library(lists), [append/3]).
:- use_module(library(system), [get_pwnam/1, get_grnam/1]).


% comment this line to generate documentation
:- reexport( '<v>package_root_dir</v>doc/LPSETTINGS' ).


% ----------------------------------------------------------------------------

:- comment(title,"Configuration File for Package Generation").


% ----------------------------------------------------------------------------

:- comment(filetype,user).

% ----------------------------------------------------------------------------
% FILES THAT COMPOSE THE PACKAGE
% ----------------------------------------------------------------------------

% EXCLUDED FILES
% ==============

:- pred exclude_files( ExcFiles ) => list( ExcFiles , atm )

# "@var{ExcFiles} is a list with the filenames that will not be
  included in the generation of tar distribution files

  Example: exclude_files := ['ciao/SETTINGS', 'ciao/SETTINGS_AUTO'].".

% exclude_files := ['ciao/SETTINGS', 'ciao/SETTINGS_AUTO'].


% EXCLUDED DIRECTORIES
% ====================

:- pred exclude_dir( ExcFiles ) => list( ExcFiles , atm )

# "@var{ExcFiles} is a list with the directories that will not be
   included in the generation of tar distribution dir

   Example: exclude_dir := ['ciao/SETTINGS', 'ciao/SETTINGS_AUTO'].".

% exclude_dir := ['ciaopp'].

%-----------------------------------------------------------------------------
% TYPE OF DISTRIBUTION
%-----------------------------------------------------------------------------

:- pred disttype( Type )

# "@var{Type} determines the type of distribution: Alpha,
  Beta... Notice that it has to end up with an slash.

  Example: @includedef{disttype/1}".

% disttype:= 'Beta/'.
% disttype:= 'Alpha/'.
% disttype:= 'Test/'.
% disttype:= 'ASAP/'.
disttype:=''.


%-----------------------------------------------------------------------------
% WHERE THE DISTRIBUTION WILL BE INSTALLED
%-----------------------------------------------------------------------------

:- pred disthtmldir( Dir ) => atm( Dir )

# "@var{Dir} is the directory where the webiste template files will be
  copied.
  
   Example: @includedef{disthtmldir/1}".


disthtmldir := ~atom_concat(['/home/clip/public_html/Software/', ~disttype ]).

%disthtmldir := ~atom_concat(['/home/', ~get_pwnam,
%	                 '/public_html/', ~disttype]).

%-----------------------------------------------------------------------------
% HOW TO ACCESS FROM A BROWSER               
%-----------------------------------------------------------------------------

:- pred disthtmlurl( Url ) => atm( Url )

# "@var{Url} is the url path to access to generated website from a
  browser.
  
   Example: @includedef{disthtmlurl/1}".

% disthtmlurl := ~atom_concat(['/~', ~get_pwnam, '/',
%	                     ~package_name, '/', ~disttype]).

disthtmlurl := ~atom_concat(['/Software/', ~disttype ]).


%-----------------------------------------------------------------------------
% DISTRIBUTION BUILD ROOT DIR
%-----------------------------------------------------------------------------

:- pred distbuild_root( Dir ) => atm( Dir )

# "@var{Dir} is the directory where generated files will take
  place. 

  A subdirectory under @var{Dir} with name @tt{site} will be
  considered as the website template to be installed if no website
  templates are found in @pred{distdir/1}. Under the site directory
  there are the non-generated web files, and the sub-directory
  @tt{tmpl} contains the proper templates themselves. The file
  @tt{site/tmpl/generate.pl} must exists in order to be possible to
  generate the website. The file @tt{site/tmpl/TMPLSETTINGS.pl}
  contains the variable @tt{package_info} which determines with
  information should be generated for that website template.

  Generated package files will be stored under packages subdirectory
  of @pred{distbuild_root/1} directory.

  Basically the directory tree is like:

@begin{verbatim}
  distbuild_root
     +
     |
     |-- site
     |   \\-- tmpl
     |       |-- TMPLSETTINGS.pl
     |       \\-- generate.pl
     \\-- package
@end{verbatim}

  Example: @includedef{distbuild_root/1}
".

distbuild_root := ~atom_concat( ~package_root_dir , 'dist/' ).

%-----------------------------------------------------------------------------
% DOCUMENTATION
%-----------------------------------------------------------------------------

% Default document formats
% ========================

:- pred distdocformat(Format) => supported_format

# "Defines the documentation formats to be generated by default when
   running @apl{lpdoc}. 

   Example: @includedef{distdocformat/1}".

:- regtype supported_format/1 # "Supported doc formats by @apl{lpdoc}:
@includedef{supported_format/1}.".

supported_format := texi.       
supported_format := dvi.
supported_format := ps.
supported_format := pdf.
supported_format := ascii.
supported_format := manl.
supported_format := info.
supported_format := html.


distdocformat := ps|pdf|html.

% PERMISIONS FOR GENERATED FILES
% ==============================

:- pred distperms(DataPermissions) => permission_term

# "Define this to be the mode for automatically generated data
   files.

   Example: @includedef{distperms/1}".


distperms := perm(rwX, rX,  rX).

:- regtype permission_term/1 # "Permisions: @includedef{permission_term/1}.".

permission_term(perm(User,Group,Others)) :-
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
permission := rw.
permission := rwx.
permission := 'X'.
permission := wX.
permission := rwX.


:- pred distowner(ExecPermissions) => atom

# "Define this to be the owner (user) for automatically generated
   directories and files.

   Example: @includedef{distowner/1}".

distowner := ~get_pwnam.


:- pred distgroup(ExecPermissions) => atom

# "Define this to be the group for automatically generated directories
   and files.

   Example: @includedef{distgroup/1}".

distgroup := ~get_grnam.


:- comment( hide, do_target_atm/2      ).
:- comment( hide, do_target_var/2      ).
:- comment( hide, do_target/1          ).
:- comment( hide, target_exists/1      ).
:- comment( hide, target_deps/3        ).
:- comment( hide, target_comment/3     ).
:- comment( hide, do_dependency/3      ).
:- comment( hide, dependency_exists/2  ).
:- comment( hide, dependency_precond/3 ).


% ----------------------------------------------------------------------------
% FILES THAT COMPOSE THE PACKAGE
% ----------------------------------------------------------------------------

% EXCLUDED FILES
% ==============

:- pred exclude_files( ExcFiles ) => list( ExcFiles , atm )

# "@var{ExcFiles} is a list with the filenames that will not be
  included in the generation of tar distribution files

  Example: exclude_files := ['ciao/SETTINGS', 'ciao/SETTINGS_AUTO'].".

% exclude_files := ['ciao/SETTINGS', 'ciao/SETTINGS_AUTO'].


% EXCLUDED DIRECTORIES
% ====================

:- pred exclude_dir( ExcFiles ) => list( ExcFiles , atm )

# "@var{ExcFiles} is a list with the directories that will not be
   included in the generation of tar distribution dir

   Example: exclude_dir := ['ciao/SETTINGS', 'ciao/SETTINGS_AUTO'].".

% exclude_dir := ['ciaopp'].


% ----------------------------------------------------------------------------
% DISTRIBUTION FILES FORMAT
% ----------------------------------------------------------------------------

:- pred package_tar_file( Format ) => supported_tar_format

# "Defines the extensions (without dots) of the distribution tar files
   to be generated by @apl{lpdoc}. The current supported extensions
   are: 

   @includedef{supported_tar_format/1}

   Example: package_tar_file := tgz | tbz.".

:- regtype supported_tar_format/1 # "Posible Extension".

supported_tar_format := zip.
supported_tar_format := gz.
supported_tar_format := bz2.


package_tar_file := tgz | tbz.


% ----------------------------------------------------------------------------
% DOCUMENTATION FILES FORMATS
% ----------------------------------------------------------------------------

:- pred package_doc_file( Format ) => supported_doc_format

# "Defines the extensions (without dots) of the documentation files to
   be generated by @apl{lpdoc}. The current supported extensions are:

   @includedef{supported_doc_format/1}

   Example: @tt{package_doc_file := ps | pdf.}

   Notice that an extension like pdfzip means that the generated pdf
   is compressed using @apl{zip} compressor.

   When describing a composed package (a package that is a set of
   other packages), a term which functor is a known as compressed
   format (tgz, or bz2) with a list of packages as its first argument
   and atom specifying the output file name can be specified.

   Example: 
   package_doc_file := ps | pdf | pstgz( [ciao,lpdoc], 'manuals_' ).

   The example means that current pakage documentation in ps and pdf
   formats have to be generated. The third term, pstgz([ciao,lpdoc],
   'manuals_'), determines a document consisting is several documents
   that comes from (sub-)packages. The first argument is the list of
   (sub-)packages to be used in the compressed file, and the second
   argument is a prefix of the extension, which is determined by the
   functor of the term: pstgz. In this case, a compressed file ending
   in 'manuals_ps.tgz' (ps.tgz comes from the functor pstgz and
   'manuals_' is the second argument) have to be generated containing
   the documentation of the (sub-)packages @tt{ciao} and @tt{lpdoc} in
   ps format. To define the package see @pred{component_options/3}.".

:- regtype supported_doc_format/1  # "Posible Extension".

supported_doc_format := ps.
supported_doc_format := pdf.
supported_doc_format := html.
supported_doc_format := psgz.
supported_doc_format := psbz2.
supported_doc_format := pszip.
supported_doc_format := pdfgz.
supported_doc_format := pdfbz2.
supported_doc_format := pdfzip.
supported_doc_format(docpart(Title, Part, Exts)) :-
	string(Title),
	atm(Part),
	list(Exts, supported_doc_format).

package_doc_file := ps | pdf. % | tgz( [ciao, lpdoc] , ciaode ).


:- pred component_options( Key, SubDir, LPSETTINGS )
	: atm(Key)
        => (atm( Subdir ),atm( LPSETTINGS ))

# "Use this option to set-up the subdirectory (@var{SubDir}) and
  LPSETTINGS.pl (@var{LPSEETINGS}) of a given package name
  @var{Key}.

  Example:
component_options( ciao   , 'ciao/'  , 'ciao/doc/reference/LPSETTINGS' ).
component_options( ciaopp , 'ciaopp/', 'ciao/doc/reference/LPSETTINGS' ).
component_options( lpdoc  , 'lpdoc/' , 'ciao/doc/LPSETTINGS'           ).".

%-----------------------------------------------------------------------------
% FOR DISTRIBUTION
%-----------------------------------------------------------------------------

:- pred gen_xdelta( XDelta ) => yes_or_no( XDelta )

# "@var{XDelta} decides whether generate xdeltas between earlier
  versions.
  
   Example: @includedef{gen_xdelta/1}".

:- regtype yes_or_no/1 # "enumerated type: @includedef{yes_or_no/1}".

yes_or_no( yes ).
yes_or_no( no  ).

gen_xdelta:='no'.

%-----------------------------------------------------------------------------
:- pred gen_diff( XDiffs )  => yes_or_no( XDiffs )

# "@var{XDiffs} decides whether generate diffs between earlier
  versions.
  
   Example: @includedef{gen_diff/1}".

gen_diff:='no'.

%-----------------------------------------------------------------------------
% END

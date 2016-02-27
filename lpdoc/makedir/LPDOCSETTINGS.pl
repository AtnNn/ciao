%% ===========================================================================
%% 
%% Installation configuration settings
%% 
%% Set to appropriate values before installing the system.
%% The defaults listed are suggestions and/or the ones used for local 
%% installation in the CLIP group machines.
%% ===========================================================================
:- module(_, _, [assertions, fsyntax]).
%:- use_module(library(terms)).
% :- use_module(library(distutils)).
% :- use_module(library(persvalue)).

:- use_module(library(terms), [atom_concat/2]).


% :- reexport(ciaodesrc(makedir('ConfigMenu')), [execmode/1, datamode/1, docdir/1,
% 	mandir/1, infodir/1, htmldir/1]).

% :- initialization(ciaoinitvals).


%% ===========================================================================
%% This first part determines where things will be installed
%% ===========================================================================

%% ---------------------------------------------------------------------------
%% Define this to be the directory in which you want the executable installed. 
%% 
%% bindir := '/usr/local/bin'.
%% --- DTM: OBSOLETE??
bindir := ~ciaobinroot.

%% ---------------------------------------------------------------------------
%% Define this to be the directory in which you want libraries to be installed.
%% LIBDIR= /usr/local/lib
%%  
%% ***** Needs to be changed to libdir := '/home/clip/lib'.
%%       (overloading in SETTINGS not working fully yet)
%libdir := '/home/clip/lib'.

:- use_module(library(autoconfig)).

baselibdir := ~ciaolibroot.

%% ===========================================================================
%% This second part determines which auxiliary commands will be used
%% 
%% You may not have to change any of the things below, but you should 
%% check just in case. 
%% ===========================================================================
%% ---------------------------------------------------------------------------
%% Define this to be the command that runs tex in your system
%% 
tex := 'tex'.
%% Alternative (sometimes smarter about number of times it needs to run):
%% tex := 'texi2dvi '.
%% (but insists on checking the links, which is a pain...)
%% ---------------------------------------------------------------------------
%% Define this to be the command that runs texindex in your system
%% 
%% Not needed if texi2dvi is installed
texindex := 'texindex'.
%% ---------------------------------------------------------------------------
%% Define this to be the command that builds .bbl files from .bib bibliography
%% files in your system
%% 
bibtex := 'bibtex'.
%% ---------------------------------------------------------------------------
%% Define this to be the command that converts dvi to postscript in
%% your system. Make sure it generates postscript fonts, not bitmaps 
%% (selecting -Ppdf often does the trick). -z preserves hypertext links.
%% 
dvips := 'dvips -z -Ppdf'.
%% ---------------------------------------------------------------------------
%% Define this to be the command that views dvi files in your system
%% 
xdvi := 'xdvi'.
%% ---------------------------------------------------------------------------
%% Define this to be the default size at which manuals are viewed
%% This is typically an integer (1-10 usually) and unfortunately changes 
%% depending on the version of xdvi used.
%%  
xdvisize := '8'.
%% ---------------------------------------------------------------------------
%% Define this to be the command that views ps files in your system
%% 
%ghostview := 'ghostview'.

%% ---------------------------------------------------------------------------
%% Define this to be the command that views PDF pages in your system
%% 
% TODO: If 'see' is not there, try other ('xdg-open', 'gnome-open', etc.)
% TODO: 'open' is used ad-hoc in the code in DARWIN, configure here?
pdfview := 'see'.

psview := 'see'.

%% ---------------------------------------------------------------------------
%% Define this to be the command that views html pages in your system
% TODO: If 'see' is not there, try other ('xdg-open', 'gnome-open', 'firefox', etc.)
% TODO: 'open' is used ad-hoc in the code in DARWIN, configure here?
htmlview := 'see'.

% htmlview := '`which xdg-open ',
%	'|| which firefox ',
%	'|| which mozilla-firefox ',
%	'|| which mozilla ',
%	'|| which x-www-browser`'.

%% ---------------------------------------------------------------------------
%% Define this to be the command that converts postscript to pdf in
%% your system. Make sure it generates postscript fonts, not bitmaps 
%% (selecting -Ppdf in dvips often does the trick)
%% 
ps2pdf := 'ps2pdf'.
%% ---------------------------------------------------------------------------
%% Define this to be the command that converts tex to pdf in your system
%% 
%% texpdf := 'pdftex'.
%% ---------------------------------------------------------------------------
%% Define this to be the command that converts texinfo files into info
%% files in your system. Set also the appropriate flags.
%% 
makeinfo := 'makeinfo'.
%% ----------------------------------------------------------------------------
%% This is a command that converts graphics files to other formats and is better
%% than pstogif and ppmtojpeg 
convertc := 'convert'.
%% ----------------------------------------------------------------------------
%% This is a pointer to your version of makertf 
%% (converts .texi files into .rtf files) 
% TODO: This may be obsolete, keep anyway
%% 
makertf := 'makertf'.
%% ----------------------------------------------------------------------------
%% This is a pointer to your version of MS hc31 
%% (converts .rtf files into Win32 .HLP files) 
% TODO: This may be obsolete, keep anyway
%% 
rtftohlp := 'hc31'.
%% ----------------------------------------------------------------------------


%% ---------------------------------------------------------------------------
%% Command used for compiling lpdoc
%%  

:- use_module(lpdocsrc(makedir('LPDOCSHARED')), [vers/1, basemain/1]).

:- use_module(ciaodesrc(makedir('ConfigValues')), [srcbindir/1, ciaoc/1,
		ciaobinroot/1, ciaolibroot/1, setlocalciao/1, 
		runtime_checks/1]).

:- reexport(ciaodesrc(makedir('ConfigValues')), [emacs_for_ciao/1, xemacs_for_ciao/1]).

srcbinlpdoc := ~atom_concat([~srcbindir, '/', ~basemain, '-', ~vers,
		~get_ciao_ext]).

rtcheck_opt(yes, '-rc').
rtcheck_opt(no,  '').

% TODO: factorize
pl2staexe := ~atom_concat([~setlocalciao, ' ', ~ciaoc,
		' -u ', ~component_src(ciao),
		'/lib/autoconfig/autoconfig.pl -s -x ',
		~rtcheck_opt(~runtime_checks), ' -o ',
		~srcbinlpdoc, ' src/', ~basemain]).

pl2dynexe := ~atom_concat([~setlocalciao, ' ', ~ciaoc,
		' -u ', ~component_src(ciao),
		'/lib/autoconfig/autoconfig.pl    -x ',
		~rtcheck_opt(~runtime_checks), ' -o ',
		~srcbinlpdoc, ' src/', ~basemain]).

%% ---------------------------------------------------------------------------
% Define this to be the permissions for installed execs/dirs and data files:
% execmode(perm(rwx, rwx, rx)).
% datamode(perm(rw, r, r)).
%% ---------------------------------------------------------------------------
%% ===========================================================================
% Command used to compress files by default
% defaultcompresscommand := 'gzip -f'.
% defaultcompressext     := 'gz'.

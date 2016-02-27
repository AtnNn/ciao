:- module(skip_settings, _, [fsyntax]).

:- use_module(library(terms), [atom_concat/2]).

% ----------------------------------------------------------------------------
% Auto generated temporary and process files
skip_clean_only := '*.out|*.aux|*.log|*.tmp|tmpciao*'.
skip_clean_files := ~skip_clean_only.

% Files that must not be distributed in a raw/binary distribution
skip_bak_only := '*.bak|*.Bak|*.BAK|*.old|*.Old|*.OLD|*.gz|*.bz2|*.tgz|\
*.tbz|*.zip|*~|#*'.

%Here we don't skip iss to facilitate debugging of Windows installer:
skip_raw_only := '*_co.pl|*_co.java|*.class|*.o|*_glue.c|*_inline.c'.

skip_pure_only := '*.texic|*.refs|auto*.eps|auto*.txt|auto*.ppm|*.src|\
*_blue.*|*refs.el|*refs.aux|*refs.blg'.


% Files that can be auto generated, and must not be distributed in a
% source distribution
skip_dist_only :=
	'*.po|*.itf|*.wam|*.dep|*.asr|*.ast|*.ass|*_auto.pl|icon_address.pl'.

skip_dist_files(src) := ~atom_concat([~skip_raw_files, '|', ~skip_platdep_files,
		'|', ~skip_dist_only]).
skip_dist_files(noa) :=
	~atom_concat([~skip_raw_files, '|', ~skip_platdep_files]).
skip_dist_files(bin) := ~skip_raw_files.
% *.elc files should not be included due to they are emacs version dependent
skip_dist_files(raw) := ~skip_raw_files.

skip_dist_dirs(src) := 'build/include|build/objs|build/bin|build/doc'.
skip_dist_dirs(noa) := 'build/include|build/objs|build/bin'.
skip_dist_dirs(bin) := 'build/include|build/objs|build/bin'.
skip_dist_dirs(raw) := 'build/objs/*-install'.

skip_raw_files := ~atom_concat([~skip_clean_only, '|', ~skip_raw_only,
		'|', ~skip_pure_only,
'|*_src_auto.*|*_ins_auto.*|*_wrapper_auto.*\
|ciao-mode-init.el|ciao.el|*.elc']).

skip_platdep_files := '*.so|*.dll|*.dylib'.

skip_dist_files_bak(DistType) := ~atom_concat([~skip_bak_only, '|',
		~skip_dist_files(DistType)]).

% Files that do not needed to be cleaned, and will not be distributed
skip_dirs := 'bak|*.bak|*.Bak|*.BAK|CVS|.svn|tmp'.

% File name used as flags to indicate if a directory will not be distributed
nodist_dirs := ['NODISTRIBUTE', 'nodistribute', '.NODISTRIBUTE',
	    '.nodistribute'].

% Directories containing the next files will not be automatically compiled:
nocompile_dirs := ['Makefile.pl', 'SETTINGS.pl', '.NOCOMPILE',
	    'NOCOMPILE'].

nocompile_files := ['NOCOMPILEFILES', '.NOCOMPILEFILES'].

% File name used as flags to indicate that a directory should not be installed
noinstall_dirs := ['NOINSTALL', 'noinstall', '.NOINSTALL', '.noinstall'].
% ----------------------------------------------------------------------------

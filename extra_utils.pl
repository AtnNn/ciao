:- module(_,[
	copy_distribution/6,
	copy_src_distribution/2
	    ], [functions]).

:- use_module(library('make/system_extra')).
:- use_module(library(lists),[list_concat/2]).
:- use_module(library(terms),[atom_concat/2]).
%:- use_module(library(format), [format/2, format/3]).
%:- use_module(settings).
%:- use_module(shared).
%:- include(common_make).

copy_distribution(BaseDir, DestDir, Pattern, PatternExclude, ExcludeDirs, ExcludeDirFiles) :-
	lss(BaseDir, Pattern, PatternExclude, ExcludeDirs, ExcludeDirFiles, FileLists),
	copy_files_rec(BaseDir, DestDir, FileLists).

copy_src_distribution(BaseDir, DestDir) :-
	copy_distribution(BaseDir, DestDir, '*',
	'*.bak|*.Bak|*.BAK|*.old|*.Old|*.OLD|*.gz|*.tar|*.aux|*.log|.*|*.po|*.itf|*.dep|*.asr|*~',
	['CVS'], ['NODISTRIBUTE', '.nodistribute', '.NODISTRIBUTE']).

:- module(engine_path, [get_engine/4], [assertions]).

:- use_module(library(system)).

% Which engine name can be applied to each architecture? Windows
% executables are always named ciaoengine.exe and is only an small loader,
% but the engine is located in libciao.dll
% **x static executables are named *.sta, and can have the
% OS/Arch combination in the name.  It is probably not wise to look
% for a generic ciaoengine.sta executable in the shared, general
% library directory; hence the direct/generic atom in the third
% argument. 

% Windows engines always same name (at least for now)
determine_engine_name('Win32i86', 'ciaoengine.exe', direct).
% Other engines have different names according to placement!
determine_engine_name(TargetEng, Engine, direct) :- % If in installation
	TargetEng \== 'Win32i86',
	atom_concat('ciaoengine.', TargetEng, Eng1),
	stat_extension(Sta),
	atom_concat(Eng1, Sta, Engine).
determine_engine_name(TargetEng, Engine, generic) :- % For sources
	stat_extension(Sta),
	atom_concat('ciaoengine', Sta, Engine),
	TargetEng \== 'Win32i86'.

:- doc(bug, "Generation of a static engine requires that also the
	foreing modules be compiled together the engine.  Currently
	the engine is always dynamic -- EMM.").

stat_extension('.sta').
stat_extension('').

% What directory this engine can be in?
determine_engine_dir(TargetEng, Where, LibDir, EngDir) :-
	intermediate_dir(TargetEng, Where, IntermediateDir),
	atom_concat(LibDir, IntermediateDir, EngDir),
	file_exists(EngDir).

% Windows engines can be placed differently from other engines
%intermediate_dir('Win32i86', _, '/bin/Win32i86/').
%intermediate_dir('Win32alpha', _, '/bin/Win32alpha/').
intermediate_dir(_Target, direct, '/engine/'). % For unix --- Windows also, later?
intermediate_dir(Target,  _,      Dir       ) :-
	atom_concat('/../build/objs/', Target, Dir1),
	(Ciaodebug = '' ; get_debug(Ciaodebug)),
	atom_concat(Dir1, Ciaodebug, Dir2),
	atom_concat(Dir2, '/',       Dir ).

get_engine(TargetEng, EngDir, EngName, Engine) :-
	ciao_lib_dir(LibDir),
	determine_engine_name(TargetEng, EngName, Where),
	determine_engine_dir(TargetEng, Where, LibDir, EngDir),
	atom_concat(EngDir, EngName, Engine),
	file_exists(Engine).

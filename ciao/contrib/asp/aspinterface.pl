:- module(aspinterface, [buildAspInterface/0]).

%:- use_module(library(term_to_string)).

:- use_module(library(asp(misc))).

:- use_module(library(asp(h3))).
:- use_module(library(asp(header))).
:- use_module(library(asp(intrunsolver))).
:- use_module(library(asp(intassert))).
:- use_module(library(asp(intretract))).
:- use_module(library(asp(intcompute))).
:- use_module(library(asp(intmisc))).
:- use_module(library(asp(intatom))).
:- use_module(library(asp(intclasses))).
:- use_module(library(asp(intclean))).
:- use_module(library(asp(intdefine))).
:- use_module(library(asp(intfhandle))).
:- use_module(library(asp(intground))).
:- use_module(library(asp(intmodel))).
:- use_module(library(asp(intparm))).
:- use_module(library(asp(intreadasp))).
:- use_module(library(asp(intrelease))).
:- use_module(library(asp(intstate))).
:- use_module(library(asp(modulestruct))).
:- use_module(library(asp(inttmpvar))).
:- use_module(library(asp(intdbg))).
:- use_module(library(asp(intskep))).
:- use_module(library(asp(interror))).

buildAspInterface :-
%	dmsg(1,['build ASP Interface Strart ---------------------------',nl]),
	findall(Module1,moduleTree(_,Module1), ModuleList1),
%	dmsg(1,['ModuleList1=',ModuleList1, nl]),
	aspNames(ModuleList1, ModuleList1a),
%	dmsg(1,['ModuleList1a=',ModuleList1a, nl]),
	aspList2Set(ModuleList1a, Module1a),
%	dmsg(1,['Module1a=',Module1a, nl]),
	asp2ListAspInterface(Module1a),!.

asp2ListAspInterface([]).
asp2ListAspInterface([asp(AbsH,Parm)|T]) :-
%	dmsg(1,['AbsH=',AbsH, nl]),
	file2Path(AbsH, AspFileName, AbsPath),

	aspName2IntName(AspFileName, IntFileNameExt),
	file2Module(IntFileNameExt, IntModuleName),
	atom_concat(AbsPath, IntFileNameExt, AbsPathIntFileName),
	splitFileExt(IntFileNameExt, IntFileName, _),
	
%	aspName2IntName(H,IFileName),
%	combineFileNameExt(H,PathIntName),
%	atom_concat(IntName, '.asp', IntFileNameExt),
%	file2Path(IntName,FileName,_),
%	dmsg(1,['IntFileNameExt=',IntFileNameExt, ' IntModuleName=', IntModuleName,' AbsPathIntFileName=',AbsPathIntFileName, ' IntFileName=',IntFileName, nl]),
%	no_path_file_name(IntName, IntFileName),
	open(AbsPathIntFileName, write, S),!,
%	filterModule(IntFileName, IntModuleName),

%	display('buildHeader\n'),
	buildHeader(S, IntModuleName, IntFileName, AbsH),
	buildModuleStruct(S, AbsH),
%	display('aaaaaaaaaaaaaaaaaaaaaaa\n'),
	buildDefine(S),
	buildRunSolver(S),
	buildReadAsp(S),
	buildAssert(S),
	buildRetract(S),
	buildCompute(S),
%	display('aaaaaaaaaaaaaaaaaaaaaaa\n'),

	buildParm(S,Parm),
	buildModel(S),
	buildGroundASP(S),
%	buildJust(S),
	buildAtomRule(S),
	buildSkep(S),
	buildAspState(S),
	buildMisc(S),
	buildCleanup(S),
	buildRelease(S),
	buildFormatHandle(S),
%	display('bbbbbbbbbbbbbbbbbbbbbbbbbbb\b'),
	buildClasses(AbsPath),
%	display('cccccccccccccccccccc\n'),
	buildDbg(S),
%	display('dddddddddddddddddd\n'),
	buildError(S),
	buildTmpVar(S),
	close(S),
%	display('xxxxxxxxxxxxxxxxxxxxx T='), display(T), nl,!,
	asp2ListAspInterface(T).	

aspNames([], []).
aspNames([asp(_,AspModule)|MTail], [asp(AspModule,[])|T]) :- !,
	aspNames(MTail, T).
aspNames([asp(_,AspModule,Parm)|MTail], [asp(AspModule,Parm)|T]) :- !,
	aspNames(MTail, T).
aspNames([prolog(_,_)|MT], T) :- !,
	aspNames(MT,T).
aspNames([prolog(_)|MT], T) :- !,
	aspNames(MT,T).

/*
moduleNames([],[]).
moduleNames([asp(_,AspModule)|MTail], [asp(AspModule)|T]) :- !,
	moduleNames(MTail, T).
moduleNames([prolog(_,PrologModule)|MT], [prolog(PrologModule)|T]) :- !,
	moduleNames(MT,T).
moduleNames([prolog(PrologModule)|MT], [prolog(PrologModule)|T]) :- !,
	moduleNames(MT,T).
*/

aspList2Set([], []).
aspList2Set([asp(AspModule,_)|AT], T) :-
	aspMember(asp(AspModule),AT), !,
	aspList2Set(AT,T).
%aspList2Set([asp(AspModule)|AT], [asp(AspModule)|T]) :- !,
%	aspList2Set(AT,T).
aspList2Set([asp(AspModule,Parm)|AT], [asp(AspModule,Parm)|T]) :-
	aspList2Set(AT,T).
aspList2Set([prolog(_)|AT], T) :- !,
	aspList2Set(AT, T).

aspMember(_, []) :- fail.
aspMember(asp(AspModule), [asp(AspModule,_)|_]) :- !.
%aspMember(asp(AspModule), [asp(AspModule)|_]) :- !.
aspMember(E, [_|T]) :-
%	dmsg(1,[E,' not member', nl]),
	aspMember(E,T).


%list2set([], []).
%list2set([H1|T1], T2) :-
%	member(H1,T1),!,
%	list2set(T1,T2).
%list2set([H1|T1],[H1|T2]) :-
%	list2set(T1,T2).

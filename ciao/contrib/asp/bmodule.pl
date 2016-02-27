:- module(bmodule, [openRead/1]).

%:- use_module(library(term_to_string)).

:- use_module(library(asp(h3))).
:- use_module(library(asp(readrules))).
:- use_module(library(asp(dbg))).
:- use_module(library(asp(misc))).
:- include('define.pl').

%:- include('misc.pl').
%:- include('dbg.pl').
%:- include('readrules.pl').
%:- include('bmodule.pl').
%:- include('write2file.pl').
%:- include('aspInterface.pl').

%openRead(Type,WinFN) :-
openRead(H) :-
	((H=prolog(AbsFN),Type=prolog);(H=asp(AbsFN),Type=asp)),
%	dmsg(1,'Start Open Read -------------------------------------'),
%	dmsg(1,[AbsFN,nl]),
%	getType(H,Type),
	open(AbsFN, read, S1),
%	dmsg(1,[WinFN,nl]),
%	open(WinFN, read, S1),
	read_file(Type,S1,LineTerm),
%	dmsg(1,['Open Read: LineTerm=',LineTerm, ' Type=', Type,nl]),
%	lookupTerm(Type,WinFN,LineTerm),
	lookupTerm(Type,AbsFN,LineTerm),
	close(S1),!.
%	dcall(5,prtModuleTree).
%	dmsg(1,['end OpenRead', nl,nl]).

% getType(prolog(_),prolog).
% getType(asp(_),asp).

lookupTerm(_,_,end_of_file) :- !.
lookupTerm(prolog,AbsFN,:-(use_package([asp]))) :-
	absFile2Module(AbsFN,A),
	(prologFiles(A) -> true;assertz_fact(prologFiles(A))), 
	!,fail.
%lookupTerm(prolog,_,:-(use_module(library(PrologModule)))) :-
%	dmsg(5,[PrologModule,' ', nl]), 
%	!,fail.
%lookupTerm(prolog,WinFN,:-(use_module(PrologModule))) :-
lookupTerm(prolog,AbsFN,:-(use_module(PrologModule))) :-
	absFileName(PrologModule, AbsPrologFileName),
	absFile2Module(AbsPrologFileName, M),
	assertz_fact(moduleTree(prolog(AbsFN),prolog(M,AbsPrologFileName))),
	assertz_fact(searchModule(prolog(AbsPrologFileName))),
%	winFileName(PrologModule, WinPrologFileName),
%      addPrologExtNecessary(PrologModule, PrologModuleExt),
%      getFileName(PrologModuleExt,PrologModule1),
%	dmsg(1,['WinPrologFileName=',WinPrologFileName, nl]),
%	assertz_fact(moduleTree(prolog(WinFN),prolog(WinPrologFileName))),
%	assertz_fact(searchModule(prolog(WinPrologFileName))),
%	dmsg(5,['reading prolog:', PrologModule1,nl]),
%	openRead(prolog,PrologModule1),
	!,fail.
%lookupTerm(prolog,WinFN,:-(use_module(PrologModule,_))) :-
lookupTerm(prolog,AbsFN,:-(use_module(PrologModule,_))) :-
	absFileName(PrologModule, AbsPrologFileName),
	absFile2Module(AbsPrologFileName, M),
	assertz_fact(moduleTree(prolog(AbsFN),prolog(M,AbsPrologFileName))),
	assertz_fact(searchModule(prolog(AbsPrologFileName))),
%      addPrologExtNecessary(PrologModule, PrologModuleExt),
%      getFileName(PrologModuleExt,PrologModule1),
%	winFileName(PrologModule, WinPrologFileName),
%	dmsg(1,WinPrologFileName),
%	fileName2ModuleName(WinPrologFileName, M),
%	assertz_fact(moduleTree(prolog(WinFN),prolog(M,WinPrologFileName))),
%	assertz_fact(searchModule(prolog(WinPrologFileName))),
%	dmsg(5,['reading prolog:', WinPrologFileName,' ', PredList, nl]), 
%	openRead(prolog,WinPrologFileName),
	!,fail.
lookupTerm(prolog,AbsFN,:-(use_module(PrologModule,_,_))) :-
	absFileName(PrologModule, AbsPrologFileName),
	absFile2Module(AbsPrologFileName, M),
	assertz_fact(moduleTree(prolog(AbsFN),prolog(M,AbsPrologFileName))),
	assertz_fact(searchModule(prolog(AbsPrologFileName))),
	!,fail.

%lookupTerm(prolog,WinFN,:-(use_asp(ASPInterface,ASPModule))) :-
lookupTerm(prolog,AbsFN,:-(use_asp(ASPInterface,ASPModule))) :-
	absFileName(ASPModule, AbsASPFileName),
%	format("AbsASPFileName=~k",AbsASPFileName),nl,
	assertz_fact(moduleTree(prolog(AbsFN),asp(ASPInterface,AbsASPFileName,[]))),
	assertz_fact(searchModule(asp(AbsASPFileName))),
%	dmsg(5,'#use_asp found\n'),
%      addASPExtNecessary(ASPModule, ASPModuleExt),
%      getFileName(ASPModuleExt,ASPModule1),
%	winFileName(ASPModule, WinASPFileName),
%	dmsg(1,['WinASPFileName=',WinASPFileName, nl]),
%	format("WinASPFileName=~k",WinASPFileName),nl,
%	assertz_fact(moduleTree(prolog(WinFN),asp(ASPInterface,WinASPFileName,[]))),
%	assertz_fact(searchModule(asp(WinASPFileName))),
%	dmsg(5,['reading asp:',ASPInterface,' ', WinASPFileName, nl]), 
%	openRead(asp,WinASPFileName),
	!, fail.

%lookupTerm(prolog,WinFN,:-(use_asp(ASPInterface,ASPModule,Parm))) :-
lookupTerm(prolog,AbsFN,:-(use_asp(ASPInterface,ASPModule,Parm))) :-
	absFileName(ASPModule, AbsASPFileName),
	assertz_fact(moduleTree(prolog(AbsFN),asp(ASPInterface,AbsASPFileName,Parm))),
	assertz_fact(searchModule(asp(AbsASPFileName))),

%	dmsg(5,'#use_asp found\n'),
%      addASPExtNecessary(ASPModule, ASPModuleExt),
%      getFileName(ASPModuleExt,ASPModule1),
%	winFileName(ASPModule, WinASPFileName),
%	dmsg(1,['WinASPFileName=',WinASPFileName, nl]),
%	assertz_fact(moduleTree(prolog(WinFN),asp(ASPInterface,WinASPFileName,Parm))),
%	assertz_fact(searchModule(asp(WinASPFileName))),
%	dmsg(5,['reading asp:',ASPInterface,' ', WinASPFileName, nl]), 
%	openRead(asp,WinASPFileName),
	!, fail.


%lookupTerm(asp,WinFN,:-('#use_asp'(ASPInterface,ASPModule))) :-
lookupTerm(asp,AbsFN,'#use_asp'(ASPInterface,ASPModule)) :-
	absFileName(ASPModule, AbsASPFileName),
	assertz_fact(moduleTree(asp(AbsFN),asp(ASPInterface,AbsASPFileName,[]))),
	assertz_fact(searchModule(asp(AbsASPFileName))),
%	addASPExtNecessary(ASPModule, ASPModuleExt),
%	getFileName(ASPModuleExt,ASPModule1),
%	winFileName(ASPModule, WinASPFileName),
%	dmsg(1,WinASPFileName),
%	assertz_fact(moduleTree(asp(WinFN),asp(ASPInterface,WinASPFileName,[]))),
%	assertz_fact(searchModule(asp(WinASPFileName))),
%	dmsg(5,['reading asp:', ASPInterface,' ', AbsASPFileName, nl]), 
%	openRead(asp,WinASPFileName),
	!, fail.
%lookupTerm(asp,WinFN,'#use_asp'(ASPInterface,ASPModule,Parm)) :-
lookupTerm(asp,AbsFN,'#use_asp'(ASPInterface,ASPModule,Parm)) :-
	absFileName(ASPModule, AbsASPFileName),
	assertz_fact(moduleTree(asp(AbsFN),asp(ASPInterface,AbsASPFileName,Parm))),
	assertz_fact(searchModule(asp(AbsASPFileName))),
%	addASPExtNecessary(ASPModule, ASPModuleExt),
%	getFileName(ASPModuleExt,ASPModule1),
%	winFileName(ASPModule, WinASPFileName),
%	dmsg(1,WinASPFileName),
%	assertz_fact(moduleTree(asp(WinFN),asp(ASPInterface,WinASPFileName,Parm))),
%	assertz_fact(searchModule(asp(WinASPFileName))),
%	dmsg(5,['reading asp:', ASPInterface,' ', WinASPFileName, nl]), 
%	openRead(asp,WinASPFileName),
	!, fail.
%lookupTerm(asp,WinFN,:-('#import'(PrologInterface,PrologModule))) :-
lookupTerm(asp,AbsFN,'#import'(PrologInterface,PrologModule)) :-
	absFileName(PrologModule, AbsPrologFileName),
	assertz_fact(moduleTree(asp(AbsFN),prolog(PrologInterface,AbsPrologFileName))),
	assertz_fact(searchModule(prolog(AbsPrologFileName))),
%	dmsg(5,['reading prolog:',PrologInterface,' ', AbsPrologFileName, nl]), 
%	addASPExtNecessary(PrologModule, PrologModuleExt),
%	getFileName(PrologModuleExt,PrologModule1),
%	winFileName(PrologModule, WinPrologFileName),
%	dmsg(1,WinASPFileName),
%	getFileName(PrologModuleExt,WinPrologFileName),
%	dmsg(1,WinPrologFileName),
%	assertz_fact(moduleTree(asp(WinFN),prolog(PrologInterface,WinPrologFileName))),
%	assertz_fact(searchModule(prolog(WinPrologFileName))),
%	dmsg(5,['reading prolog:',PrologInterface,' ', WinPrologFileName, nl]), 
%	openRead(prolog,WinPrologFileName),
	!,fail.
lookupTerm(_,_,_) :-
%      dmsg(5,T), nl, 
      !, fail.

%addPrologExtNecessary(PM, PM) :-
%      name(PM,Str),
%      pos(".",Str, _,_),!.
%addPrologExtNecessary(PM,PM1) :-
%      atom_concat(PM,'.pl',PM1).
      
%addASPExtNecessary(PM, PM) :-
%      name(PM,Str),
%      pos(".",Str, _,_),!.
%addASPExtNecessary(PM,PM1) :-
%      atom_concat(PM,'.lp',PM1).


:- export(prtModuleTree/0).

prtModuleTree :- moduleTree(A,B),
      dmsg(1,[A,' ', B, nl]),
      fail.
prtModuleTree.


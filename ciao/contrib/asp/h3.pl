:- module(h3, [sn/3, tm/3]).
:- data sm/5.

:- use_module(library(system)).
:- use_module(library(asp(bmodule))).
:- use_module(library(asp(aspinterface))).
:- use_module(library(asp(misc))).
%:- use_module(library(asp(define))).
%:- use_module(library()).
%:- use_module(library()).
%:- use_module(library()).
%:- use_module(library()).

%:- export(children/1).
:- data moduleTree/2, searchModule/1, children/1.
:- data prologFiles/1, chkTerm/0.
chkTerm.

:- export(moduleTree/2).
:- export(searchModule/1).
:- export(prologFiles/1).

%sn(0,0,M) :-
%	display(M), nl, read(_),
%	retractall(sm(_,_,_,_,_)),
%	root(_),!, dmsg(1,['-------------------------root exist', nl]),
%	absFileName(M,MM),
%	saveChildren(MM).
sn(0,0,M) :-
	setenvstr('MALLOC_CHECK_',[48]),
	\+moduleTree(_,_), !,
%	dmsg(1,['M=',M, nl]),
	buildMainModule(M).

%	display('-------------------> '), nl,
%	absFileName(M,MM),
%	display('-------------------> '), display(MM), nl,
%	saveChildren(MM),
%	children(L),
%	display(' ------------------> '), display(L), nl,
%	Statments = [0,
%			 (:- initialization('$static_instance_creation$')),
%			  (:- use_module(library(system))),
%			   (:- use_module(library(filenames))),
%		     (:- new_declaration(ccc/2,on)),
%		     ('$static_instance_creation$' :- atom_concat(M,'.itf',M1),display('in initialize\n'),display(M),display(' '), display(M1), nl,delete_file(M1)), 
%(children(L)),
%(mtype(Q,0) :- var(Q),!),
%(mtype(Q,1) :- functor(Q,justclass,1),!),
%(mtype(Q,2) :- children(L),member(Q,L),!),
%(mtype(Q,3)),
%(justTerm(getTrueAtoms(_),0) :- !),
%(justTerm(getFalseAtoms(_),0) :- !),
%(justTerm(unknown(_),0) :- !),
%(justTerm(draw_all_just,0) :- !),
%(justTerm(draw_atom_just(_),0) :- !),
%(justTerm(graph_just_all(_),0) :- !),
%(justTerm(graph_just_atom(_,_),0) :- !),
%(justTerm(_,1)),
%(doQualify(0,C1,C2) :- C1:C2),
%(doQualify(1,C1,C2) :- justTerm(C2,J), (J=0->C1:C2;C1:atom_val(C2))),
%(doQualify(2,C1,C2) :- C1:getSkep(C2,0)),
%(doQualify(3,C1,C2) :- C1:C2),
%(ccc(C1,C2) :- display('mtype:'),display(C1),display(':'),display(C2),mtype(C1,T),display(' T='),display(T),nl,doQualify(T,C1,C2))].


%	dmsg(1,['calling prtModuleTree\n']),
%	prtModuleTree.
%	display('calling root\n\n'),
%	root(MM), 
%	retractall_fact(root(_)),
%	dmsg(1,['saveChildren ', MM, nl]),
%	saveChildren(MM).
%	display('==============================\n').

sn((:- use_asp(AliasName, AspName, Parm)), [(:- use_module(AbsInt))],_) :-
	aspName2AbsIntName(AspName, AbsInt),
	absFileName(AspName, AbsAspName),
	fileName2ModuleName(AbsInt, IntModule),
%    chk_file_exist(FullName),
%    fix_full_name(ASPName, FullASPName, _,'.pl'),
%    must_exist(FullASPName),
%    display(FullASPName), nl,

	assertz_fact(sm(AliasName,IntModule,AbsInt,AbsAspName,Parm)).
%	display('create_all_files\n'),
%    create_all_files,
%    message([end]).
sn((:- use_asp(AliasName, AspName)), [(:- use_module(AbsInt))],_) :-
%sn((:- use_asp(AliasName, AspName)), Statments,M) :-
	aspName2AbsIntName(AspName, AbsInt),
	absFileName(AspName, AbsAspName),
	fileName2ModuleName(AbsInt, IntModule),

%    fix_full_name(AliasName, FullName, Name_no_ext,'.asp'),
%    chk_file_exist(FullName),
%    fix_full_name(ASPName, FullASPName, _,'.pl'),
%    must_exist(FullASPName),
%	display('assert: sm'),
%	display(AbsInt), nl,
%	display(IntModule), nl,
	assertz_fact(sm(AliasName, IntModule, AbsInt, AbsAspName, [])).

%    create_all_files.

sn(end_of_file, Statments, M) :-
%	display('end_of_file reached M='),display(M), nl,
	absFileName(M,MM),
%	display('MM='), display(MM), display(' '),
	saveChildren(MM),
%	children(L),
%	prtPrologFiles,
%	display(M), nl, display(L), nl, read(_),
	retract_fact(prologFiles(M)),
%	prtPrologFiles, read(_),
	emptyPrologFiles,

	Statments = [
			 (:- initialization('$static_instance_creation$')),
			  (:- use_module(library(system))),
			   (:- use_module(library(filenames))),
			    (:- use_module(aspQualify)),
%		     (:- new_declaration(ccc/2,on)),
		     ('$static_instance_creation$' :- atom_concat(M,'.itf',M1),display('in initialize\n'),display(M),display(' '), display(M1), nl,delete_file(M1)),
%(children(L)),
(mtype(Q,0) :- var(Q),!),
(mtype(Q,1) :- functor(Q,justclass,1),!),
%(mtype(Q,2) :- children(L),member(Q,L),!),
%(mtype(_,3)),
(justTerm(getTrueAtoms(_),0) :- !),
(justTerm(getFalseAtoms(_),0) :- !),
(justTerm(unknown(_),0) :- !),
(justTerm(draw_all_just,0) :- !),
(justTerm(draw_atom_just(_),0) :- !),
(justTerm(graph_just_all(_),0) :- !),
(justTerm(graph_just_atom(_,_),0) :- !),
(justTerm(_,1)),
%(doQualify(0,C1,C2) :- !,C1:C2),
%(doQualify(1,C1,C2) :- !,justTerm(C2,J), display('J='), display(J),nl,(J=0->C1:C2;(display('call atom_val'),nl,C1:atom_val(C2), display('return from atom_val'),nl))),
%(doQualify(2,C1,C2) :- !,C1:getSkep(C2,0)),
%(doQualify(3,C1,C2) :- !,C1:C2),
%(ccc(M,C1,C2) :- !,display(M),display(':mtype:'),display(C1),display(':'),display(C2),mtype(C1,T),display(' T='),display(T),nl,doQualify(T,C1,C2)),
end_of_file], display('end of initilization code\n').

sn(_,_,_) :- !,fail.

tm(use_asp(AliasName,AspName,Parm), [(use_module(AbsInt))],_) :-
	!,
%	display('tm1: use_asp\n'),
	aspName2AbsIntName(AspName, AbsInt),
	absFileName(AspName, AbsAspName),
	fileName2ModuleName(AbsInt, IntModule),

%    fix_full_name(AliasName, FullName, Name_no_ext,'.asp'),
%    chk_file_exist(FullName),
%    fix_full_name(ASPName, FullASPName, _,'.pl'),
%    must_exist(FullASPName),
    asserta(sm(AliasName,IntModule,AbsInt,AbsAspName,Parm)),
%    display('create_all_files\n'),
%    create_all_files,
    message([end]).
tm(use_asp(AliasName, AspName), [(use_module(AbsInt))],_) :- !,
%	display('tm2: use_asp\n'),
	aspName2AbsIntName(AspName, AbsInt),
	absFileName(AspName, AbsAspName),
	fileName2ModuleName(AbsInt, IntModule),

%    fix_full_name(AliasName, FullName, Name_no_ext,'.asp'),
%    chk_file_exist(FullName),
%    fix_full_name(ASPName, FullASPName, _,'.pl'),
%    must_exist(FullASPName),
    asserta(sm(AliasName, IntModule,AbsInt, AbsAspName, [])).
%    create_all_files.

tm((:(X,model(Q))), (:(Z,model(Q))), _) :- !,
%	display('tm3:  model\n'),
	sm(X,Z,_,_,_).
%	dmsg(1,['model:', X,':',Q,' --> ',Z,':',Q, nl]).
tm((:(X,wfm(Q))), (:(Z,wfm(Q))), _) :- !,
%	display('tm3:  model\n'),
	sm(X,Z,_,_,_).
%	dmsg(1,['model:', X,':',Q,' --> ',Z,':',Q, nl]).
tm((:(X,assert(Y))), (:(Z,assert(Y))), _) :- !,
	sm(X,Z,_,_,_).
%	dmsg(1,['assert: ', X,':',Y,' --> ',Z,':',Y, nl]).
tm((:(X,assert_nb(Y))), (:(Z,assert_nb(Y))), _) :- !,
	sm(X,Z,_,_,_).
%	dmsg(1,['assert_nb:', X,':',Y,' --> ',Z,':',Y, nl]).
tm((:(X,retract(Y))), (:(Z,retract(Y))), _) :- !,
	sm(X,Z,_,_,_).
%	dmsg(1,['retract:', X,':',Y,' --> ',Z,':',Y, nl]).
tm((:(X,retract_nb(Y))), (:(Z,retract_nb(Y))), _) :- !,
	sm(X,Z,_,_,_).
%	dmsg(1,['retract_nb:', X,':',Y,' --> ',Z,':',Y, nl]).
tm((:(X,compute(N,L))), (:(Z,compute(N,L))), _) :- !,
	sm(X,Z,_,_,_).
%	dmsg(1,['compute:', X,':',(N,L),' --> ',Z,':',(N,L), nl]).
tm((:(X,change_parm(Y))), (:(Z,change_parm(Y))), _) :- !,
	sm(X,Z,_,_,_).
%	dmsg(1,['change_parm:', X,':',Y,' --> ',Z,':',Y, nl]).
tm((:(X,release(Q))), (:(Z,release(Q))), _) :- !,
	sm(X,Z,_,_,_).
%	dmsg(1,['release: ', X,':',Y,' --> ',Z,':',Y, nl]).
tm((:(X,traceASP(T))), (:(Z,traceASP(T))), _) :- !,
	sm(X,Z,_,_,_).
tm((:(X,traceQuit(T))), (:(Z,traceQuit(T))), _) :- !,
	sm(X,Z,_,_,_).
%tm(X,X,_) :- dmsg(1,['last tm: ', X, nl]).

tm((:(X,Y)), (:(Z,getSkep(Y,1))), _) :- 
	ground(X), sm(X,Z,_,_,_), !.
%	dmsg(1,['qualification: ', X,':',Y,' --> ',Z,':','getSkep(',Y,')\n']).
tm(not(:(X,Y)), (:(Z,getSkep(Y,0))), _) :- 
	ground(X), sm(X,Z,_,_,_), !.
%	dmsg(1,['qualification: ', X,':',Y,' --> ',Z,':','getSkep(',Y,')\n']).
%tm(X,X,_) :- display('term: X='),display(X),nl.

tm((:(C1,C2)), aspQualify:chkQualify(C1,C2), _) :- 
	\+ground(C1),
%	display('In tm4: C1:C2='), display(C1), display(':'),display(C2),nl,
	!,chkTerm.
%	display(M),
%	display(' In tm5: C1:C2='), display(C1), display(':'),display(C2),
%	display('translated to chkQualify'),nl.

tm('$static_instance_creation$','$static_instance_creation$',_) :-
%	display('erase chkTerm\n'),
	retractall_fact(chkTerm),!.
tm(end_of_file, end_of_file, _) :-
%	display('assert chkTerm\n'),
	assertz_fact(chkTerm),!.
tm(_,_,_) :- !, fail.

%gm(C1:C2, mtype(Q,0),_) :- 
%	display('----------------------------------------\n'),
%	display(C1), display(':'), display(C2), nl.

%cm(clause(0,0), clause(0,0), _) :- !.
%cm(clause(X,Y), clause(X,St), _) :- !,
%	dmsg(1,['clause qualify2: ', X,':-',Y,' -> ',nl]),
%	handle_clause(Y,St).
%	dmsg(1,[X,':-',St,nl]).

%cm(X,X,_) :- dmsg(1,['clause: ', X, nl]), read(_).

/*
handle_clause((C1,C2),(P1,P2)) :-!,
%	display(C1), nl,
	handleEachClause(C1,P1),
%	display('to:'), nl, display(P1), nl, read(_),
%	display('recursive call C2='),display(C2),nl,
	handle_clause(C2,P2).
%	display(C2), nl, display('to:'), nl, display(P2), nl, read(_).
handle_clause(C1,P1) :- 
%	display(C1), nl,
	handleEachClause(C1,P1).

handleEachClause((:(C1,C2)), (:(C1,C2))) :-
	ground(C1), !.
handleEachClause((not(:(C1,C2))), not(:(C1,C2))) :-
	ground(C1), !.
handleEachClause((:(C1,C2)), C3) :-
	var(C1),!,
%	display('In handleEachClause\n'),
	C3 = (mtype(C1,T),(T=0->(:(C1,C2));(T=1->(justTerm(C2,J),(J=0->(:(C1,C2));(:(C1,atom_val(C2)))))))).
%;(T=2->(:(C1,getSkep(C2,1)));(:(C1,C2)))))).
handleEachClause(not(:(C1,C2)), C3) :-
	var(C1),!,
	C3 = (mtype(C1,T),(T=0->(:(C1,C2));(T=1->(justTerm(C2,J),(J=0->(:(C1,C2));(:(C1,atom_val(C2)))))))).
%;(T=2->(:(C1,getSkep(C2,0)));(:(C1,C2)))))).
handleEachClause(C1,C1) :- atomic(C1),!.
%,display(C1), display(' atomic\n').
handleEachClause(C1,C1) :- list(C1),!.
%,display(C1), display(' list\n').
handleEachClause(C1,C1) :- var(C1),!.
%,display(C1), display(' var\n').
handleEachClause(C1,P1) :- term(C1), C1=.. L, 
%	display('term:'), display(C1), nl, display('L='),display(L),nl,
	handleClauseList(L,R),!,
%	display('R='),display(R), nl,
	P1 =.. R.
%, display('P1='),display(P1), nl.
handleEachClause(C1,C1).

handleClauseList([H],[H]) :- var(H), !.
%, display('var:'), display(H), nl.
handleClauseList([H],[R]) :- !,handleEachClause(H,R).
handleClauseList([H|T],[H|R]) :- var(H),!,
%,display('var:'), display(H), nl,
	handleClauseList(T,R).
handleClauseList([H|T],[RH|R]) :- 
%display('list: H='), display(H), nl,
	handleEachClause(H,RH), 
	handleClauseList(T,R).

% Q:A  ==> changed to: I, where A not in [model,assert,retract,assert_nb,
%                                         retract_nb,compute,change_parm,
%                                         trace,just,release,...].
%mType(Q,V),
%(V=0-> I=[Q:A];(V=1-> I=[Q:get(A)];(V=2-> I=[Q:get(A)];I=[Q:A]))).

prtPrologFiles :-
	prologFiles(M),
	display(M), nl,
	fail.
prtPrologFiles.
*/

emptyPrologFiles :-
	prologFiles(_),!.
emptyPrologFiles :-
	retractall_fact(sm(_,_,_,_,_)),
	retractall_fact(moduleTree(_,_)),
	retractall_fact(children(_)),
	retractall_fact(searchModule(_)).

buildMainModule(Module) :-
%	dmsg(1,['in buildMainModule', nl]),
%	retractall(aspFileName(_)),
	retractall(sm(_,_,_,_,_)),
	retractall(moduleTree(_,_)),
%	retractall(root(_)),

	absFileName(Module,AbsFileName),
%	winFileName(Module,WinFileNameExt),
%	display('aaaaaaaaaaaaaaaaaaaaa\n'),
%	dmsg(1,['WinFileNameExt====',WinFileNameExt,nl]),
%	dmsg(1,['AbsFileName===', AbsFileName, nl]),
%	assertz_fact(root(AbsFileName)),
	assertz_fact(searchModule(prolog(AbsFileName))),
%	assertz_fact(root(WinFileNameExt)),
%	assertz_fact(searchModule(prolog(WinFileNameExt))),
	buildModuleTree,
%	prtModuleTree,
%	dmsg(1, ['End Build ModuleTree--------------------',nl]),
	buildAspInterface,
%	dmsg(1,['end buildASPInterface',nl]),
%	retractall(moduleTree(_,_)),
	retractall(sm(_,_,_,_,_)).
%	retractall(aspFileName(_)), 
%	display('end\n\n').

buildModuleTree :-
	searchModule(_),
	findall(Module, searchModule(Module), ModuleList),
	retractall(searchModule(_)),
	searchModuleList(ModuleList),
	buildModuleTree,!.
buildModuleTree.

searchModuleList([]).
searchModuleList([prolog(H)|T]) :-
	moduleTree(prolog(H),_),!,
	searchModuleList(T).
searchModuleList([asp(H)|T]) :-
	moduleTree(asp(H),_),!,
	searchModuleList(T).
searchModuleList([H|T]) :-
	(H=asp(F) ; H=prolog(F)),
	openRead(H),
	searchModuleList(T).

saveChildren(M) :-
%	display('Calling prtModuleTree\n'),
%	prtModuleTree,
	retractall_fact(children(_)),
	findall(ASPModule, moduleTree(prolog(M),asp(_,ASPModule,_)), L1),
%	dmsg(1,['L1=', L1, nl]),
	aspList2IntList(L1,L2),
%	dmsg(1,['L2=', L2, nl]),
	list2Set(L2,L),
%	dmsg(1,['L=', L, nl]),
	retractall_fact(children(L)),
	assertz_fact(children(L)).

aspList2IntList([],[]).
aspList2IntList([A|T],[I|T1]) :- 
	aspName2IntName(A,I),
	aspList2IntList(T,T1).

list2Set([], []).
list2Set([A|T],L) :- member(A,T), !,list2Set(T,L).
list2Set([A|T],[A|T1]) :- list2Set(T,T1).



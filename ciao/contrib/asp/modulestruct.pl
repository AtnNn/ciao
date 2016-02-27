:- module(modulestruct, [buildModuleStruct/2]).

:- use_module(library(asp(h3))).
:- use_module(library(asp(misc))).

buildModuleStruct(S, AbsAspFileName) :-
%	dmsg(1,'\nbuildModuleStruct\n'),
%	dmsg(1,['AbsAspFileName=', AbsAspFileName, nl]),
%	format("AbsAspFileName=~k",AbsAspFileName), nl,
	scanModuleTreeChildren(S,AbsAspFileName,0),
	write(S,'parent(['),
	scanModuleTreeParent(S,AbsAspFileName,0),
	write(S,']).\n').

scanModuleTreeChildren(S,AbsAspFileName,Comma) :-
	findall(Child,moduleTree(asp(AbsAspFileName),Child),ChildList),
%	dmsg(1,['ChildList=',ChildList,nl]),
	getChildren(ChildList,ChildList1,0),

%	dmsg(1,['ChildList1=',ChildList1,nl, nl]),
	write(S, 'children(['),
	storeInt(S,ChildList1,Comma),
	write(S,']).\n'),
%	dmsg(1,['ChildList=',ChildList,nl, nl]), 
	writeModules(S,ChildList).
scanModuleTreeChildren(_,_,_).

% getChildren if C=0 both prolog and asp are in the returned list.
%             if C=1 prolog files only included in the returned list.
%             if C=2 asp files only included in the returned list.
getChildren([], [],_).
getChildren([prolog(I,N)|T],[prolog(I,M)|T1],C) :- !,
	C<2,
	absFile2Module(N,M),
	getChildren(T,T1,C).
getChildren([asp(I,N,_)|T],[asp(I,M)|T1],C) :- !,
	(C=0;C=2),
	aspName2IntName(N,M),
	getChildren(T,T1,C).

scanModuleTreeParent(S,AbsAspFileName,Comma) :-
	% I need to call ancestor of AbsAspFileName.
	% code below of findall is wrong.
%	findall(Parent,moduleTree(Parent,asp(_,AbsAspFileName,_)),ParentList),
%	prtModuleTree,
%	prtAnc(P,AbsAspFileName),
	findall(Parent, anc(Parent,AbsAspFileName),ParentList),
%	display('ParentList='), display(ParentList), nl,
	getIntName(ParentList, ParentList1),
%	display('ParentList1='), display(ParentList1), nl,
	storeInt(S,ParentList1,Comma).
scanModuleTreeParent(_,_,_).

getIntName([], []).
getIntName([prolog(_)|T],T1) :-
	getIntName(T,T1).
getIntName([asp(H)|T],[H1|T1]) :-
	aspName2IntName(H,H1),
	getIntName(T,T1).

storeInt(_,[],_).
storeInt(S,[H|T],C) :-
	storeOneModule(S,H,C),
	storeInt(S,T,1).

storeOneModule(S,prolog(PrologInt, PrologModule),Comma) :-
	writeComma(S,Comma),
	write(S,'prolog('''), write(S,PrologInt), write(S,''','''),
	write(S,PrologModule), write(S,''')'),
%	writeList2File(S,['prolog(''',PrologInt,''',''',PrologModule,''')']),
%	write(S,prolog(PrologInt,PrologModule)),
	!.
storeOneModule(S,asp(ASPInt,ASPModule),Comma) :-
	writeComma(S,Comma),
%	writeList2File(S,['asp(''',ASPInt,''',''',ASPModule,''')']),
	write(S,'asp('''), write(S,ASPInt), write(S,''','''),
	write(S,ASPModule), write(S,''')'),
%	write(S,asp(ASPInt, ASPModule)),
	!.
storeOneModule(S,IntName,Comma) :-
	writeComma(S,Comma),
	write(S,''''), write(S,IntName), write(S,''''),!.

writeModules(_,[]).
writeModules(S,[prolog(_,H)|T]) :- !,
%	display('prolog found'), nl, read(_),
	write(S,':- use_module('''),
	write(S,H),
	write(S,''').\n'),
	writeModules(S,T).
writeModules(S,[asp(_,H,_)|T]) :-
%	display(H), nl, read(),
	aspName2AbsIntName(H,M),
	write(S,':- use_module('''),
	write(S,M),
	write(S,''').\n'),
	writeModules(S,T).

writeComma(S,1) :- write(S,','),!.
writeComma(_,_).

anc(asp(X),Y) :- moduleTree(asp(X),asp(_,Y,_)).
anc(asp(X),Y) :- moduleTree(asp(X), prolog(_,Y)).
anc(prolog(X),Y) :- moduleTree(prolog(X), asp(_,Y,_)).
anc(prolog(X),Y) :- moduleTree(prolog(X), prolog(_,Y)).
anc(asp(X),Y) :- moduleTree(asp(X), asp(_,Z,_)),
	anc(Z,Y).
anc(asp(X),Y) :- moduleTree(asp(X), prolog(_,Z)),
	anc(Z,Y).
anc(prolog(X),Y) :- moduleTree(prolog(X), asp(_,Z,_)),
	anc(Z,Y).
anc(prolog(X),Y) :- moduleTree(prolog(X), prolog(_,Z)),
	anc(Z,Y).

%prtAnc(X,Y) :-
%	anc(X,Y),
%	display(X), display(' '), display(Y), nl,
%	fail.
%prtAnc(_,_).

%prtModuleTree :- moduleTree(A,B),
%      dmsg(1,[A,' ', B, nl]),
%      fail.
%prtModuleTree.

:- module(intclasses, [buildClasses/1]).

%:- use_module(library(system_extra)).
%:- use_module(inttrace).
%:- use_module(h3,[children/1]).

buildClasses(AbsPath) :-

%	splitFileExt(IntModuleName, ModuleName, _),
%	atoms_concat([AbsPath,ModuleName,Ext],PFE),

	atom_concat(AbsPath,'/atomclass.pl', PFAtomClass),
%	atom_concat(AbsPath,'/ruleclass.pl', PFRuleClass),
	atom_concat(AbsPath,'/skepclass.pl', PFSkepClass),
	atom_concat(AbsPath,'/justclass.pl', PFJustClass),
	atom_concat(AbsPath,'/wellfound.pl', PFWellClass),
	atom_concat(AbsPath,'/symboltab.pl', PFSymbolClass),
	atom_concat(AbsPath,'/aspQualify.pl',PFPrologFile),

%	atom_concat(PFE,'atomclass.pl', PFAtomClass),
%	atom_concat(PFE,'ruleclass.pl', PFRuleClass),
%	atom_concat(PFE,'skepclass.pl', PFSkepClass),
%	atom_concat(ModuleName,'j.pl', PFJustClass),
%	atom_concat(PFE,'trcclass.pl', PFTraceClass),

	buildAtomClass(PFAtomClass),
%	display('rule\n'),
%	buildRuleClass(PFRuleClass),
%	display('skep\n'),
	buildSkepClass(PFSkepClass),
	buildJustClass(PFJustClass),
	buildWellClass(PFWellClass),
	buildSymbolTableClass(PFSymbolClass),
	buildPrologFile(PFPrologFile).

%	display('just\n'),
%	buildJustClass(PFJustClass, IntModuleName).
%	buildTraceClass(PFTraceClass, IntModuleName).

%buildAtomClass(PFAtomClass) :- file_exists(PFAtomClass), !.
buildAtomClass(PFAtomClass) :-
	
%	fileName2ModuleName(PFAtomClass, FAtomClass),
	open(PFAtomClass, write, AtomClass),

	write(AtomClass, ':- class(atomclass).\n\n'),
%	write(AtomClass, FAtomClass),
%	write(AtomClass, ').\n\n'),

	write(AtomClass,':- use_package([objects]).\n\n'),
	
	write(AtomClass, ':- export(atom/2).\n'),
	write(AtomClass, ':- export(addAtom/2).\n'),
	write(AtomClass, ':- export(incRef/0).\n'),
	write(AtomClass, ':- export(decRef/0).\n\n'),
	
	write(AtomClass, ':- data atom/2.\n'),
	write(AtomClass, ':- data ref/1.\n\n'),

	write(AtomClass, 'ref(0).\n'),
	write(AtomClass,'incRef :- ref(C),!,retractall_fact(ref(_)),\n'),
	write(AtomClass,'	C1 is C+1,\n'),
	write(AtomClass,'	assertz_fact(ref(C1)).\n'),
	write(AtomClass,'incRef :- assertz_fact(ref(0)).\n'),
	write(AtomClass,'decRef :- ref(C), retractall_fact(ref(_)),\n'),
	write(AtomClass,'	C1 is C-1,\n'),
%	write(AtomClass,'	display(''decRef:atomClass:C1=''),display(C1),nl,\n'),
	write(AtomClass,'	assertz_fact(ref(C1)),\n'),
	write(AtomClass,'	(C1=0 -> self(SELF),destroy(SELF);true).\n\n'),

	write(AtomClass, 'addAtom(Index,AtomName) :-\n'),
	write(AtomClass,'	assertz_fact(atom(Index,AtomName)).\n\n'),

%	write(AtomClass,'destructor :-\n'),
%	write(AtomClass,'	display(''AtomClass destructor.''),nl.\n'),
	close(AtomClass).

/*
%buildAtomClass(PFRuleClass) :- file_exists(PFRuleClass), !.
buildRuleClass(PFRuleClass) :-
%	fileName2ModuleName(PFRuleClass, FRuleClass),
	open(PFRuleClass, write, RuleClass),

	write(RuleClass, ':- class(ruleclass).\n\n'),
%	write(RuleClass, FRuleClass),
%	write(RuleClass, ').\n\n'),

	write(RuleClass,':- use_package([objects]).\n\n'),
	
	write(RuleClass, ':- export(rule/3).\n'),
	write(RuleClass, ':- export(addRule/3).\n'),
	write(RuleClass, ':- export(incRef/0).\n'),
	write(RuleClass, ':- export(decRef/0).\n\n'),
	
	write(RuleClass, ':- data rule/3.\n'),
	write(RuleClass, ':- data ref/1.\n\n'),

	write(RuleClass, 'ref(0).\n'),
	write(RuleClass,'incRef :- ref(C),!,retractall_fact(ref(_)),\n'),
	write(RuleClass,'	C1 is C+1,\n'),
	write(RuleClass,'	assertz_fact(ref(C1)).\n'),
	write(RuleClass,'incRef :- assertz_fact(ref(0)).\n'),
	write(RuleClass,'decRef :- ref(C), retractall_fact(ref(_)),\n'),
	write(RuleClass,'	C1 is C-1,\n'),
%	write(RuleClass,'	display(''decRef:ruleClass:C1=''),display(C1),nl,\n'),
	write(RuleClass,'	assertz_fact(ref(C1)),\n'),
	write(RuleClass,'	(C1=0 -> self(SELF),destroy(SELF);true).\n'),

	
	write(RuleClass, 'addRule(Index,Head, Body) :-\n'),
	write(RuleClass,'	assertz_fact(rule(Index,Head,Body)).\n\n'),
%	write(RuleClass,'destructor :-\n'),
%	write(RuleClass,'	display(''RuleClass destructor.''),nl.\n'),
	close(RuleClass).
*/

%buildAtomClass(PFSkepClass) :- file_exists(PFSkepClass), !.
buildSkepClass(PFSkepClass) :-
%	fileName2ModuleName(PFSkepClass, FSkepClass),
	open(PFSkepClass, write, SkepClass),

	write(SkepClass, ':- class(skepclass).\n\n'),
%	write(SkepClass, FSkepClass),
%	write(SkepClass, ').\n\n'),

	write(SkepClass,':- use_package([objects]).\n'),
	write(SkepClass,':- use_module(library(aggregates)).\n\n'),
	
	write(SkepClass, ':- export(addSkep/2).\n'),
	write(SkepClass, ':- export(incRef/0).\n'),
	write(SkepClass, ':- export(decRef/0).\n'),
	write(SkepClass, ':- export(saveState/1).\n'),
	write(SkepClass, ':- export(addClasses/1).\n'),
%	write(SkepClass, ':- export(empty/0).\n'),
%	write(SkepClass, ':- export(setEmpty/0).\n'),
%	write(SkepClass, ':- export(resetEmpty/0).\n'),
	write(SkepClass, ':- export(getAllSkepTrue/1).\n'),
%	write(SkepClass, ':- export(replaceReason/2).\n\n'),

	write(SkepClass, ':- export(symboltableClass/1).\n'),
	write(SkepClass,' :- data symboltableClass/1.\n\n'),
	write(SkepClass, ':- data just/2, state/1.\n'),
	write(SkepClass, ':- data ref/1.\n'),

	write(SkepClass, ':- export(atomClass/1).\n'),
%	write(SkepClass, ':- export(ruleClass/1).\n'),
	write(SkepClass, ':- data atomClass/1, ruleClass/1.\n\n'),

	write(SkepClass, ':- export(just/2).\n\n'),

%	write(SkepClass, 'replaceReason(A,R) :-\n'),
%	write(SkepClass, '      retract_fact(just(A,V,_)),\n'),
%	write(SkepClass, '      assertz_fact(just(A,V,R)).\n\n'),

	write(SkepClass, 'ref(0).\n'),
	write(SkepClass,'incRef :- ref(C),retractall_fact(ref(_)),\n'),
	write(SkepClass,'	C1 is C+1,\n'),
	write(SkepClass,'	assertz_fact(ref(C1)).\n'),
	write(SkepClass,'incRef :- assertz_fact(ref(0)).\n'),
	write(SkepClass,'decRef :- ref(C),retractall_fact(ref(_)),\n'),
	write(SkepClass,'	C1 is C-1,\n'),
%	write(SkepClass,'       display(''decRef:skepclass:C1=''),display(C1),nl,\n'),
	write(SkepClass,'	assertz_fact(ref(C1)),\n'),
	write(SkepClass,'	(C1=0 -> (self(SELF),destroy(SELF));true).\n'),

	write(SkepClass,'saveState(State) :- state(State),!.\n'),
	write(SkepClass,'saveState(State) :- assertz_fact(state(State)).\n\n'),

	write(SkepClass,'addClasses(A) :- assertz_fact(atomClass(A)).\n'),
%	write(SkepClass,'	assertz_fact(ruleClass(R)).\n\n'),

	write(SkepClass,':- export(data_add/1).\n'),
	write(SkepClass,'data_add(symboltableClass(ST)):-\n'),
	write(SkepClass,'	assertz_fact(symboltableClass(ST)).\n\n'),

	write(SkepClass,'addSkep([],_).\n'),
	write(SkepClass,'addSkep([H|T],V) :-\n'),
	write(SkepClass,'	atomClass(A),\n'),
	write(SkepClass,'	A:atom(I,H),\n'),
	write(SkepClass,'	assertz_fact(just(I,V)),\n'),
	write(SkepClass,'	addSkep(T,V).\n\n'),

	write(SkepClass,':- export(getAtomNo/2).\n'),
        write(SkepClass,'getAtomNo(Atom,No):- atomClass(A),\n'),
%	write(SkepClass,'       prtAtom,\n'),
	write(SkepClass,'	A:atom(No,Atom).\n\n'),

	write(SkepClass,':- export(prtAtom/0).\n'),
	write(SkepClass,'prtAtom :- atomClass(A),\n'),
	write(SkepClass,'	A:atom(No,Atom),\n'),
	write(SkepClass,'	display(Atom), display('' ''), display(No),nl,\n'),
	write(SkepClass,'	fail.\n'),
	write(SkepClass,'prtAtom.\n\n'),

%	write(SkepClass, 'setEmpty :- assertz_fact(empty).\n'),
%	write(SkepClass, 'resetEmpty :- retractall_fact(empty).\n\n'),

	write(SkepClass, ':- export(getSkep/2).\n\n'),

	write(SkepClass,'getSkep(Atom,V) :- !,\n'),
	write(SkepClass,'	atomClass(A),\n'),
	write(SkepClass,'	A:atom(I,Atom), just(I,V).\n\n'),

%	write(SkepClass, ':- export(getTrueSkep/1).\n'),
%	write(SkepClass, ':- export(getFalseSkep/1).\n\n'),

%	write(SkepClass,'getTrueSkep(Atom) :- !,\n'),
%	write(SkepClass,'	atomClass(A),\n'),
%	write(SkepClass,'	A:atom(I,Atom), just(I,1).\n\n'),

%	write(SkepClass,'getFalseSkep(Atom) :- !,\n'),
%	write(SkepClass,'	atomClass(A),\n'),
%	write(SkepClass,'	A:atom(I,Atom), just(I,0).\n\n'),

	write(SkepClass,'getAllSkepTrue(Atoms) :- !,\n'),
	write(SkepClass,'	atomClass(A),\n'),
	write(SkepClass,'	findall(N,(just(I,1),A:atom(I,N)),Atoms).\n\n'),

write(SkepClass,'destructor :-\n'),
%    write(SkepClass,'    display(''destructor skepclass''),nl,\n'),
    write(SkepClass,'    retractall_fact(just(_,_)),\n'),

    write(SkepClass,'    atomClass(A),\n'),
    write(SkepClass,'    A:decRef,\n'),
    write(SkepClass,'    retractall_fact(atomClass(_)),\n'),

%    write(SkepClass,'   display(''SkepClass:ruleClass:R=''),nl,\n'),
%    write(SkepClass,'    ruleClass(R),\n'),
%    write(SkepClass,'   display(R),nl,\n'),
%    write(SkepClass,'    R:decRef,\n'),
%    write(SkepClass,'    retractall_fact(ruleClass(_)),\n'),

%    write(SkepClass,'   display(''SkepClass:symboltableClass:ST=''),nl,\n'),
    write(SkepClass,'    symboltableClass(ST),\n'),
%    write(SkepClass,'   display(ST),nl,\n'),
    write(SkepClass,'    ST:decRef,\n'),
    write(SkepClass,'    retractall_fact(symboltableClass(_)).\n'),
	

%	write(SkepClass,'truefalse(At,1,At).\n'),
%	write(SkepClass,'truefalse(At,0,Atom) :- \n'),
%	write(SkepClass,'	atom_concat(''not '',At,Atom).\n\n'),
	close(SkepClass).

% ----------------------------------------------
% Create ASP Class
% ----------------------------------------------
%buildJustClass(PFJustClass, IntModuleName) :-
buildJustClass(PFJustClass) :- 
%	fileName2ModuleName(PFJustClass, FJustClass),

%	display(PFJustClass), nl,
	open(PFJustClass, write, S), 

	write(S, ':- class(justclass).\n\n'), 
%	write(S, FJustClass), 
%	write(S, ').'), nl(S), nl(S), 
	write(S, ':- use_package([objects]).\n'),
	write(S, ':- use_module(library(aggregates)).\n'),
	write(S, ':- use_module(library(compiler)).\n'),
	write(S, ':- use_module(engine(internals)).\n'),

%	write(S, ':- use_module('''),
%	write(S, IntModuleName), write(S, ''').\n\n'),

	write(S,':- data aspInterface/1.\n'),
	write(S,':- data atomClass/1.\n'),
%	write(S,':- data ruleClass/1.\n'),
	write(S,':- data symboltableClass/1.\n'),
	write(S,':- data just/2.\n\n'),
	write(S,':- data flag/2.\n\n'),
	write(S,':- export(modelNo/1).\n'),
	write(S,':- data modelNo/1.\n\n'),

	write(S,':- use_class(atomclass).\n'),
%	write(S,':- use_class(ruleclass).\n'),
	write(S,':- use_class(symboltab).\n\n'),

	write(S, ':- export(getTrueAtoms/1).\n'),
	write(S, ':- export(getFalseAtoms/1).\n'),
	write(S, ':- export(unknown/1).\n'),
%	write(S, ':- export(draw_all_just/0).\n'),
%	write(S, ':- export(draw_atom_just/1).\n'),
	write(S, ':- export(data_add/1).\n'),
	write(S, ':- export(atomClass/1).\n'),
	write(S, ':- export(symboltableClass/1).\n'),
	write(S, ':- export(just/2).\n'),
	write(S, ':- export(atom_val/1).\n'),
%	write(S, ':- export(graph_just_all/1).\n'),
%	write(S, ':- export(graph_just_atoms/2).\n\n'),
%	write(S,' :- export(addModelNo/1).\n\n'),
	write(S, ':- export(getAtomNo/2).\n\n'),
%	put_pred(S),

%	write(S, 'constructor(_) :- \n'),
%write(S,'display(''call set current_prolog_flag''),nl,\n'),
%	write(S,'         current_prolog_flag(unknown,X),\n'),
%write(S,'display(''call assertz_fact''),nl,\n'),
%	write(S,'         assertz_fact(flag(unknown,X)),\n'),
%write(S,'display(''call set_prolog_flag''),nl,\n'),
%	write(S, '        set_prolog_flag(unknown,fail).\n\n'),

%	write(S,'addModelNo(No) :- assertz_fact(modelNo(No)).\n\n'),

	write(S,'justclass(X,No) :- assertz_fact(aspInterface(X)),\n'),
	write(S,'	assertz_fact(modelNo(No)).\n\n'),

    write(S, 'atom_val(Atom) :- \\+ground(Atom), !,\n'),
    write(S, '        getAtomNo(Atom,AtomNo), \n'),
    write(S,'         just(AtomNo, 1).\n'),
    write(S, 'atom_val(Atom) :- ground(Atom),\n'),
    write(S, '        getAtomNo(Atom,AtomNo), \n'),
    write(S, '        just(AtomNo,1),!.\n'),
    write(S,'atom_val(Atom) :- ground(Atom),\n'),
    write(S, '        getAtomNo(Atom,AtomNo), \n'),
    write(S,'         \\+just(AtomNo,0),\n'),
    write(S,'         !, loop_forever.\n'),

write(S,'loop_forever :- loop_forever.\n\n'),

    write(S,'unknown(Atom) :- getAtomNo(Atom,AtomNo),\n'),
    write(S,'         \\+just(AtomNo,_), loop_forever.\n\n'),

    write(S,'getAtomNo(Atom,No):- atomClass(A),A:atom(No,Atom).\n\n'),

    write(S, 'getTrueAtoms(L) :- var(L),\n'),
    write(S,'    atomClass(A),\n'),
    write(S,'    findall(Atom,(just(No,1),A:atom(No,Atom),Atom=.. [P|_],\\+atom_concat(aspprolog,_,P)),L).\n\n'),

    write(S, 'getFalseAtoms(L) :- var(L),\n'),
    write(S,'    atomClass(A),\n'),
    write(S,    'findall(Atom,(just(No,0),A:atom(No,Atom),Atom=.. [P|_],\\+atom_concat(aspprolog,_,P)),L).\n\n'),

    write(S,'data_add(atomClass(A)) :- assertz_fact(atomClass(A)).\n'),
    write(S,'data_add(just(No,V)) :- assertz_fact(just(No,V)).\n'),
    write(S,'data_add(symboltableClass(ST)) :-\n'),
    write(S,'    assertz_fact(symboltableClass(ST)).\n\n'),

    write(S, 'graph_just_atoms(AtomList, G) :- \n'),
    write(S, '         self(Name),\n'),

    write(S, '    aspInterface(IntName),\n'),
    write(S, '    use_module(IntName),\n'),
    write(S, '    module_concat(IntName,remFM,G2),\n'),
    write(S, '    ''$meta_call''(G2),\n'),
    write(S, '    module_concat(IntName,gen_graph(Name,AtomList,G),G1),\n'),
    write(S, '    ''$meta_call''(G1).\n\n'),

%    write(S,'         '''),
%    write(S,IntModuleName),
%    write(S,''':gen_graph(Name, AtomList, G).\n\n'),
    write(S, 'graph_just_all(G):-\n'),
    write(S,'    self(Name),\n'),
    write(S,'    atomClass(A),\n'),
    write(S,'    findall(Atom, A:atom(_,Atom), AtomList),\n'),

    write(S, '    aspInterface(IntName),\n'),
    write(S, '    use_module(IntName),\n'),
    write(S, '    module_concat(IntName,remFM,G2),\n'),
    write(S, '    ''$meta_call''(G2),\n'),
    write(S, '    module_concat(IntName,gen_graph(Name,AtomList,G),G1),\n'),
    write(S, '    ''$meta_call''(G1).\n\n'),

%    write(S,'         '''),   
%    write(S,IntModuleName),
%    write(S,''':gen_graph(Name,AtomList,G).\n\n'),
    
    write(S, 'draw_all_just :- \n'),
    write(S, '     self(Name),'),

    write(S, '    aspInterface(IntName),\n'),
    write(S, '    use_module(IntName),\n'),
    write(S, '    module_concat(IntName,remFM,G2),\n'),
    write(S, '    ''$meta_call''(G2),\n'),
    write(S, '    module_concat(IntName,justify_all(Name),G1),\n'),
    write(S, '    ''$meta_call''(G1).\n\n'),

%    write(S, IntModuleName),
%    write(S, ''':justify_all(Name).\n'),
    write(S, 'draw_atom_just(A) :- \n'),
    write(S, '    self(Name),'),

    write(S, '    aspInterface(IntName),\n'),
    write(S, '    use_module(IntName),\n'),
    write(S, '    module_concat(IntName,remFM,G2),\n'),
    write(S, '    ''$meta_call''(G2),\n'),
    write(S, '    module_concat(IntName,justify_atoms(A,Name),G1),\n'),
    write(S, '    ''$meta_call''(G1).\n\n'),

    write(S, ':- export(modJust/3).\n\n'),
    write(S,'modJust(B,S1,S2) :- self(Q),\n'),
    write(S,'    aspInterface(IntName),\n'),
    write(S,'    use_module(IntName),\n'),
    write(S,'     modelNo(No),\n'),
    write(S, '    module_concat(IntName,incFM(IntName,No),G2),\n'),
    write(S, '    ''$meta_call''(G2),\n'),
    write(S,'      module_concat(IntName,addHideFlag,G4),\n'),
    write(S,'      ''$meta_call''(G4),\n'),
    write(S,'    module_concat(IntName,jjjj(Q,B,S1,S2,0),G1),\n'),
    write(S,'    ''$meta_call''(G1).\n\n'),
%    close(S).

%    write(S, IntModuleName),
%    write(S, ''':justify_atoms(A,Name).\n'),
    write(S,'destructor :-\n'),
%    write(S,'    flag(unknown,UnknownFlag),\n'),
%    write(S,'    set_prolog_flag(unknown,UnknownFlag),\n'),
%    write(S,'    display(''destructor justclass''),nl,\n'),

    write(S,'    retractall_fact(just(_,_)),\n'),

    write(S,'    atomClass(A),\n'),
%    write(S,'  display(A),nl,\n'),
    write(S,'    A:decRef,\n'),
    write(S,'    retractall_fact(atomClass(_)),\n'),

%    write(S,'    ruleClass(R),\n'),
%    write(S,'    R:decRef,\n'),
%    write(S,'    retractall_fact(ruleClass(_)),\n'),

    write(S,'    symboltableClass(ST),\n'),
    write(S,'    ST:decRef,\n'),
    write(S,'    retractall_fact(symboltableClass(_)).\n\n'),
    close(S).

buildWellClass(PFWellClass) :-

open(PFWellClass, write, S), 
write(S,':- class(wellfound).\n\n'),

write(S,':- use_package([objects]).\n\n'),
write(S, ':- use_module(library(aggregates)).\n'),
write(S, ':- use_module(library(compiler)).\n'),
write(S, ':- use_module(engine(internals)).\n'),

%	write(S, ':- use_module('''),
%	write(S, IntModuleName), write(S, ''').\n\n'),

write(S,':- data aspInterface/1.\n'),
write(S,':- data atomClass/1.\n'),
%write(S,':- data ruleClass/1.\n'),
write(S,':- data symboltableClass/1.\n'),
write(S,':- data just/2.\n\n'),
write(S,':- data flag/2.\n\n'),
write(S,':- export(modelNo/1).\n'),
write(S,':- data modelNo/1.\n\n'),

write(S,':- use_class(atomclass).\n'),
%	write(S,':- use_class(ruleclass).\n'),
write(S,':- use_class(symboltab).\n\n'),

write(S, ':- export(getTrueAtoms/1).\n'),
write(S, ':- export(getFalseAtoms/1).\n'),
write(S, ':- export(unknown/1).\n'),
write(S, ':- export(data_add/1).\n'),
write(S, ':- export(atomClass/1).\n'),
write(S, ':- export(symboltableClass/1).\n'),
write(S, ':- export(just/2).\n'),
write(S, ':- export(atom_val/2).\n'),
write(S, ':- export(getAtomNo/2).\n\n'),

%	write(S, 'constructor(_) :- \n'),
%write(S,'display(''call set current_prolog_flag''),nl,\n'),
%	write(S,'         current_prolog_flag(unknown,X),\n'),
%write(S,'display(''call assertz_fact''),nl,\n'),
%	write(S,'         assertz_fact(flag(unknown,X)),\n'),
%write(S,'display(''call set_prolog_flag''),nl,\n'),
%	write(S, '        set_prolog_flag(unknown,fail).\n\n'),


write(S,'wellfound(X) :- assertz_fact(aspInterface(X)).\n\n'),

write(S,'atom_val(Atom,V) :- \n'),
write(S,'	getAtomNo(Atom,AtomNo),\n'),
write(S,'	just(AtomNo,V).\n\n'),

write(S,'unknown(Atom) :- getAtomNo(Atom,AtomNo),\n'),
write(S,'         \\+just(AtomNo,_).\n\n'),

write(S,'getAtomNo(Atom,No):- atomClass(A),A:atom(No,Atom).\n\n'),

write(S, 'getTrueAtoms(L) :- var(L),\n'),
write(S,'    atomClass(A),\n'),
write(S,'    findall(Atom,(just(No,1),A:atom(No,Atom),Atom=.. [P|_],\\+atom_concat(aspprolog,_,P)),L).\n\n'),

write(S, 'getFalseAtoms(L) :- var(L),\n'),
write(S,'    atomClass(A),\n'),
write(S,    'findall(Atom,(just(No,0),A:atom(No,Atom),Atom=.. [P|_],\\+atom_concat(aspprolog,_,P)),L).\n\n'),

write(S,'data_add(atomClass(A)) :- assertz_fact(atomClass(A)).\n'),
write(S,'data_add(just(No,V)) :- assertz_fact(just(No,V)).\n'),
write(S,'data_add(symboltableClass(ST)) :-\n'),
write(S,'    assertz_fact(symboltableClass(ST)).\n\n'),
close(S).

buildSymbolTableClass(PFSymbolClass) :-

open(PFSymbolClass, write, S), 
write(S,':- class(symboltab).\n\n'),

write(S,':- use_package([objects]).\n\n'),
write(S,':- data symbolTable/5.\n'),
write(S,':- export(symbolTable/5).\n\n'),

write(S,':- export(addSymbolTable/5).\n'),
write(S,'addSymbolTable(M,A1,A2,ARG,Type) :- \n'),
write(S,'	symbolTable(M,A1,A2,ARG,Type),!.\n'),
write(S,'addSymbolTable(M,A1,A2,ARG,Type) :- \n'),
write(S,'	assertz_fact(symbolTable(M,A1,A2,ARG,Type)).\n\n'),

write(S,'clearSymbolTable :- retractall_fact(symbolTable(_,_,_,_,_)).\n\n'),

write(S,':- export(prtSymbolTable/0).\n'),
write(S,'prtSymbolTable :- \n'),
write(S,'	symbolTable(M,A1,A2,ARG,Type),\n'),
write(S,'	display(M), display('' ''),\n'),
write(S,'	display(A1), display('' ''), display(A2), display('' ''),\n'),
write(S,'	display(ARG), display('' ''), display(Type), nl,fail.\n'),
write(S,'prtSymbolTable.\n\n'),

write(S,':- export(incRef/0).\n'),
write(S,':- export(decRef/0).\n'),
write(S, ':- data ref/1.\n\n'),
write(S, 'ref(0).\n'),
write(S,'incRef :- ref(C),retractall_fact(ref(_)),\n'),
write(S,'	C1 is C+1,\n'),
write(S,'	assertz_fact(ref(C1)).\n'),
write(S,'incRef :- assertz_fact(ref(0)).\n'),
write(S,'decRef :- ref(C),retractall_fact(ref(_)),\n'),
write(S,'	C1 is C-1,\n'),
%write(S,'       display(''decRef:symboltableClass:C1=''),display(C1),nl,\n'),

write(S,'	assertz_fact(ref(C1)),\n'),
write(S,'	(C1=0 -> self(SELF),destroy(SELF);true).\n'),

    close(S).
%    message(['end class']).

buildPrologFile(PFPrologFile) :-
	
open(PFPrologFile, write, S), 
write(S,':- module(aspQualify, [chkQualify/2]).\n\n'),
%display('h3:children'), nl,read(_),
%h3:children(L), display(L), nl, read(_),
%write(S,'children('), write(S,L),
%write(S,').\n\n'),

write(S,'mtype(Q,0) :- var(Q),!.\n'),
write(S,'mtype(Q,1) :- functor(Q,justclass,1),!.\n'),
write(S,'mtype(Q,1) :- functor(Q,wellfound,1),!.\n'),
%write(S,'mtype(Q,2) :- children(L),member(Q,L),!.\n'),
%write(S,'mtype(Q,3).\n\n'),

write(S,'justTerm(getTrueAtoms(_),0) :- !.\n'),
write(S,'justTerm(getFalseAtoms(_),0) :- !.\n'),
write(S,'justTerm(unknown(_),0) :- !.\n'),
write(S,'justTerm(draw_all_just,0) :- !.\n'),
write(S,'justTerm(draw_atom_just(_),0) :- !.\n'),
write(S,'justTerm(graph_just_all(_),0) :- !.\n'),
write(S,'justTerm(graph_just_atom(_,_),0) :- !.\n'),
write(S,'justTerm(_,1).\n\n'),

write(S,'doQualify(0,C1,C2) :- !,C1:C2.\n'),
write(S,'doQualify(1,C1,not(C2)) :- \+C1:atom_val(C2).\n'),
write(S,'doQualify(1,C1,C2) :- !,justTerm(C2,J),(J=0->C1:C2;C1:atom_val(C2)).\n'),
%write(S,'doQualify(2,C1,C2) :- !,C1:getSkep(C2,0).\n'),
%write(S,'doQualify(3,C1,C2) :- !,C1:C2.\n\n'),


write(S,'chkQualify(C1,C2) :- !,\n'),
%write(S,'	display('':mtype:''),\n'),
%write(S,'	display(C1),display('':''),display(C2),nl,\n'),
write(S,'	mtype(C1,T),\n'),
write(S,'	doQualify(T,C1,C2).\n\n'),

close(S).

:- class(symboltab).

:- use_package([objects]).

:- data symbolTable/5.
:- export(symbolTable/5).

:- export(addSymbolTable/5).
addSymbolTable(M,A1,A2,ARG,Type) :- 
	symbolTable(M,A1,A2,ARG,Type),!.
addSymbolTable(M,A1,A2,ARG,Type) :- 
	assertz_fact(symbolTable(M,A1,A2,ARG,Type)).

clearSymbolTable :- retractall_fact(symbolTable(_,_,_,_,_)).

:- export(prtSymbolTable/0).
prtSymbolTable :- 
	symbolTable(M,A1,A2,ARG,Type),
	display(M), display(' '),
	display(A1), display(' '), display(A2), display(' '),
	display(ARG), display(' '), display(Type), nl,fail.
prtSymbolTable.

:- export(incRef/0).
:- export(decRef/0).
:- data ref/1.

ref(0).
incRef :- ref(C),retractall_fact(ref(_)),
	C1 is C+1,
	assertz_fact(ref(C1)).
incRef :- assertz_fact(ref(0)).
decRef :- ref(C),retractall_fact(ref(_)),
	C1 is C-1,
	assertz_fact(ref(C1)),
	(C1=0 -> self(SELF),destroy(SELF);true).

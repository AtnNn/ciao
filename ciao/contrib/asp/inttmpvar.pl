:- module(inttmpvar, [buildTmpVar/1]).

buildTmpVar(S) :-
write(S,':- data varc/1.\n\n'),

write(S,'setVar(N) :- retractall_fact(varc(_)),\n'),
write(S,'	assertz_fact(varc(N)).\n\n'),

write(S,'incVar(N) :- var(N),\n'),
write(S,'	(varc(N) -> true;N=0),\n'),
write(S,'	N1 is N+1,\n'),
write(S,'	setVar(N1).\n\n'),

write(S,'getVar(TmpVar) :- var(TmpVar),\n'),
write(S,'	incVar(N),\n'),
write(S,'	cnv_atom(N,A),\n'),
write(S,'	atom_concat(''aspprolog'',A,TmpVar).\n\n'),

write(S,'tmpVarTerm(TmpVarName,ArgList,Term) :- \n'),
write(S,'	getVar(TmpVarName),\n'),
write(S,'	Term =.. [TmpVarName|ArgList].\n\n').

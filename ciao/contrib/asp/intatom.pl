:- module(intatom, [buildAtomRule/1]).

buildAtomRule(S) :-

write(S,':- data stateAtom/2.\n\n'),

write(S,'currentAtom(State, A) :-\n'),
write(S,'	stateAtom(State, A),!.\n'),
write(S,'currentAtom(State, A) :-\n'),
write(S,'       aspFileName(AspF),\n'),
write(S,'	children(L), (L=[]->F=AspF;atom_concat(AspF,''1'',F)),\n'),
write(S,'	atom_concat(''lparse -d all '',F,EXELP),\n'),
write(S,'	popen(EXELP,read,LP),\n'),
write(S,'	skip_lines(LP,State,A),!.\n'),

write(S,'skip_lines(LP,State,A) :- \n'),
write(S,'	repeat,\n'),
write(S,'        get_line(LP, String),\n'),
%write(S,'       write_string(String), nl,\n'),
write(S,'	String="0",\n'),
write(S,'        read_pred(LP, State, A), !.\n\n'),

write(S,'read_pred(LP, State, A) :- \n'),
write(S,'	A new atomclass,\n'),
write(S,'        repeat,\n'),
write(S,'        get_line(LP, String),\n'),
write(S,'        process_lparse(A, String),!,\n'),
write(S,'	asserta_fact(stateAtom(State,A)),\n'),
write(S,'	A:incRef.\n'),

write(S,'process_lparse(_,"0") :- !. \n'),
write(S,'process_lparse(A,String) :- \n'),
write(S,'       pos(" ", String, L1, L2),\n'),
write(S,'	string2term(L1,A1),\n'),
write(S,'       string2term(L2, A2),\n'),
write(S,'        A:addAtom(A1, A2), !,fail.\n\n').

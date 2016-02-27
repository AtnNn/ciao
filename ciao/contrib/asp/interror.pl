:- module(interror, [buildError/1]).

buildError(S) :-
	write(S,':- data error/3.\n'),

write(S,'err(_,M,Action,0) :-\n'),
write(S,'	display(''Module ''), display(M),display('': ''),\n'),
write(S,'	display(''Error in ''), display(Action), nl, \n'),
write(S,'	abortExec(0).\n'),

write(S,'err(error(syntax_error(_,_,_,L),Pred),M,Action,1) :-\n'),
write(S,'	display(''Module ''), display(M),display(': '),\n'),
write(S,'	display(''Error in ''), display(Pred), nl,\n'),
write(S,'	display(Action),nl,\n'),
write(S,'	display(''Entred term is:''),\n'),
write(S,'	atomList2string(L,Str),\n'),
write(S,'	name(A,Str),\n'),
write(S,'	display(A),!, nl.\n\n'),

write(S,'atomList2string([],"").\n'),
write(S,'atomList2string([''\n''|T],S) :- !,\n'),
write(S,'	atomList2string(T,S).\n'),
write(S,'atomList2string([''** here **''|T],S) :- !,\n'),
write(S,'	atomList2string(T,S).\n'),
write(S,'atomList2string([H|T],S) :-\n'),
write(S,'	name(H,A),\n'),
write(S,'	atomList2string(T,S1),\n'),
write(S,'	append(A,S1,S).\n\n'),

write(S,'err1(Stop, Msg, Pred) :-\n'),
write(S,'	dispError(Stop),\n'),
write(S,'	disp_list([Msg, '' in Pred:'', Pred]), nl,\n'),
write(S,'	abortExec(Stop).\n\n'),

write(S,'err2 :- \n'),
write(S,'	error(Stop,Msg,Pred),\n'),
write(S,'	dispError(Stop),\n'),
write(S,'	disp_list([Msg, '' in Pred:'', Pred]), nl,\n'),
write(S,'	abortExec(Stop).\n\n'),

write(S,'dispError(0) :- display(''Error:'').\n'),
write(S,'dispError(1) :- display(''Warning:'').\n\n'),

write(S,'abortExec(0) :- \n'),
write(S,'	display(''abort Execution.''),nl,nl,abort.\n'),
write(S,'abortExec(_).\n\n').

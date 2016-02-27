:- module(intrelease, [buildRelease/1]).

buildRelease(S) :-
	write(S,'% release a model name ASP_Name....'), nl(S),
	     write(S,'release(Q) :-'), nl(S),
	     write(S,'	remAspModel(Q), !,'), nl(S),
	     write(S,'	destroy(Q),!.'), nl(S),
	     write(S,'release(_).'), nl(S),
	     write(S,'% -----------------------------------------'), nl(S),

write(S,'remAspModel(Q) :- retract_fact(aspModel(_,_,Q)),!.\n'),
write(S,'remAspModel(_).\n\n').

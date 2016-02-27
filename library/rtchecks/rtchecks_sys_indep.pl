calls([],_Goal).
calls([(OldGoal : Call)|Calls],Goal):-
	instance(Goal,OldGoal),
	disentailed(Call,OldGoal,Goal),!,
	write('ERROR: undefined call for predicate '),
	write(OldGoal), nl,
	calls(Calls,Goal).
calls([_|Calls],Goal):-
	calls(Calls,Goal).

c_prec(Pred_Checks,NewName_G,NewChecks):-
	prec(Pred_Checks,NewName_G,NewChecks),
	(possibly_entailed(Pred_Checks,NewName_G) ->
	     true
	;
	     write('ERROR: undefined call for predicate '),
	     write(NewName_G), nl).


prec([],_Goal,[]).
prec([=>(OldGoal : Call, Succ)|Conds],Goal,NConds):-
	instance(Goal,OldGoal),
	entailed(Call,OldGoal,Goal),!,
	NConds = [(OldGoal, Call, Succ)|MoreConds],
	prec(Conds,Goal,MoreConds).
prec([_|Conds],Goal,NConds):-
	prec(Conds,Goal,NConds).

possibly_entailed([=>(OldGoal : Call, _Succ)|Conds],Goal):-
	(\+(instance(Goal,OldGoal)) ;
	 instance(Goal,OldGoal),disentailed(Call,OldGoal,Goal)),!,
	possibly_entailed(Conds,Goal).
possibly_entailed([_|_Conds],_Goal).


postc([],_).
postc([(OldGoal,Call , Succ)|Cs],Goal):-
	disentailed(Succ,OldGoal,Goal),!,
	copy_term(Goal,NewGoal),
	OldGoal= NewGoal,
	write('ERROR: for Goal '),
	write(OldGoal), nl,
	write('with Precondition '),
	write(Call), nl,
	write(' holds but Postcondition '),
	write(Succ),
	write(' does not.'),nl,
	postc(Cs,Goal).
postc([_|Cs],Goal):-
	postc(Cs,Goal).

disentailed(Prop,Goal,Goal):-
	is_disentailed(Prop).

is_disentailed([Cond]):-!,
	is_disentailed(Cond).
is_disentailed([Cond|Conds]):-!,
	(is_disentailed(Cond); is_disentailed(Conds)).
is_disentailed((L;R)):-!,
 	is_disentailed(L),
	is_disentailed(R).
is_disentailed(compat(Prop)):-!,
	system_dependent_incompatible(Prop).
is_disentailed(Prop):-
	system_dependent_disentailed(Prop).

entailed(Prop,OldGoal,Goal):-
	\+(\+((OldGoal=Goal,is_entailed(Prop)))).
	
is_entailed([Cond]):-!,
	is_entailed(Cond).
is_entailed([Cond|Conds]):-!,
	is_entailed(Cond),
	is_entailed(Conds).
is_entailed((L;R)):-!,
 	(is_entailed(L);is_entailed(R)).
is_entailed(compat(Prop)):-!,
	system_dependent_compatible(Prop).
is_entailed(Prop):-
	system_dependent_entailed(Prop).

%% For program points
check(Prop):-
	is_disentailed(Prop),!,
	write('ERROR: false program point assertion'),	nl,
	write(Prop),nl.
check(_).

check(Prop,Action):-
	is_disentailed(Prop),!,
	perform(Action,Prop).
check(_,_).

perform(pr,Prop):-!,
	show_prop(Prop).
perform(pp(Prog_Point),_):-!,
	show_prog_point(Prog_Point).
perform(pr_pp(Prog_Point),Prop):-!,
	show_prop_and_prog_point(Prop,Prog_Point).
perform(Action,_):-
	call(Action).

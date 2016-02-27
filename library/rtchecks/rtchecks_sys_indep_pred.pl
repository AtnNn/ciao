% checks calls assertions
calls([],_Goal).
calls([(Pred_desc, Cond)|_Calls],Goal):-
	instance(Goal,Pred_desc),
	Pred_desc = Goal,
	disentailed(Cond),
	write('ERROR: undefined call for predicate '),
	write(Goal), nl,
	fail.
calls([_|Calls],Goal):-
	calls(Calls,Goal).

% collects those success assertions whose precondition hold
prec([],_Goal,[]).
prec([(Pred_desc, Precond, Postcond)|RS],Goal,NRS):-
	instance(Goal,Pred_desc),
	test_entailed(Precond,Pred_desc,Goal), !,
	NRS = [(Pred_desc, Precond, Postcond)|MoreRS],
	prec(RS,Goal,MoreRS).
prec([_|RS],Goal,NRS):-
	prec(RS,Goal,NRS).

test_entailed(Prop,Pred_desc,Goal):-
	\+ \+ (Pred_desc=Goal, entailed(Prop)).
	
% checks success assertions
postc([],_).
postc([(Pred_desc,Prec , Postcond)|_Cs],Goal):-
	Pred_desc = Goal,
	disentailed(Postcond),
	write('ERROR: for Goal '),
	write(Pred_desc), nl,
	write('with Precondition '),
	write(Prec), nl,
	write(' holds but Postcondition '),
	write(Postcond),
	write(' does not.'),nl,
	fail.
postc([_|Cs],Goal):-
	postc(Cs,Goal).

% checks that at least one of the precondition of pred assertion may hold
c_prec(Pred_Checks,NewName_G,NewChecks):-
	prec(Pred_Checks,NewName_G,NewChecks),
	(possibly_entailed(Pred_Checks,NewName_G) ->
	     true
	;
	     write('ERROR: undefined call for predicate '),
	     write(NewName_G), nl).

possibly_entailed([Pred_ass|_RP],Goal):-
	pred_ass_possibly_entailed(Pred_ass,Goal).
possibly_entailed([_|RP],Goal):-
	possibly_entailed(RP,Goal).

pred_ass_possibly_entailed((Pred_desc, _Prec, _Postcond),Goal):-
	\+(instance(Goal,Pred_desc)), 
	!,
	fail.
pred_ass_possibly_entailed((Pred_desc, Prec, _Postcond),Goal):-
	instance(Goal,Pred_desc),
	Pred_desc = Goal,
	disentailed(Prec), 
	!, 
	fail. 
pred_ass_possibly_entailed((_Pred_desc, _Prec, _Postcond),_Goal):-
	true.

% checks those comp assertions whose precondition holds
check_comp([],_Goal).
check_comp([(Pred_desc,  Prec, CompProp)|_RC],Goal):-
	instance(Goal,Pred_desc),
	test_entailed(Prec,Pred_desc,Goal),
	Pred_desc = Goal,
	system_dependent_incompatible(CompProp),
	write('ERROR: for Goal '),
	write(Pred_desc), nl,
	write('with Precondition '),
	write(Prec), nl,
	write(' holds but CompProp '),
	remove_int_goal(CompProp,NCompProp),
	write(NCompProp),
	write(' does not.'), nl,
	fail.
check_comp([_|RC],Goal):-
	check_comp(RC,Goal).


% auxiliary predicates for issuing errors of comp assertions
remove_int_goal((L,R),(NL,NR)):-!,
	remove_int_goal(L,NL),
	remove_int_goal(R,NR).
remove_int_goal((L;R),(NL;NR)):-!,
	remove_int_goal(L,NL),
	remove_int_goal(R,NR).
remove_int_goal(CompProp,NCompProp):-
	functor(CompProp,F,A),
	A1 is A -1,
	functor(NCompProp,F,A1),
	shift_args(1,A,CompProp,NCompProp).

shift_args(Pos,N,Goal,NGoal):-
	Pos < N, !,
	Pos1 is Pos + 1,
	arg(Pos1,Goal,Arg),
	arg(Pos,NGoal,Arg),
	shift_args(Pos1,N,Goal,NGoal).
shift_args(_N1,_N,_Goal,_NGoal).

%% possibly_entailed([_|_RP],_Goal).
%% 
%% possibly_entailed([=>(Pred_desc : Prec, _Postcond)|RP],Goal):-
%% 	(\+(instance(Goal,Pred_desc)) ;
%% 	 instance(Goal,Pred_desc),disentailed(Prec,Pred_desc,Goal)), !,
%% 	possibly_entailed(RP,Goal).
%% possibly_entailed([_|_RP],_Goal).


%% disentailed([Prop]):-!,
%% 	disentailed(Prop).
%% disentailed([Prop|Props]):-!,
%% 	(disentailed(Prop); disentailed(Props)).
%% disentailed((L;R)):-!,
%%  	disentailed(L),
%% 	disentailed(R).
%% disentailed(compat(Prop)):-!,
%% 	system_dependent_incompatible(Prop).
%% disentailed(Prop):-
%% 	system_dependent_disentailed(Prop).

%% disentailed([Prop]):-!,
%% 	disentailed(Prop).
%% disentailed([Prop|Props]):-!,
%% 	(disentailed(Prop); disentailed(Props)).
%% disentailed((L;R)):-!,
%%  	disentailed(L),
%% 	disentailed(R).
%% disentailed(compat(Prop)):-!,
%% 	system_dependent_incompatible(Prop).
%% disentailed(Prop):-
%% 	system_dependent_disentailed(Prop).


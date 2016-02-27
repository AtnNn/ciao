
% first checks if preconditions hold, then executes goal
% finally checks if postconditions hold if precondition held
check_succ(Pre_Posts,Goal):-
	evaluate_preconds(Pre_Posts,Eval_Pre_Posts),
	callme(Goal),
	evaluate_postc_if_needed(Eval_Pre_Posts,Pre_Posts,Goal).

evaluate_preconds([],[]).
evaluate_preconds([=>(Pre,Post)|Pre_Posts],[=>(Value,Post)|Eval_Pre_Posts]):-
	(entailed(Pre) ->
	    Value = true
	;
	    Value = Pre), % maybe either false or unknown
	evaluate_preconds(Pre_Posts,Eval_Pre_Posts).

evaluate_postc_if_needed([],[],_Goal).
evaluate_postc_if_needed([=>(true,Post)|Eval_Pre_Posts],[A|Pre_Posts],Goal):-!,
	(disentailed(Post) ->
	    A = (=>(Pre,Post)),
	    write('ERROR: for Goal '),
	    write(Goal), nl,
	    write('with Precondition '),
	    write(Pre), nl,
	    write(' holds but Postcondition '),
	    write(Post),
	    write(' does not.'),nl
	;
	    true),
	evaluate_postc_if_needed(Eval_Pre_Posts,Pre_Posts,Goal).
evaluate_postc_if_needed([_|Eval_Pre_Posts],[_|Pre_Posts],Goal):-
	evaluate_postc_if_needed(Eval_Pre_Posts,Pre_Posts,Goal).

check_pred(Pre_Posts,Goal):-
	evaluate_preconds(Pre_Posts,Eval_Pre_Posts),
	(possibly_some_prec_holds(Eval_Pre_Posts) ->
	     true
	;
	     write('ERROR: undefined call for predicate '),
	     write(Goal), nl),
	callme(Goal),
	evaluate_postc_if_needed(Eval_Pre_Posts,Pre_Posts,Goal).

possibly_some_prec_holds([=>(true,_)|_]):-!.
possibly_some_prec_holds([=>(Pre,_)|_]):-
	\+(disentailed(Pre)),!.
possibly_some_prec_holds([_|Eval_Pre_Posts]):-
	possibly_some_prec_holds(Eval_Pre_Posts).

disentailed((Prop,Props)):-!,
	( disentailed(Prop); disentailed(Props) ).
disentailed((Prop;Props)):-!,
 	disentailed(Prop),
	disentailed(Props).
disentailed(compat(Prop)):-!,
	system_dependent_incompatible(Prop).
disentailed(Prop):-
	\+ \+ system_dependent_disentailed(Prop).
disentailed(Prop):-
	disproves(Prop,SufficientProp),
	entailed(SufficientProp).

entailed((Prop,Props)):-!,
	entailed(Prop),
	entailed(Props).
entailed((Prop;Props)):-!,
 	( entailed(Prop); entailed(Props) ).
entailed(compat(Prop)):-!,
	system_dependent_compatible(Prop).
entailed(Prop):-
	\+ \+ system_dependent_entailed(Prop).
entailed(Prop):-
	proves(Prop,SufficientProp),
	entailed(SufficientProp).

%% For program points
true(_).

checked(_).

trust(Cond):-
	check(Cond).

compat(Cond):-
	\+(\+(Cond)).

false(Cond):-
	write('ERROR: false program point assertion'),	nl,
	write(Cond),nl.

check(Cond):-
	disentailed(Cond), !,
	write('ERROR: false program point assertion'),	nl,
	write(Cond),nl.
check(_).
:- push_prolog_flag(multi_arity_warnings,off).
check(Cond,Action):-
	disentailed(Cond), !,
	perform(Action,Cond).
check(_,_).
:- pop_prolog_flag(multi_arity_warnings).

perform(pr,Cond):-!,
	write('ERROR: false program point assertion'),	nl,
	write(Cond),nl.
perform(pp(Prog_Point),_):-!,
	write('ERROR: false program point assertion'),	nl,
	write('at '),
	write(Prog_Point),nl.
perform(pr_pp(Prog_Point),Cond):-!,
        write('ERROR: false program point assertion'),  nl,
        write(Cond), nl, 
	write('at '), write(Prog_Point),nl.
perform(Action,_):-
	callme(Action).


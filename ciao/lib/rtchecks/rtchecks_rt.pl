:- module(rtchecks_rt, [
		condition/1,
		checkif_comp/4,
		checkc/6,
		checkc/4,
		checkif/8,
		rtcheck/7,
		rtcheck/3,
		send_prop_rtcheck/5,
		disj_prop/5,
		disj_prop/3,
		comploc_stack/3,
		call_stack/2,
		non_inst/2,
		non_compat/2,
		attach_cut_fail/2,
		select_defined/3
	    ],
	    [assertions, nortchecks, hiord]).

:- use_module(library(terms_vars)).
:- use_module(library(freeze)).

:- doc(author, "Edison Mera").

%:- doc(author, "Based on David Trallero's rtchecks package.").

:- doc(module, "This module contains the predicates that are
	required to implement run-time checks.").

%% check_comp copied from rtchecks_mod.pl to checkif_comp -- EMM

:- prop condition/1 + regtype.

condition(true).
condition(fail).

:- meta_predicate comploc_stack(?, goal, ?).

comploc_stack(Pos, Goal, PredName) :-
	intercept(Goal, rtcheck(comp, _, Prop, Valid, []),
	    send_signal(rtcheck(comp, PredName, Prop, Valid, Pos))).

:- pred checkif_comp(Condition, CompGoal, CompGoalArg, Head)

# "If @var{Condition} is @tt{true} then the @var{CompGoal} containing
the nested comp predicate calls is called with @var{Head} as
argument. To allow efficient implementation, @var{CompGoalArg} is the
last nested argument of @var{CompGoal}, so unifiying with @var{Head}
we have the comp check, and calling directly to @var{Head} we skip the
test. An example call could be:

@begin{verbatim}
checkif_comp(C,not_fails(is_det(A)),A,partition(_1,_2,_3,_4))
@end{verbatim}

so if C is true (it should come from @pred{checkc/2} predicate), then
A is unified with partition(_1,_2,_3,_4) and
not_fails(is_det(partition(_1,_2,_3,_4))) is called. Else, just
partiton(_1,_2,_3,_4) is called.".

% :- trust pred checkif_comp(Condition, CompGoal, CompGoalArg, Head)
% 	:: condition * callable * term * callable.

:- doc(bug, "checkif_comp/4 generates a unnecessary run-time
	module expansion").

:- meta_predicate checkif_comp(?, goal, ?, goal).
checkif_comp(true, Comp, Goal, Goal) :- call(Comp).
checkif_comp(fail, _,    _,    Goal) :- call(Goal).

:- doc(bug, "non_compat/2 and non_inst/2 are incompatible with
	attributed variables, due to the usage of the freeze/2
	predicate.").

selectvar(V, VL0, VL) :-
	var(V) -> VL0 = [V|VL] ; VL0 = VL.

selectvars([],    []).
selectvars([V|L], VL0) :-
	selectvar(V, VL0, VL),
	selectvars(L, VL).

:- meta_predicate non_compat(goal, ?).

non_compat(var(A),     _) :- !, nonvar(A).
non_compat(nonvar(A),  _) :- !, var(A).
non_compat(atm(A),     _) :- !, \+ atm(A).
non_compat(int(A),     _) :- !, \+ int(A).
non_compat(nnegint(A), _) :- !, \+ nnegint(A).
non_compat(num(A),     _) :- !, \+ num(A).
non_compat(Goal,       Args) :-
	varset(Args, VS),
	'$metachoice'(C),
	list(VS, freeze('$metacut'(C))),
	call(Goal),
	selectvars(Args, VS1),
	varset(VS1, VS2),
	list(VS2, detach_attribute),
	!,
	fail.
non_compat(_, _).

attach_cut_fail(V, C) :- attach_attribute(V, '$cut_fail'(V, C)).

:- meta_predicate non_inst(goal, ?).
non_inst(var(A),    _) :- !, nonvar(A).
non_inst(nonvar(A), _) :- !, var(A).
non_inst(gnd(A),    _) :- !, \+ ground(A).
non_inst(int(A),    _) :- !, \+ integer(A).
non_inst(num(A),    _) :- !, \+ number(A).
non_inst(atm(A),    _) :- !, \+ atom(A).
non_inst(Goal,      Args) :-
	varset(Args, VS),
	'$metachoice'(C),
	list(VS, attach_cut_fail(C)),
	Goal,
	list(VS, detach_attribute),
	!,
	fail.
non_inst(_, _).

:- multifile verify_attribute/2.

verify_attribute('$cut_fail'(Var, C), _) :-
	detach_attribute(Var),
	'$metacut'(C),
	fail.

:- multifile combine_attributes/2.
combine_attributes('$cut_fail'(V1, C), '$cut_fail'(V2, C)) :-
	detach_attribute(V1),
	detach_attribute(V2),
	V1 = V2,
	'$metacut'(C),
	fail.

:- meta_predicate disj_prop(list(goal), ?, ?, pred(2), ?).

disj_prop([Prop|Props], [PropArg|PropArgs], [PropName0|PropNames],
	    Verifier, PropName) :-
	Verifier(Prop, PropArg),
	PropName = PropName0
    ;
	disj_prop(Props, PropArgs, PropNames, Verifier, PropName).

:- meta_predicate disj_prop(list(goal), ?, pred(2)).

disj_prop([Prop|Props], [PropArg|PropArgs], Verifier) :-
	Verifier(Prop, PropArg)
    ;
	disj_prop(Props, PropArgs, Verifier).

:- meta_predicate checkc(list(goal), ?, ?, pred(2), ?, ?).
checkc(Props, PropArgs, PropNames, NegPropType, PropName, Exit) :-
	disj_prop(Props, PropArgs, PropNames, NegPropType, PropName) ->
	Exit = fail
    ;
	Exit = true.

:- meta_predicate checkc(list(goal), ?, pred(2), ?).
checkc(Props, PropArgs, NegPropType, Exit) :-
	disj_prop(Props, PropArgs, NegPropType) ->
	Exit = fail
    ;
	Exit = true.

:- meta_predicate checkif(?, ?, ?, list(goal), ?, ?, pred(2), ?).
checkif(fail, _,       _,        _,     _,        _,      _,        _).
checkif(true, ErrType, PredName, Props, PropArgs, NProps, Verifier, AsrLocs) :-
	rtcheck(ErrType, PredName, Props, PropArgs, NProps, Verifier, AsrLocs).

select_defined(N=V, SDict0, SDict) :-
	var(V) ->
	SDict = SDict0
    ;
	SDict0 = [N=V|SDict].

:- use_module(library(hiordlib)).
:- use_module(library(varnames(apply_dict))).
:- use_module(library(varnames(complete_dict))).

pretty_prop(Valid, Valid2) :-
	complete_dict(Valid, Valid, [], EDict),
	apply_dict(Valid, EDict, Valid1),
	map(Valid1, select_defined, Valid2, []).

send_prop_rtcheck(ErrType, PredName, PropName, Actual, AsrLocs) :-
	pretty_prop(Actual, PrettyActual),
	send_signal(rtcheck(ErrType, PredName, PropName, PrettyActual,
		AsrLocs)).

:- meta_predicate rtcheck(?, ?, list(goal), ?, ?, pred(2), ?).
rtcheck(ErrType, PredName, Props, PropArgs, NProps, Verifier, AsrLocs) :-
	disj_prop(Props, PropArgs, NProps, Verifier, PropName-ActualProp) ->
	send_prop_rtcheck(ErrType, PredName, PropName, ActualProp, AsrLocs)
    ;
	true.

:- meta_predicate rtcheck(goal, ?, ?).
rtcheck(Check, PredName, Loc) :-
	rtcheck_(Check, PredName, Loc),
	fail.
rtcheck(_, _, _).

:- meta_predicate rtcheck_(goal, ?, ?).
rtcheck_(Check, _, _) :-
	call(Check),
	!.
rtcheck_(Check, PredName, Loc) :-
	send_signal(rtcheck(pp_check, PredName, Check, [], [pploc(Loc)])).

:- meta_predicate call_stack(goal, ?).
call_stack(Goal, Pos) :-
	intercept(Goal, rtcheck(Type, PredName, PropName, Valid, Poss),
	    send_signal(rtcheck(Type, PredName, PropName, Valid, [Pos|Poss]))).

:- use_module(execution, [call_cleanup_det/2]).
:- use_module(database, [retractall/1]).
:- use_module(term, [expand_term/2, numbervars/3]).
:- use_module(lists, [append/3, member/2, length/2]).
:- use_module(io, [error_stream/1]).
:- use_module(misc, [format/3]).

'toplevel:main' :-
	'execution:init_builtins',
        catch(conv, E, handle_exception(E)).

handle_exception(E) :-
	error_stream(Err), 
	nl(Err),
	'execution:write_exception'(Err, E),
	nl(Err),
	halt(1).

warn(Message) :-
	error_stream(Err),
	write(Err, Message),
	nl(Err).

warn(Format, Args) :-
	error_stream(Err),
	format(Err, Format, Args).

conv :-
	read_file, !, write_file.

% --------------------------------------------------------------------------
% Reading in

read_file :-
	dynamic_clean('conv predicate'(_,_,_)),
	dynamic_clean('conv private'(_)),
	dynamic_clean('conv module'(_,_)),
	dynamic_clean('conv import'(_,_)),
	repeat,
	read_term(Cl, [singletons(S)]),
	(   S \= [] ->
	    clause_head(Cl, Clh),
	    functor(Clh, N, A),
	    singleton_names(S, SN),
	    (   SN \= [] -> %%%% pts %%%% Dat: added check for emptyness
	        warn('{Warning: Singleton variables ~w in ~q}\n',
		   [SN, N/A])
            ;   true
            )
	;   true
	),
	process_clause(Cl).

%%%% pts %%%% Dat: remove variables starting with underscores
singleton_names([], []).
singleton_names([Name=_|Singletons], Names) :-
	atom_codes(Name, NameC),
	(   NameC=[0'_|_] -> singleton_names(Singletons, Names) %'
	;   Names=[Name|Names1], singleton_names(Singletons, Names1)
	).

dynamic_clean(Pred) :-
	asserta(Pred),
	retractall(Pred).

process_clause(end_of_file) :-
	!.
process_clause((:-(module(Module, Public)))):-
	!,
	asserta('conv module'(Module, Public)),
	asserta('conv private'([])),
	fail.
process_clause((:-(use_module(Module, Import)))):-
	!,
	asserta('conv import'(Module, Import)),
	fail.
process_clause((:-(_))) :-
	!,
	fail.
process_clause((X,Y)) :-
	warn('Warning: defining '','' '-(X,Y)), fail.
process_clause((X;Y)) :-
	warn('Warning: defining '';'' '-(X;Y)), fail.
process_clause((X->Y)) :-
	warn('Warning: defining ''->'' '-(X->Y)), fail.
process_clause(Cl) :-
	expand_term(Cl, Cl1),
	add_module_converted(Cl1), !, 
	fail.


add_module_converted(Cl) :-
	module_head(Cl, MCl),
	add_converted(MCl).

add_converted(Cl) :-
	clause_head(Cl, Head),
	functor(Head, Name, Arity),
	get_predicate(Name, Arity, Pred),
	length(Pred, N),
	clause_conv(Name-Arity-N, Cl, H0, B0),
	escape_vars(H0-B0, H-B),
	numbervars(H-B, 0, NV),
	append(Pred, [clause(NV,H,B)], NewPred),
	put_predicate(Name, Arity, NewPred).

clause_head((Head :- _), Head) :- !.
clause_head(Head, Head).

clause_head_replace((Head :- Body), Head, (NewHead :- Body), NewHead) :- !.
clause_head_replace(Head, Head, NewHead, NewHead).

get_predicate(Name, Arity, Pred) :-
	retract('conv predicate'(Name, Arity, Pred)), !.
get_predicate(_, _, []).


put_predicate(Name, Arity, Pred) :-
	assertz('conv predicate'(Name, Arity, Pred)).


clause_conv(Ind, (Head:-Body), Head, BodyList) :- !,
	body_list(Body, Ind, [Head], [], BodyList).
clause_conv(_, Head, Head, []).
	

body_list(G, _, _, B0, [G|B0]) :-
	var(G), !.
body_list((G1;G2), Ind, Rest,  B0, B) :- !,
	convert_disjunction((G1;G2), Ind, Rest, B0, B).
body_list((G1->G2), Ind, Rest,  B0, B) :- !,
	convert_disjunction((G1->G2), Ind, Rest, B0, B).
body_list((G1,G2), Ind, Rest, B0, B) :- !,
	body_list(G2, Ind, [G1|Rest], B0, B1),
	body_list(G1, Ind, [G2|Rest], B1, B).
body_list(G, _, _, B0, [G|B0]).


escape_vars(T, E):-
	var(T), !, 
	E = T.
escape_vars('$VAR'(T), Escaped):-
	!,
	Escaped = '$VAR'('$VAR'(TE)),
	escape_vars(T, TE).
escape_vars(T, E):-
	compound(T), !, 
	T =.. [Func|Args],
	escape_vars_l(Args, ArgsE),
	E =.. [Func|ArgsE].
escape_vars(T, T).
	
escape_vars_l([], []).
escape_vars_l([A|Args], [AE|ArgsE]):-
	escape_vars(A, AE),
	escape_vars_l(Args, ArgsE).

% --------------------------------------------------------------------------

convert_disjunction(G, Ind, Rest, B0, B) :-
	no_cuts(G), !,
	length(B0, GN),
	disjunction_name(Ind, GN, Name),
	expand_disjunction(G, Rest, Name, Call),
	B = [Call|B0].
convert_disjunction(G, _, _, B0, B) :-
	B = ['$choice'(T), 'execution:interpret'(G, T) | B0].

%----------------
expand_disjunction(Disj, Rest, Name, Call) :-
	collect_vars(Disj, DisjVars0, []),
	collect_vars(Rest, RestVars, []),
	different_vars(DisjVars0, DisjVars),
	intersect(DisjVars, RestVars, CommonVars),
	Call =.. [Name|CommonVars],
	new_clauses(Disj, Call).

%----------------
new_clauses((Cond->Term1;Term2), Call) :-
	!,
	inline_unifications((Call :- Cond,!,Term1), Clause),
	add_converted(Clause),
	new_clauses(Term2, Call).	
new_clauses((Term1;Term2), Call) :-
	!,
	inline_unifications((Call :- Term1), Clause),
	add_converted(Clause),
	new_clauses(Term2, Call).
new_clauses((Cond->Term1), Call) :-
	!,
	inline_unifications((Call :- Cond,!,Term1), Clause),
	add_converted(Clause).
new_clauses(Term, Call):-
	inline_unifications((Call :- Term), Clause),
	add_converted(Clause).

%----------------
inline_unifications(Clause, ClauseInline):-
	copy_term(Clause, Clause1),
	inline_unifications_in(Clause1, ClauseInline).

inline_unifications_in((Head :- V1=V2), Head) :-
	(var(V1) ; var(V2)), !,
	V1 = V2.
inline_unifications_in((Head :- V1=V2, Goals), Clause) :-
	(var(V1) ; var(V2)), !,
	V1 = V2,
	inline_unifications_in((Head :- Goals), Clause).
inline_unifications_in(Clause, Clause).

%----------------
no_cuts((G1,G2)) :- !,
	no_cuts(G1),
	no_cuts(G2).
no_cuts((G1;G2)) :- !,
	no_cuts(G1),
	no_cuts(G2).
no_cuts((G1->G2)) :- !,
	no_cuts(G1),
	no_cuts(G2).
no_cuts((!)) :- !,
	fail.
no_cuts(_).
	
%----------------
disjunction_name(Name-Arity-N, GN, Newname):-
	atom_codes(Name, LName),
	number_codes(Arity, LArity),
	number_codes(N, LN),
	number_codes(GN, LGN),
	append(LN, [0'_|LGN], L1),
	append(LArity, [0'_|L1], L2),
	append(LName, [0'$|L2], LNewName),
	atom_codes(Newname, LNewName).

%----------------
%% collect_vars(Term, Vs, Vs0): Vs is the list of variables in Term prepended
%%      to Vs0.
collect_vars(V, Vs, Vs0) :-
	var(V), !,
	Vs = [V|Vs0].
collect_vars(C, Vs, Vs0) :-
	compound(C), !,
	C =.. [_|Args],
	collect_vars0(Args, Vs, Vs0).
collect_vars(_, Vs, Vs).

%% collect_vars0(List, Vs, Vs0): Vs is the list of variables in all of the
%%      terms in List prepended to Vs0.
collect_vars0([], Vs, Vs).
collect_vars0([H|T], Vs, Vs0) :-
	collect_vars(H, Vs1, Vs0),
	collect_vars0(T, Vs, Vs1).

intersect([], _, []).
intersect([H|T], L0, L) :-
	(    var_memberchk(H, L0) -> L = [H|L1]
	;    L = L1
	),
	intersect(T, L0, L1).

different_vars([], []).
different_vars([H|T0],L) :-
	(    \+ var_memberchk(H, T0) -> L = [H|T1]
	;    L = T1
	),
	different_vars(T0, T1).

var_memberchk(V, [X|_]) :- X == V, !.
var_memberchk(V, [_|Xs]) :- var_memberchk(V, Xs).

% --------------------------------------------------------------------------

module_name(Name, Module, MName) :-
	atom_codes(Module, L1),
	atom_codes(Name, L2),
	append(L1, [0':|L2], L3),
	atom_codes(MName, L3).

module_head(Cl, MCl) :-
	'conv module'(Module, Public),
	clause_head_replace(Cl, Head, MCl, MHead),
	functor(Head, Name, Arity),
	\+ member(Name/Arity, Public), !,
	retract('conv private'(Priv)),
	(   member(Name/Arity, Priv) -> Priv1 = Priv
        ;   Priv1 = [Name/Arity|Priv]
        ),
        asserta('conv private'(Priv1)),
	Head =.. [Name|Args],
	module_name(Name, Module, MName),
	MHead =.. [MName|Args].
module_head(Cl, Cl).

% --------------------------------------------------------------------------
module_term(T, [], T) :- !.
%module_term(T, Prefs, TM) :- !,
%	modularize(T, Prefs, TM).
module_term(V, Prefs, T) :- var(V), !,
	throw(bad(module_term(V,Prefs,T))).
module_term([], _Prefs, []) :- !.
module_term([C|T], Prefs, TM) :- !, %%%% pts %%%%
	module_term_clause_list(C, T, Prefs, TM).
module_term(T, Prefs, TM) :-
	throw(bad(module_term(T,Prefs,TM))).
	%modularize(T, Prefs, TM).

%%%% pts %%%% Modularize clause bodies only.
module_term_clause_list(C, T, Prefs, [CM|TM]) :-
	%print(user_error,CM), nl(user_error),
	%modularize([C|T], Prefs, [CM|TM]).
	(   nonvar(C), C=clause(NV,H,B) ->
	    CM=clause(NV,H,BB),
	    modularize_all(B, Prefs, BB)
	;   throw(bad(modularize__(C, Prefs, CM))),
	    modularize(C, Prefs, CM)
	),
	(   T = [CC|TT] -> module_term_clause_list(CC, TT, Prefs, TM)
	;   TM = T
	).

%%%% pts %%%%
% !! make this modular
% !! scan for imported meta_predicates
% Dat: `meta_predicate' is a SICStus reserved predicate -- so we use mymeta_predicate
mymeta_predicate(','(:,:)). % Dat: modularize/3 will never be called for this -- already converted
mymeta_predicate('->'(:,:)).
%mymeta_predicate('.'(:,?)).
mymeta_predicate('.'(:,:)). % Dat: list for '$asserts'
mymeta_predicate(';'(:,:)).
mymeta_predicate(\+(:)).
mymeta_predicate('^'(?,:)).
mymeta_predicate(call_cleanup_det(:,:)).
mymeta_predicate('$asserts'(:,:,?)).
mymeta_predicate('$abolish'(:)).
mymeta_predicate(with_input(?,?,:,:)). % Imp: is last arg? (get_end_of_stream)
mymeta_predicate(with_output(?,?,:,:)). % Imp: is last arg?
mymeta_predicate(nodebug(:)).
mymeta_predicate(err_check(:,dolist)).
mymeta_predicate(catch_some(:,?,:,?)). % Imp: is last `?' list-of-calls?
mymeta_predicate('execution:catch_some'(:,?,:,?)). % Imp: is last `?' list-of-calls?
mymeta_predicate('io:default_streams'(:)). % Imp: is last `?' list-of-calls?
mymeta_predicate(not(:)). %%%% pts %%%% Dat: needed, but not in SICStus
% vvv Dat: list from SICStus 3.12
mymeta_predicate(abolish(:)).
mymeta_predicate(abolish(:,?)).
mymeta_predicate(add_breakpoint(:,?)).
mymeta_predicate(assert(:)).
mymeta_predicate(assert(:,?)).
mymeta_predicate(asserta(:)).
mymeta_predicate(asserta(:,?)).
mymeta_predicate(assertz(:)).
mymeta_predicate(assertz(:,?)).
mymeta_predicate(bagof(?,:,?)).
% vvv Dat: we have '$set_bb'/2 etc., but they are not per-module
%mymeta_predicate(bb_delete(:,?)).
%mymeta_predicate(bb_get(:,?)).
%mymeta_predicate(bb_put(:,?)).
%mymeta_predicate(bb_update(:,?,?)).
mymeta_predicate(call(:)).
mymeta_predicate(call_cleanup(:,:)).
mymeta_predicate(call_residue(:,?)).
mymeta_predicate(catch(:,?,:)).
mymeta_predicate(clause(:,?)).
mymeta_predicate(clause(:,?,?)).
mymeta_predicate(compile(:)).
mymeta_predicate(consult(:)).
mymeta_predicate(current_breakpoint(:,?,?,?,?)).
mymeta_predicate(current_predicate(:)).
mymeta_predicate(current_predicate(?,:)).
mymeta_predicate(ensure_loaded(:)).
mymeta_predicate(execution_state(:)).
mymeta_predicate(execution_state(?,:)).
%mymeta_predicate(fcompile(:)).
mymeta_predicate(findall(?,:,?)).
mymeta_predicate(findall(?,:,?,?)).
mymeta_predicate(format(?,:)).
mymeta_predicate(format(?,?,:)).
mymeta_predicate(freeze(?,:)).
mymeta_predicate(if(:,:,:)).
mymeta_predicate(incore(:)).
%mymeta_predicate(initialization(:)).
mymeta_predicate(listing(:)).
mymeta_predicate(load(:)).
mymeta_predicate(load_files(:)).
mymeta_predicate(load_files(:,?)).
mymeta_predicate(load_foreign_files(:,?)).
mymeta_predicate(load_foreign_resource(:)).
mymeta_predicate(nospy(:)).
mymeta_predicate(on_exception(?,:,:)).
mymeta_predicate(once(:)).
mymeta_predicate(phrase(:,?)).
mymeta_predicate(phrase(:,?,?)).
mymeta_predicate(predicate_property(:,?)).
mymeta_predicate(profile_data(:,?,?,?)).
mymeta_predicate(profile_reset(:)).
mymeta_predicate(reconsult(:)).
mymeta_predicate(require(:)).
mymeta_predicate(retract(:)).
mymeta_predicate(retractall(:)).
mymeta_predicate(save_predicates(:,?)).
mymeta_predicate(save_program(?,:)).
mymeta_predicate(setof(?,:,?)).
%mymeta_predicate(source_file(:,?)).
%mymeta_predicate(spy(:,:)).
%mymeta_predicate(spy(:)).
mymeta_predicate(undo(:)).
mymeta_predicate(unload_foreign_resource(:)).
mymeta_predicate(use_module(:)).
mymeta_predicate(use_module(:,?)).
mymeta_predicate(use_module(?,:,?)).
mymeta_predicate(when(?,:)).

%%%% pts %%%%
modularize_all([], _, []).
modularize_all([Arg|Args], Prefs, [MArg|MArgs]) :-
	modularize(Arg, Prefs, MArg),
	modularize_all(Args, Prefs, MArgs).

modularize(T, Prefs, TM) :-
	callable(T), !,
	functor(T, Name, Arity), 
	%write(user_error, Name/Arity=T), nl(user_error), %%%% pts %%%%
	T =.. [Name|Args],
        modularize_name(Name, Arity, Prefs,  MName),
	%%%% pts %%%% Dat: added mymeta_predicate
	(   (functor(M, Name, Arity), mymeta_predicate(M)) -> % !!
	    M =.. [_|Metas], %%%% pts %%%% Imp: more efficiently
	    %write(user_error,Metas), nl(user_error),
	    modularize_args(Args, Metas, Prefs, MArgs)
	;   MArgs=Args
	),
	TM =.. [MName|MArgs].
modularize(T, _, T).

modularize_name(Name, Arity, Prefs, MName) :-
	member(Name/Arity-Module, Prefs), !,
	module_name(Name, Module, MName).
modularize_name(Name, _, _, Name).

%%%% pts %%%%
modularize_args([], _, _, []).
modularize_args([Arg|Args], [Meta|Metas], Prefs, [MArg|MArgs]) :-
	(   Meta == : -> modularize(Arg, Prefs, MArg)
	;   Meta == dolist -> modularize_dolist(Arg, Prefs, MArg)
	;   Arg=MArg
	),
	modularize_args(Args, Metas, Prefs, MArgs).

%%%% pts %%%%%
%** Dat: see the definition `T =.. List :-' in term.pl for an example of err_check(..., [..., do(...), ...]).
modularize_dolist(List, Prefs, OutList) :-
    (   nonvar(List), List=[H|T] ->
        (   nonvar(H), H=do(Call) ->
            modularize(Call, Prefs, OutCall),
            OutH=do(OutCall)
        ;   OutH=H
        ),
        OutList=[OutH|OutT],
        modularize_dolist(T, Prefs, OutT)
    ;   OutList=List
    ).

% --------------------------------------------------------------------------
make_prefix_list(Prefs) :-
	module_private(Prefs1),
	findall(IM, 'conv import'(IM, _), IMods),
	imported_modules(IMods, Prefs1, Prefs).

module_private(Prefs) :-
	'conv module'(Module, _), !,
	'conv private'(Private),
	module_prefix_list(Private, Module, [], Prefs).
module_private([]).


imported_modules([], Prefs, Prefs).
imported_modules([Module|Modules], Prefs0, Prefs) :-
	'conv import'(Module, Import),
	module_prefix_list(Import, Module, Prefs0, Prefs1),
	imported_modules(Modules, Prefs1, Prefs).

module_prefix_list([], _, Prefs, Prefs).
module_prefix_list([Pred|Preds], Module, Prefs0, Prefs):-
	module_prefix_list(Preds, Module, [Pred-Module|Prefs0], Prefs).

% --------------------------------------------------------------------------
% Writing out

write_file :-
	make_prefix_list(Prefs),
	retract('conv predicate'(Name, Arity, Pred)),
	write_pred(Name/Arity-Pred, Prefs),
	fail.
write_file.

write_pred(F/N-Cs, Prefs) :-
	module_term(Cs, Prefs, CsM),
	write_func(F, N, 'CLS', 0),
	write_expr(CsM, 2).

write_expr(E, F) :-
	float(E), !, write_num(E, 'FLT', F).
write_expr(E, I) :-
	integer(E), !, write_num(E, 'INT', I).
write_expr('$VAR'('$VAR'(X)), I) :- !,
	write_struct('$VAR'(X), '$VAR', 1, I).
write_expr('$VAR'(N), I):- !,
	write_num(N, 'VAR', I).
write_expr(E, I) :-
	functor(E, Func, Arity),
	atom(Func), !,
	write_struct(E, Func, Arity, I).
write_expr(E, _) :-
        write('WARNING: could not output '), write(E), nl. 

write_struct(E, Func, Arity, I):-
	write_func(Func, Arity, 'FUN', I),
	I1 is I+2,
	write_args(E, Arity, I1).

write_args(E, Arity, I) :-
	mybetween(1, Arity, ArgInd), %%%% pts %%%% Dat: for SWI-Prolog
	arg(ArgInd, E, Arg),
	write_expr(Arg, I),
	fail.
write_args(_,_,_).

write_num(Num, Tag, I) :-
	write_tag(Tag, I), write(Num), nl.

write_func(Name, Arity, Tag, I) :-
	write_tag(Tag, I), write(Arity), write_atom(Name), nl.

write_tag(Tag, I) :-
	mytab(I), write(Tag), write(' ').

write_atom(Name) :-
	mytab(1), atom_length(Name, Len), write(Len), mytab(1), write(Name).


% --------------------------------------------------------------------------
% Utilities

%%%% pts %%%% Dat: renamed between/3 -> mybetween/3 to avoid conflict with built-in between/3 of SWI-Prolog
mybetween(N, M, I) :-
	N =< M, I = N.
mybetween(N, M, I) :-
	N < M, N1 is N+1,
	mybetween(N1, M, I).

% ----------------
mytab(N):-
	N > 0,
	put_code(0' ),
	N1 is N - 1,
	mytab(N1).
mytab(0).


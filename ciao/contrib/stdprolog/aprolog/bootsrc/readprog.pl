:- module(readprog, [(.)/2, consult/1]).

:- use_module(database, [retractall/1]).
:- use_module(execution, [call_cleanup_det/2, convert_to_goal/2]).
:- use_module(err, [err_check/2]).
:- use_module(term, [expand_term/2]).
:- use_module(lists, [member/2, append/3, select/3]).
:- use_module(io, [error_stream/1]).
:- use_module(misc, [format/3]).

[File|S] :-
	consult_files([File|S]).

consult([File|S]) :- !,
	consult_files([File|S]).
consult(File) :-
	consult_files([File]).

consult_files([]).
consult_files([File|Files]) :-
	consult_file(File),
	consult_files(Files).


consult_file(File) :-
	get_file(File, FileRef, S),
	asserta('$consult_file'(FileRef)),
	asserta('$prev_clause'([])),
	asserta('$inits'([])),
	put_preds([]),
	call_cleanup_det((read_file(S), post_actions), end_consult(S)).
	
end_consult(S) :-
	get_preds(_),
	retract('$inits'(_)),
	retract('$prev_clause'(_)),
	retract('$consult_file'(_)), !,
	close(S).

post_actions :-
	preds(PL),
	add_uncreated_preds(PL),
	do_inits.

% ----------------
do_inits :-
	'$inits'(Inits), !,
	do_inits(Inits).

do_inits([]).
do_inits([Goal|Inits]) :-
	call(Goal),
	do_inits(Inits).

% ----------------
get_file(user, user, user_input) :- !.
get_file(File, FilePl, S) :-
	check_extension(File, FilePl),
	open(FilePl, read, S).

% ----------------
check_extension(File, FilePl) :-
	sub_atom(File, N, 3, 0, '.pl'), N > 0, !, 
	FilePl = File.
check_extension(File, FilePl) :-
	atom_concat(File, '.pl', FilePl), !.
check_extension(File, _) :-
	err_check(consult(File),
		   [inst(File), atom(File)]).
% ----------------
include_file(File) :-
	get_file(File, _, S), 
	call_cleanup_det(read_file(S), close(S)).

% ----------------
read_file(S) :-
	current_input(PS),
	set_input(S),
	call_cleanup_det(read_clauses, end_file(PS)).

end_file(PS) :-
	set_input(PS).

% ----------------
read_clauses :-
	repeat,
	read_term(T, [singletons(Sing)]),
	(   var(T)          -> consult_error(instantiation_error)
	;   T = end_of_file -> true
	;   callable(T)     -> add_to_program(T, Sing), fail
	;   consult_error(type_error(callable, T))
	), !.

% ----------------
add_to_program((:-(Directive)), _) :- !,
	do_directive(Directive).
add_to_program(T, Sing) :-
	expand_term(T, Cl),
	split_clause(Cl, Head, Body),
	functor(Head, F, A),
	PI = F/A,
	check_singletons(Sing, PI),
	check_props(PI, Dyn, Multi),
	add_clause(Dyn, Multi, PI, Head, Body),
	retract('$prev_clause'(_)),
	asserta('$prev_clause'(PI)), !.
add_to_program(T, _) :-
	consult_error(internal_error(adding(T))).

% ----------------
check_singletons([], _) :- !.
check_singletons(S, PI) :-
	singleton_names(S, SN),
	(   SN = [] -> true
	;   warn('Singleton variables ~w in ~q', [SN, PI])
	).

% ----------------
singleton_names([], []).
singleton_names([Name=_|Singletons], Names) :-
	atom_codes(Name, [0'_|_]), !,
	singleton_names(Singletons, Names).
singleton_names([Name=_|Singletons], [Name|Names]) :-
	singleton_names(Singletons, Names).	

% ----------------
do_directive(op(P,T,O)) :- !,
	op(P,T,O).
do_directive(char_conversion(IC, OC)) :- !,
	char_conversion(IC, OC).
do_directive(set_prolog_flag(Flag, Value)) :- !,
	set_prolog_flag(Flag, Value).
do_directive(initialization(Goal)) :- !,
	add_initialization(Goal).
do_directive(dynamic(PIS)) :- !,
	add_property(dynamic, PIS).
do_directive(multifile(PIS)) :- !,
	add_property(multifile, PIS).
do_directive(discontiguous(PIS)) :- !,
	add_property(discontiguous, PIS).
do_directive(include(Filename)) :- !,
	include_file(Filename).
do_directive(ensure_loaded(Filename)) :- !,
	consult_file(Filename).
do_directive(X) :- !,
	functor(X, N, A),
	warn('~q is not a valid directive', [N/A]).

% ---------------
add_initialization(Goal) :-
	retract('$inits'(Inits0)), !,
	append(Inits0, [Goal], Inits1),
	asserta('$inits'(Inits1)).

% ---------------
add_clause(dynamic, Multi, PI, Head, Body) :-
	add_dynamic(Multi, PI, Head, Body). 
add_clause(static, Multi, PI, Head, Body) :-
	add_static(Multi, PI, Head, Body).

% ---------------
add_dynamic(Multi, PI, Head, Body) :-
	convert_to_goal(Body, Body1),
	'$assertz'(Head, [Body1], Ref), !,
	add_multi(Multi, PI, Ref).
add_dynamic(_, _, _, Body) :-
	file_name(Filename),
	err_check(Filename, [conv_to_goal(Body)]).

% ---------------
add_static(Multi, PI, Head, Body) :-
	body_list(Body, B),
	'$asserts'(Head, B, Ref), !,
	add_multi(Multi, PI, Ref).
add_static(_, _, _, _) :-
	file_name(Filename),
	err_check(Filename, []).
	
% ---------------
add_multi(singlefile, _, _).
add_multi(multifile, PI, Ref) :-
	'$refclause'(Ref),
	file_name(Filename),
	retract('$pred_origin'(PI, _, Refs)),
	asserta('$pred_origin'(PI, Filename, [Ref|Refs])).
		
% ---------------
add_property(Prop, PIS) :-
	preds(PL0),
	directive_to_list(PIS, PIL),
	add_property_list(PIL, Prop, PL0, PL1),
	get_preds(_),
	put_preds(PL1).

add_property_list([], _, PL, PL).
add_property_list([PI|PIS], Prop, PL0, PL) :-
	check_pred_ind(PI, Prop),
	add_property_one(PI, Prop, PL0, PL1),
	add_property_list(PIS, Prop, PL1, PL).

add_property_one(PI, Prop, PL0, PL) :-
	select(props(PI, Created, PP), PL0, PL1), !,
	(   Created = yes ->
	    warn('directive ~w used after first occurance of procedure ~q',
		 [Prop, PI]),
	    PL = PL0
	;   add_property_to(Prop, PP, PP1),
	    PL = [props(PI, Created, PP1)|PL1]
	).
add_property_one(PI, Prop, PL0, PL) :-
	PL = [props(PI, no, [Prop])|PL0].
	    
add_property_to(Prop, PP0, PP) :-
	member(Prop, PP0), !,
	PP = PP0.
add_property_to(Prop, PP0, [Prop|PP0]).
	
check_pred_ind(PI, _) :-
	nonvar(PI), PI = _/_, !.
check_pred_ind(PI, Prop) :-
	err_check(Prop, [inst(PI), pred_ind(PI)]).

% ---------------		   
check_props(PI, Dyn, Multi) :-
	preds(PL0),
	get_prop(PI, props(PI, Created, PP), PL0, PL1),
	is_prop(PP, dynamic, static, Dyn),
	is_prop(PP, multifile, singlefile, Multi),
	(   Created = yes -> check_contiguity(PI, PP)
	;   create_pred(PI, PP, PL1, PL),
	    get_preds(_),
	    put_preds(PL)
	).

check_contiguity(PI, PP) :-
	'$prev_clause'(PPI),
	(   PI = PPI -> true
	;   member(discontiguous, PP) -> true
	;   warn('predicate ~q is not contiguous', [PI])
	).

% ---------------
get_prop(PI, Prop, PL, PL1) :-
	Prop = props(PI, _, _),
	select(Prop, PL, PL1), !.
get_prop(PI, props(PI, no, []), PL, PL).

% ---------------
add_uncreated_preds([]).
add_uncreated_preds([props(_, yes, _)|PL]) :- !,
	add_uncreated_preds(PL).
add_uncreated_preds([props(PI, _, PP)|PL]) :-
	create_pred(PI, PP, [], _),
	PI = F/A,
	functor(Head, F, A),
	'$pred_info'(Head, Type, _),
	(   Type = [] ->
	    is_prop(PP, dynamic, static, Dyn),
	    make_pred(Dyn, Head)
	;   true
	),
	add_uncreated_preds(PL).

make_pred(dynamic, Head) :-
	asserta(Head),
	retract(Head), !.
make_pred(static, Head) :-
	'$asserts'(Head, [], Ref),
	'$refclause'(Ref),
	'$delclause'(Ref).
	
% ---------------		   
create_pred(PI, PP, PL0, PL) :-
	% error_stream(ErrS), write(ErrS, adding-PI), nl(ErrS), %%%% pts %%%% !! check for verbose prolog flag
	is_prop(PP, dynamic, static, Dyn),
	is_prop(PP, multifile, singlefile, Multi),
	PI = F/A,
	functor(Head, F, A),
	'$pred_info'(Head, Type, Access),
	file_name(Filename), 
	check_access(Type, Access, PI),
	check_type(Type, Dyn, PI),
	(   Multi = singlefile -> check_singlefile(Filename, PI, Head)
	;   check_multifile(PI),
	    add_multifile(Filename, PI)
	),
	PL = [props(PI, yes, PP)|PL0].

% ----------------
check_access([], _, _) :- !.
check_access(_, access_user, _) :- !.
check_access(_, _, PredInd) :-
	consult_error(permission_error(redefine, builtin_predicate, PredInd)).

% ----------------
check_type([], _, _) :- !.
check_type(pred_static, static, _) :- !.
check_type(pred_dynamic, dynamic, _) :- !.
check_type(_, static, PI) :-
	consult_error(permission_error(modify, not_static_procedure, PI)).
check_type(_, dynamic, PI) :-
	consult_error(permission_error(modify, static_procedure, PI)).

% ----------------
check_singlefile(Filename, PI, Head) :-
	clause('$pred_origin'(PI, OrigFile, Multifile), _), !,
	(   Multifile \= singlefile ->
	    consult_error(permission_error(redefine, multifile, PI))
	;   OrigFile = Filename -> true
	;   consult_error(permission_error(redefine, original_file(OrigFile),
					   PI))
	),
	'$abolish'(Head).
check_singlefile(Filename, PI, Head) :-
	asserta('$pred_origin'(PI, Filename, singlefile)),
	'$abolish'(Head).

% ----------------
check_multifile(PI) :-
	clause('$pred_origin'(PI, _, Multifile), _), !,
	(   Multifile = singlefile ->
	    consult_error(permission_error(redefine, singlefile, PI))
	;   true
	).
check_multifile(_).

% ----------------
add_multifile(Filename, PI) :-
	(   retract('$pred_origin'(PI, Filename, Refs)) ->
	    del_clauses(Refs)
	;   true
	),
	asserta('$pred_origin'(PI, Filename, [])).

% ----------------
del_clauses([]).
del_clauses([Ref|Refs]) :-
	'$delclause'(Ref),
	del_clauses(Refs).

% ----------------
is_prop(PP, Prop, _, Result) :-
	member(Prop, PP), !,
	Result = Prop.
is_prop(_, _, NotProp, NotProp).

% ----------------
preds(PL) :-
	'$preds'(PL), !.

get_preds(PL) :-
	retract('$preds'(PL)), !.

put_preds(PL) :-
	asserta('$preds'(PL)).

% ----------------
warn(Format, Args) :-
	file_name(Filename),
	error_stream(ErrS),
	format(ErrS, '{Warning: ~w: ', [Filename]),
	format(ErrS, Format, Args),
	write(ErrS, '}\n').

consult_error(Error) :-
	file_name(Filename),
	throw(error(Error, Filename)).

file_name(Filename) :-
	'$consult_file'(Filename), !.

% ----------------
split_clause(Cl, Head, Body) :-
	nonvar(Cl),
	Cl = (Head :- Body), !.
split_clause(Head, Head, true).

% ----------------
body_list(Body, B) :-
	body_list(Body, [], B0),
	(   B0 = [true] -> B = []
	;   B = B0
	).

% ----------------
body_list(G, B0, [call(G)|B0]) :-
	var(G), !.
body_list((G1,G2), B0, B) :- !,
	body_list(G2, B0, B1),
	body_list(G1, B1, B).
body_list((G1;G2), B0, B) :- !,
	convert_goal((G1;G2), NG),
	B = ['$choice'(T), 'execution:interpret'(NG, T)|B0].
body_list((G1->G2), B0, B) :- !,
	convert_goal((G1->G2), NG),
	B = ['$choice'(T), 'execution:interpret'(NG, T)|B0].
body_list(G, B0, B) :-
	callable(G), !,
	B = [G|B0].
body_list(G, _, _) :-
	file_name(Filename),
	err_check(Filename, [callable(G)]).
	
% ----------------
convert_goal(G0, G) :-
	convert_to_goal(G0, G), !.
convert_goal(G0, _) :-
	file_name(Filename),
	err_check(Filename, [conv_to_goal(G0)]).

% ----------------

directive_to_list([], []) :- !.
directive_to_list([D|Rest], [D|Rest]) :- !.
directive_to_list((D, Rest), DList) :- !,
	seq_to_list((D, Rest), [], DList).
directive_to_list(D, [D]).
	
seq_to_list((D1, D2), L0, L) :- !,
	seq_to_list(D2, L0, L1),
	seq_to_list(D1, L1, L).
seq_to_list(D, B0, [D|B0]).


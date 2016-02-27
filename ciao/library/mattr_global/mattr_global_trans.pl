:- module(mattr_global_trans, _, [assertions, dcg]).

%% if we find a no protray clause then do not generate portray code

:- data attr_priority/2.

attr_priority(_, '500').

:- data no_portray/1.

:- data end_of_file_processed/1.

:- data is_pred_defined/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRANSFORMING CLAUSES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

mattr_def(0, _, M) :-
	retractall_fact(end_of_file_processed(M)),
	retractall_fact(is_pred_defined(M, _, _)).

%% FOR :- attributes
%% --- :- attribute at1,at/2 : priority(200) => imprimir el numero con TRES digitos
mattr_def((:- attribute_priority(P)),
	    CODE,
	    M) :-
	(
	    num(P)
	->
	    check_limits(P, NewPrio),
	    number_codes(NewPrio, NPC),
	    atom_codes(NPA, NPC),
	    retractall_fact(attr_priority(M, _)),
	    asserta_fact(attr_priority(M, NPA)),
	    generate_prio_module(M, Key),
	    CODE = (:- '$attribute_local'(Key))
	;
	    CODE = _,
	    message(error,
		['attribute_priority: Syntax is: ',
		    '`:- attribute_priority <positive number < 1000>'])
	).

% We want to keep the declaration because the file can be written to a
% file.
mattr_def((:- no_portray_mattr), [], M) :-
	asserta_fact(no_portray(M)),
	fail.

mattr_def((Head :- _Body), _, M) :-
	functor(Head, F, N),
	check_for_definition(F, N),
	set_as_defined_and_fail(M, F, N).

mattr_def(Fact, _, M) :-
	functor(Fact, F, N),
	check_for_definition(F, N),
	set_as_defined_and_fail(M, F, N).


%% FOR adding code at the end of the file :D
mattr_def(end_of_file, [
		(:-(multifile('$combine_attr'/4))),
		(:-(multifile('$check_attr'/3))),
		(:-(multifile('$portray_mattr'/2)))
		|PP
	    ],
	    M) :-
	\+ end_of_file_processed(M),
	asserta_fact(end_of_file_processed(M)),
	generate_prio_module(M, Key),
	add_code(M, combine_attr, 3,
	    ( '$combine_attr'(Key, MA1, MA2, O) :-
		M:combine_attr(MA1, MA2, O) ), PP,  PP1),
	add_code(M, check_attr,   2,
	    ( '$check_attr'(Key, MA1, X) :-
		M:check_attr(MA1, X) ),               PP1, PORTRAY),
	( ( current_fact(no_portray(M)) ;
		\+ current_fact(is_pred_defined(M, portray_mattr, 1)) )
	->
	    PORTRAY = end_of_file
	;
	    PORTRAY = [( '$portray_mattr'(Key, Attr) :-
		    M:portray_mattr(Attr) ),
		end_of_file]
	),
	retractall_fact(no_portray(_)).

check_for_definition(combine_attr,  3).
check_for_definition(check_attr,    2).
check_for_definition(portray_mattr, 1).

set_as_defined_and_fail(Module, Pred, Arity) :-
	(
	    current_fact(is_pred_defined(Module, Pred, Arity)) ->
	    fail
	;
	    assertz_fact(is_pred_defined(Module, Pred, Arity)),
	    fail
	).

add_code(A, B, C, Code, [Code|O1], O1) :-
	is_pred_defined(A, B, C),
	!.
add_code(_A, _B, _C, _Code, O1, O1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Precalculate attributes we can
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mattr_redef(get_attr(X, A), mattr_global_code:get_attr(X, A, PM), M) :-
	generate_prio_module(M, PM).

mattr_redef(set_attr(X, A), mattr_global_code:set_attr(X, A, PM), M) :-
	generate_prio_module(M, PM).

mattr_redef(detach_attr(X), mattr_global_code:detach_attr(X, PM), M) :-
	generate_prio_module(M, PM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END of code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_3digitn(AN, AN3) :-
	atom_length(AN, L),
	concat_zeroes(L, AN, AN3).

concat_zeroes(3, AN, AN).
concat_zeroes(2, AN, AN3) :- atom_concat('0',  AN, AN3).
concat_zeroes(1, AN, AN3) :- atom_concat('00', AN, AN3).


generate_prio_module(M, PM) :-
	(
	    attr_priority(M, P)
	->
	    true
	;
% this lines should never be reached
	    P = '500'
	),
	generate_3digitn(P, P3D),
	atom_concat(P3D, M, PM).


check_limits(P, 0) :-
	P < 0,
	!,
	message(error,
	    ['Priority should be between 0 and 999, rounding it to 0']).

check_limits(P, 999) :-
	P > 999,
	!,
	message(error,
	    ['Priority should be between 0 and 999, rounding it to 999']).

check_limits(P, P).

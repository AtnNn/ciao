:- module(mattr_local_common,
	    [dispatch_attr_directives/3,
		g_h_t/4,
		generate_prio_module/2,
		attr_convert/3,
		get_index/3,
		generate_hash_table/3
	    ], [assertions, dcg]).

:- use_module(library(odd)). % setarg


:- data attr_convert/3.
:- data attr_priority/1.

attr_priority('500').


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRANSFORMING CLAUSES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

dispatch_attr_directives((:- attribute(R)), _, M) :-
	transform_into_list(R, L, RL, []),
	retractall_fact(attr_convert(_, _, M)),
	asserta_fact(attr_convert(RL, L, M)).

dispatch_attr_directives((:- '$attribute_local'(R)), _, _) :-
	retractall_fact(attr_priority(_)),
	asserta_fact(attr_priority(R)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END of code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_hash_table(M, NewCodeList, L1) :-
	attr_convert(AttrList, Len, M),
	generate_prio_module(M, PM),
	NewCodeList = [(:-(fun_eval('@'/2)))|FunctionList],
	Len1 is Len + 1,
	functor(F, PM, Len1),
	g_f_t(AttrList, F, 2, FunctionList, HashList),
	g_h_t(AttrList, PM, 2, HashList),
	L1 is Len + 1,
	!.

generate_hash_table(_, [], 0) :-
	message(note,
	    [
'package mattr_local used and no \':- attribute\' declaration was found']).


% Generate Function Table. Example:
%
%  :- function @/2.
%
%  @(at1,test_local1(A, _, _)) := A.
%  @(at2,test_local1(_, B, _)) := B.
%  @(at3,test_local1(_, _, C)) := C.


g_f_t([],    _, _, A,                       A).
g_f_t([P|R], F, N, ['@'(Func, Attr, V)|R2], E) :-
	copy_term(F, Func),
	setarg(N, Func, V),
	(P = _/_ -> P = Attr/_ ; P = Attr),
	N1 is N + 1,
	g_f_t(R, F, N1, R2, E).




% Generate Hash Table
g_h_t([],    _,      _, [end_of_file]).
g_h_t([P|R], Module, N, ['$attr_hash$'(Module, P2, N)|R2]) :-
	(P = Pred/Ari -> P2 = f(Pred, Ari) ; P2 = f(P, no)),
	N1 is N + 1,
	g_h_t(R, Module, N1, R2).



% ----------------------------------------------------------------------------

generate_prio_module(M, PM) :-
	attr_priority(P),
	generate_3digitn(P, P3D),
	atom_concat(P3D, M, PM).


generate_3digitn(AN, AN3) :-
	atom_length(AN, L),
	concat_zeroes(L, AN, AN3).


concat_zeroes(3, AN, AN).
concat_zeroes(2, AN, AN3) :- atom_concat('0',  AN, AN3).
concat_zeroes(1, AN, AN3) :- atom_concat('00', AN, AN3).


% ----------------------------------------------------------------------------

transform_into_list((A, B), L) -->
	transform_into_list(A, L1),
	transform_into_list(B, L2),
	{L is L1 + L2}.

transform_into_list(A/N, 1) --> [A/N].

transform_into_list(A, 1) --> [A].

% ----------------------------------------------------------------------------

get_index(At, I1, M) :-
	attr_convert(L, _, M),
	functor(At, Attr, Ari),
	get_position(Attr/Ari, L, I, M),
	I1 is I + 1.


get_position(At/_, [At|_], 1, _) :- !.

get_position(At/X, [At/A|_], 1, M) :-
%D	message( note , ['paso1', At/X , At/A ] ),
	!,
	( X==A -> true ; message(error, ['Attribute \'', At, '/', X,
		    '\' defined in module \'', M,
		    '\' with different arity, check it out: \'',
		    At, '/', A, '\'']) ).

get_position(A/N, [], 1, M) :-
	!,
	message(error, ['Atribute \'', A, '/', N,
		'\' not defined in module \'', M, '\''
	    ]).

get_position(At/Ari, [_|R], N1, M) :-
	!,
%D	message( note , ['paso2', At/Ari , R] ),
	get_position(At/Ari, R, N, M),
	N1 is N + 1.

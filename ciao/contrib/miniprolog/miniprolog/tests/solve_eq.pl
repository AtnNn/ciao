:- module(_, _, [fsyntax]).

:- use_module(library(aggregates)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(write)).

:- include('../estimatecommon/_all/calib2eq').

% seq( A, B ) :- eq( B, A ).
% seq( A, C1 - C2 ) :- seq( A + K, C1 ), seq( K, C2 ).
% seq( A, C1 + C2 ) :- seq( A - K, C1 ), seq( K, C2 ).
% seq( A, C1 + C2 ) :- seq( A + K, C1 ), seq( - K, C2 ).
% seq( A, C1 - C2 ) :- seq( A - K, C1 ), seq( - K, C2 ).
% seq( A, C1 - C2 ) :- seq( K + A, C1 ), seq( K, C2 ).
% seq( A, C2 - C1 ) :- seq( K - A, C1 ), seq( K, C2 ).
% seq( A, C1 + C2 ) :- seq( K + A, C1 ), seq( - K, C2 ).
% seq( A, - C1 - C2 ) :- seq( K - A, C1 ), seq( - K, C2 ).

bytecode(BC) :-
	eq(_A, B),
	bytecode_eq(B, BC).

% bytecode_eq( B, B ) :-
% 	B =.. [ F, N ],
% 	atom( F ),
% 	number( N ),
% 	!.
bytecode_eq(B, B) :-
	atom(B),
	!.
bytecode_eq(A + B, BC) :-
	bytecode_eq(A, BC)
    ; bytecode_eq(B, BC).
bytecode_eq(A - B, BC) :-
	bytecode_eq(A, BC)
    ; bytecode_eq(B, BC).
bytecode_eq(- A, BC) :-
	bytecode_eq(A, BC).
bytecode_eq(A * B, BC) :-
	num(A),
	bytecode_eq(B, BC).

additionaleqs([0 = fail_1_0]).
% additionaleqs( [ 0 = trust_me_0_0 ] ).
% additionaleqs( [ 0 = retry_me_else_0_0 ] ).

maximacmd(solve(ELT, BCL)) :-
	findall(A = B, eq(A, B),     EL),
	findall(BC,    bytecode(BC), BCL0),
	sort(BCL0, BCL),
	length(EL,  NE),
	length(BCL, NBC),
	additionaleqs(AE),
	length(AE, NAE),
	(
	    NE + NAE =:= NBC -> true
	;
	    format("{WARNING: Number of equations and bytecodes differs:",
		[]),
	    nl,
	    format("Bytecodes: ~w\nEquations: ~w}\n", [NBC, NE+NAE])
	),
	append(AE, EL, ELT).

rankcmd :-
	findall(A = B, eq(A, B),     EL),
	findall(BC,    bytecode(BC), BCL0),
	sort(BCL0, BCL),
	define_matrix(EL, BCL, M0),
	M =.. [matrix|M0],
	display(rank(M)),
	nl.

define_matrix([],     _, []).
define_matrix([E|Es], B, [V|M]) :-
	define_vector(B, E, V),
	define_matrix(Es, B, M).

define_vector(B, E, V) :-
	normalize_eq(E, NE),
	define_vector_normalized(B, NE, V).

define_vector_normalized([],     _,  []).
define_vector_normalized([B|Bs], NE, [X|V]) :-
	(
	    member(C * B, NE) ->
	    X = C
	;
	    X = 0
	),
	define_vector_normalized(Bs, NE, V).

normalize_eq(_ = E, N) :-
	normalize_eq_(E, [], N).

normalize_eq_(E + T, NE0, NE) :-
	!,
	normalize_term(T, TN),
	normalize_eq_(E, [TN|NE0], NE).
normalize_eq_(T, NE0, [TN|NE0]) :-
	normalize_term(T, TN).

normalize_term(C * B, C * B) :- !.
normalize_term(B,     1 * B).

main :-
	maximacmd(A),
	write(A),
	display(';').

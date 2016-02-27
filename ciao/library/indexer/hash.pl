:- module(hash, [hash_term/2], [assertions, nativeprops, nortchecks]).

:- doc(author, "The CLIP Group").
:- doc(author, "Edison Mera").

:- doc(module, "Implementation of the rshash algorithm of
	hashing. It's Robert Sedgwick's <<Algorithms in C>> hash
	function.").

:- pred hash_term(T, N) : ground * var => int(N)
# "@var{N} is a hashing index for @var{T}.".
:- pred hash_term(T, N) : nonground * var => var(N).

hash_term(T, N) :-
	ground(T), !,
	hash_term_(T, 63689, _, 0, N).
hash_term(_, _).

% Quintus: hash_term(foo(name,2,module), 1391).

/*         hash_term(foo(name,2,module), 1389). */


% :- inline rshash_/5.

rshash_(X, A0, A1, H0, H1) :-
	H1 is (H0 * A0 + X) /\ 0xFFFFFFFF,
	A1 is (A0 * 378551) /\ 0xFFFFFFFF.

hash_term_(T, A0, A, N0, N) :-
	T =.. [X|List],
	hash_const(X, A0, A1, N0, N1),
	rshash_(0'(, A1, A2, N1, N2),
	hash_list(List, A2, A3, N2, N3),
	rshash_(0'), A3, A, N3, N).

hash_list([],     A,  A, N,  N).
hash_list([X|Xs], A0, A, N0, N) :-
	hash_term_(X, A0, A1, N0, N1),
	rshash_(0',, A1, A2, N1, N2),
	hash_list(Xs, A2, A, N2, N).

hash_const(X, A0, A, N0, N) :-
	name(X, L),
	rshash(L, A0, A, N0, N).

rshash([],     A,  A, H,  H).
rshash([X|Xs], A0, A, H0, H) :-
	rshash_(X, A0, A1, H0, H1),
	rshash(Xs, A1, A, H1, H).

:- module(relationships,
	    [father/2, son/2, grandfather/2, brother/2, familiar/2],
	    [assertions, isomodes, metatypes, hiord, nativeprops]).

:- doc(author, "Alvaro Sevilla San Mateo").

%:- use_module(ejemplo2).

father('juan',   'maria'). % juan is the father of maria
father('pablo',  'juan'). % pablo is the father of juan
father('pablo',  'marcela').
father('carlos', 'debora').

% A is the son of B if B the father of A
son(A, B) :- father(B, A).
% A is the grandfather of B if A is the father of C and C is the father of B 
grandfather(A, B) :-
	father(A, C),
	father(C, B).
% A and B are brothers if the father of A is also the father of B and A and B are different persons
brother(A, B) :-
	father(C, A),
	father(C, B),
	A \== B.

% A and B are familiars if A is the father,son or brother of B
familiar(A, B) :-
	father(A, B).
familiar(A, B) :-
	son(A, B).
familiar(A, B) :-
	brother(A, B).

:- test familiar(A, B) : (A = 'juan', B = 'marcela') + not_fails.
:- test familiar(A, B) : (A = 'pedro', B = 'pedro') + not_fails.
:- test familiar(A, B) : (A = 'debora', B = 'carlos') + fails.
:- test familiar(A, B) : (A = 'debora', B = 'nadie') + not_fails.
:- test brother(A, B) : (A = 'debora', B = 'carlos') + not_fails.

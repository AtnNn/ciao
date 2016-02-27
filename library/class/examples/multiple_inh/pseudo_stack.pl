:- class(pseudo_stack).

:- inherit_class(library('class/examples/stack')).
:- implements(library('class/examples/generic')).

callme :-
	display('Stack implementation'),
	nl.

set(Item) :-
	push(Item).

get(Item) :-
	top(Item).

:- use_module(library(sha1)).

main([In1,In2], [Out1,Out2]) :-
	sha1(In1, Out1),
	sha1(In2, Out2).

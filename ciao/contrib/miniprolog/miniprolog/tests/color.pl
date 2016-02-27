color_map(A, B, C, D, E) :-
	color(A), color(B), color(C), color(D), color(E),
	legal_coloring(A, B, C, D, E).

legal_coloring(A, B, C, D, E) :-
	A \== B,
	A \== C,
	A \== D,
	A \== E,
	c(B, C, D),
	C \== E.

c(X, Y, Z) :-
	X \== Y,
	X \== Z.

color(blue).
color(green).
color(orange).
color(red).
color(yellow).

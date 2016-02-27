
:- module(dancing_shapes,[main/0],[objects]).

:- use_class(library('class/examples/geometry/oval_class')).
:- use_class(library('class/examples/geometry/canvas_class')).
:- use_class(library('class/examples/geometry/poly_class')).

:- data dancing/1.

wanna_dance(Shape) :-
	Shape instance_of shape_class,
	Shape interface mobile,
	( dancing(Shape) -> true ; 
	    asserta_fact(dancing(Shape))
	),
	true.

lets_dance :-
	delay(10),
	dancing(
	
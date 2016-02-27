%%---------------------------------------------------------------------
%%
%% DANCING SHAPES CANVAS
%%
%%---------------------------------------------------------------------

:- class(disco_class,[],[objects]).

:- inherit_class(library('class/examples/geometry/canvas_class')).
:- use_class(library('class/examples/geometry/shape_class')).
:- use_class(library('class/examples/geometry/mobile')).

:- use_module(library(random)).
:- use_module(library(system)).

%%---------------------------------------------------------------------
%% OVERRIDEN ADD_ITEM
%%---------------------------------------------------------------------

:- data dancing/3.
:- inheritable dancing/3.

:- export(add_item/1).

add_item(Shape) :-
	inherited add_item(Shape),
	wanna_dance(Shape).

wanna_dance(Shape) :-
	Shape instance_of shape_class,
	Shape interface mobile,
	initial_movement(X,Y),
	( dancing(Shape,_,_) -> true ; 
	    asserta_fact(dancing(Shape,X,Y))
	),
	true.

initial_movement(X,Y) :-
	random(-5,5,X),
	random(-5,5,Y).

next_movement(C,Inc,NInc) :-
	( C<0 ; C>200 ),
	!,
	NInc is -Inc.

next_movement(_,Inc,Inc).


%%---------------------------------------------------------------------
%% DANCING CAPABILITIES
%%---------------------------------------------------------------------

:- export(lets_dance/0).

lets_dance :-
	retract_fact(dancing(Shape,IncX,IncY)),
	Shape:move(IncX,IncY),
	Shape:get_spot(X,Y),
	next_movement(X,IncX,NX),
	next_movement(Y,IncY,NY),
	asserta_fact(dancing(Shape,NX,NY)),
	fail.

lets_dance.

:- data dancing/1.

:- export(dammed_dance/0).

dammed_dance :-
	\+ current_fact(dancing(_)),
	eng_call(dammed_dance_aux, create, create, Goal),
	asserta_fact(dancing(Goal)).

dammed_dance_aux :-
	pause(1),
%	my_pause(10000),
	lets_dance,
	dammed_dance_aux.

%%---------------------------------------------------------------------

my_pause(0).
my_pause(Delay) :- 
	Delay > 0,
	NDelay is Delay - 1,
	my_pause(NDelay).
	


%%---------------------------------------------------------------------
%% CONSTRUCTORS
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

disco_class :-
	canvas_class.

disco_class(L) :-
	canvas_class(L),
	fail.

disco_class([]).
disco_class([Item|Next]) :-
	( wanna_dance(Item) ; true ),
	!,
	disco_class(Next).

%%---------------------------------------------------------------------

destructor :-
	dancing(Goal),
	eng_kill(Goal).

%%---------------------------------------------------------------------

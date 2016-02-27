:- module(widget2,[test/0],[objects]).

:- use_module(library(system),[pause/1]).
:- use_module(library('tcltk/examples/tk_test_aux')).

:- use_class(library('tcltk/examples/widget_class/window_class')).
:- use_class(library('tcltk/examples/widget_class/widget_class')).
:- use_class(library('tcltk/examples/widget_class/button_class')).
:- use_class(library('tcltk/examples/widget_class/label_class')).
:- use_class(library('tcltk/examples/widget_class/entry_class')).


test :- 
	B1 new button_class,
%	B1:set_text('Factorial'),
%	B1:set_font('times'),
%	B1:set_relief('raised'),
%	B1:set_side('left'),
%	B1:get_font(X),

	W1 new window_class([B1]),
        display(1),nl,
        W1:show,
	display(2),



%	W1:event_loop,

	hit_enter,

%	destroy W1,
true.

hit_enter :-
	display('Hit ENTER to continue...'),
	nl,
	get_code(_).












:- module(canvas,[test/0]).

:- use_module(library('tcltk/tcltk')). 
:- use_module(library('tcltk/examples/tk_test_aux')). 
:- use_module(library(aggregates)).
:- use_module(library(strings)).
:- use_module(library(system)).
:- use_module(library(lists),[append/3]).

:- export(test_aux/1).

% Example to probe that the returning values to prolog is correct in the third argument of the predicate tcl_eval.

test:-
	tcl_new(Interp),
	test_aux(Interp).


test_aux(Interp):-
	tcl_eval(Interp,[canvas, '.c -height 140 -width 140 -background white'],_),
	tcl_eval(Interp,[pack, '.c'],_),
	tcl_eval(Interp,['.c', 'create oval 7 7 133 133 -outline black -fill gray80 -width 2'],ID),
	tcl_eval(Interp,['.c', 'create oval 39 49 53 63 -outline black -fill black'],ID1),
	tcl_eval(Interp,['.c', 'create oval 102 63 88 49 -outline black -fill black'],ID2),
	tcl_eval(Interp,['.c', 'create polygon 70 67 74 81 69 77 67 81  -fill black'],ID3 ),
	tcl_eval(Interp,['.c', 'create arc 21 21 119 119 -start 225 -extent 95 -style arc -outline black -width 3'],ID4),
	display('Hit enter to continue'),
	nl,
	get_code(_),
	tcl_eval(Interp,['.c','move', ID2,10,10],_).


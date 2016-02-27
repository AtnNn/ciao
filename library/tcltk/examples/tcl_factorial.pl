:- module(tcl_factorial,[test/0]).

:- use_module(library('tcltk/examples/tk_test_aux')).

:- use_module(library(tcltk)).
:- use_module(library(lists)).
:- use_module(library(format)).

:-export(test/0).

test :-
	tcl_new(X),
	test_aux(X).

test_aux(X) :-
	tcl_eval(X,[button,'.b',min(text),dq('Factorial')],_),
	tcl_eval(X,[button,'.c','-text',dq('Quit')],_),
	tcl_eval(X,[entry,'.e1',min(textvariable),'inputval'],_),
	tcl_eval(X,[label,'.l1',min(text),dq('El factorial de ')],_),
	tcl_eval(X,[pack, '.l1','.e1'],_),
	tcl_eval(X,[entry,'.e2',min(textvariable),'outputval'],_),
	tcl_eval(X,[label,'.l2',min(text),dq('es  ')],_),
        tcl_eval(X,[pack, '.l2','.e2'],_),
	tcl_eval(X,[pack,'.b','.c',min(side),'left'],_),
	tcl_eval(X,[bind,'.b','<ButtonPress-1>',
                    br([set,'inputval','$inputval','\n',
		    prolog_one_event,dq(write(execute(tk_test_aux:factorial('$inputval','Outputval')))),'\n',
                    set, 'outputval','$prolog_variables(Outputval)'])],_),
	tcl_eval(X,[bind,'.c','<ButtonPress-1>',
	            br([prolog_one_event,dq(write(execute(tk_test_aux:quit)))])],_),
	tk_event_loop(X).

%test_aux(X) :-
%	tcl_delete(X).






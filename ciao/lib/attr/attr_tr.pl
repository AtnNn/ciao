:- module(attr_tr, [sentence/3], []).

:- use_module(library(messages), [warning_message/2]).

:- data found_hook/2.

reset(M):-
	retractall_fact(found_hook(M, _)).

add_hook(M, T):-
	current_fact(found_hook(M, T)), !.
add_hook(M, T):-
	assertz_fact(found_hook(M, T)), !.

sentence(0, _, Module):-
	reset(Module), 
	fail.

sentence(Clause, _, Module):-
	(Clause = (Head:-_) -> true ; Clause = Head),
	functor(Head, F, N), 
	(
%	    F=attr_unify_hook, N=2 ->
%	    add_hook(Module, attr_unify_hook)
%	;
	    F=portray_attrs, N=2 ->
	    add_hook(Module, attr_portray_attrs)
	;
	    F=attribute_goal, N=2 ->
	    add_hook(Module, attribute_goal)
	), 
	fail.
	    
sentence(end_of_file, Tail0, Module):-
	(
	    Tail0 = [ (:- multifile 'attr_rt:unify_hook'/3), 
	              ('attr_rt:unify_hook'(Module, V, Other):- 
		          attr_unify_hook(V, Other)) |
                      Tail1]
	),
	(
	    retract_fact(found_hook(Module, attr_portray_attrs)) ->
	    Tail1 = [ (:- multifile 'attr_rt:portray_attrs'/3), 
	              ('attr_rt:portray_attrs'(Module, V, Other):- 
		          attr_portray_attrs(V, Other)) |
                      Tail2]
	;
%	    warning_message("attr_portray_attrs/2 hook not defined", []),
	    Tail1 = Tail2
	), 
	(
	    retract_fact(found_hook(Module, attribute_goal)) ->
	    Tail2 = [ (:- multifile 'attr_rt:attribute_goal'/3), 
	              ('attr_rt:attribute_goal'(Module, V, Other):- 
		          attribute_goal(V, Other)) |
                      Tail3]
	;
%	    warning_message("attribute_goal/2 hook not defined", []),
	    Tail2 = Tail3
	), 
	Tail3 = end_of_file.
	
	
 
	
 
		     

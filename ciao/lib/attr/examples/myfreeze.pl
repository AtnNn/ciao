:- module(myfreeze, [myfreeze/2, stats/1], [attr, expander]).
:- use_module(engine(internals)).

:- meta_predicate(myfreeze(?, goal)). 

myfreeze(X, Goal) :- 
	(
	    nonvar(X), !, 
	    call(Goal)
 	;
	    get_attr(X, Fb), !,
	    term_to_meta(Tb, Fb), 
	    term_to_meta(Ta, Goal), 
	    term_to_meta((Ta, Tb), G),
	    put_attr(X, G)          % rescue conjunction
	;
	    put_attr(X, Goal)
%	    put_attr(Fresh, Goal), Fresh = X 
	).
        
attr_unify_hook(Fa, Other) :-
	(    
	    nonvar(Other) -> 
	    call(Fa) 
	;
	    get_attr(Other, Fb) ->
	    term_to_meta(Tb, Fb), 
	    term_to_meta(Ta, Fa), 
	    term_to_meta((Ta, Tb), G),
	    put_attr(Other, G)          % rescue conjunction
	;
	    put_attr(Other, Fa)         % rescue conjunction
	).

attribute_goal(X, myfreeze:myfreeze(X, G)) :- 
	get_attr(X, G).



%  :- data counter/2.

% incr(I, X) :-
%  	(
%  	    retract_fact(counter(I, X)) ->
%  	    Y is X + 1,
%  	    assertz_fact(counter(I, Y))
%  	;
%  	    assertz_fact(counter(I, 0))
%  	).
	

% :- use_module(library(aggregates)).

% stats(L):-
% 	findall(I-N, current_fact(I, N), L).

% :- meta_predicate put_attr(addmodule, ?).
% put_attr(Var, Module, Value):-
% 	(
% 	    attributes:get_attribute(Var, As) ->
% 	    As = att(_, _, Next),
% 	    internal_insert_attr(Next, As, Module, Value)
% 	;
% 	    attributes:attach_attribute(Var, att(Var, false, att(Module, Value, [])))
% 	).


% internal_insert_attr([], As, Module, Value):-!,
% 	'$setarg'(3, As, att(Module, Value, []), on).
% internal_insert_attr(att(M, _, NextNext), As, Module, Value):-
% 	(
% 	    M = Module -> 
% 	    '$setarg'(3, As, att(M, Value, NextNext), on)  % Backtrackable setarg
% 	;
% 	    Module @< M ->
% 	    '$setarg'(3, As, att(Module, Value, Next), on)  % Backtrackable setarg
% 	;
% 	    internal_insert_attr(NextNext, Next, Module, Value)
% 	).
% 
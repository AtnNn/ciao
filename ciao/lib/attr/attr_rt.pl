:- module(attr_rt, [attrvar/1,
                    put_attr/2, put_attr/3, % put_attrs/2,
	            get_attr/2, get_attr/3, % get_attrs/2,
		    del_attr/1, del_attr/2, % del_attrs/1,
		    copy_term/3, %copy_term_nat/2,
		    attrvarset/2
		    ], [assertions, dcg]).

:- doc(title, "Attributes Variables Runtime").
:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Jan Wielemaker (Interface and orginal documentation)").
:- doc(author, "Christian Holzbaur").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").

:- doc(version(1, 2011/06/02), "First Implementation").

:- doc(module, "Attributes Variables manipulation predicated").

:- use_module(engine(attributes), [attach_attribute/2, 
                                   get_attribute/2,
				   detach_attribute/1]).

:- use_module(engine(internals), ['$setarg'/4]).

% NOTE: In this module setarg/4 is used in a safe way.  Indeed, to
% avoid unsafe semantics, we have to ensure that the word modifed by
% setarg/4 is never a free variable.

:- pred attrvar(Var) # "Succeeds if Term is an attributed variable". 

attrvar(Var):-
	attributes:get_attribute(Var, att(_, _, _)).

:- pred put_attr(Var, +Module, Value) # "If @var{Var} is a variable or
attributed variable, set the value for the attribute named
@var{Module} to @var{Value}. If an attribute with this name is already
associated with @var{Var}, the old value is replaced. Backtracking
will restore the old value (i.e. an attribute is a mutable term. See
also library mutable). This predicate raises a representation error if
@var{Var} is not a variable and a type error if @var{Module} is not an
atom.".

:- meta_predicate put_attr(addmodule, ?).
put_attr(Var, user(_), Value):-!,
	put_attr(Var, user, Value).
put_attr(Var, Module, Value):-
	(
	    nonvar(Var) -> 
	    throw(error(representation_error, put_attr/3-1))
	;
	    \+ atom(Module) ->
	    throw(error(type_error, put_attr/3-2))
	;
	    attributes:get_attribute(Var, As) ->
	    % Has already an attrbuted
	    As = att(_, _, Next),
	    internal_insert_attr(Next, As, Module, Value)
	;
	    % Hos no attributes
	    attributes:attach_attribute(Var, att(Var, false, att(Module, Value, [])))
	).

internal_insert_attr([], As, Module, Value):- !,
	'$setarg'(3, As, att(Module, Value, []), on). 
internal_insert_attr(Next, As, Module, Value):-
	Next = att(M, _, NextNext),
	(
	    M = Module -> 
	    %% Note: setarg(2, Next, Value, on) is not _safe_ 
            %% since Value can be variable.
            %% Then replace the cell
	    '$setarg'(3, As, att(M, Value, NextNext), on)  % backtrackable
	;
	    Module @< M ->
            %% insert a cell
	    '$setarg'(3, As, att(Module, Value, Next), on)  % backtrackable
	;
	    internal_insert_attr(NextNext, Next, Module, Value) 
	).
	
:- pred get_attr(Var, +Module, Value) # "Request the current value for
the attribute named @var{Module}. If @var{Var} is not an attributed
variable or the named attribute is not associated to @var{Var} this
predicate fails silently. If @var{Module} is not an atom, a type error
is raised.".

:- meta_predicate get_attr(addmodule, ?).
get_attr(Var, user(_), Value):-!,
	get_attr(Var, user, Value).
get_attr(Var, Module, Value):-
	(
	    nonvar(Var) ->
	    throw(error(representation_error, attr_rt:get_attr/3-1))
	;
	    \+ atom(Module) ->
	    throw(error(type_error, get_attr/3-2))
	;
	    attributes:get_attribute(Var, att(_, _, As)),
	    internal_get_attr(As, Module, Value)
	).
	
internal_get_attr(att(M, V, Next), Module, Value):-
	(
	    M = Module ->
	    V = Value 
	;
	    internal_get_attr(Next, Module, Value)
	).


:- meta_predicate del_attr(addmodule).
:- pred del_attr(Var, +Module) # "Delete the named attribute. If
    @var{Var} loses its last attribute it is transformed back into a
    traditional Prolog variable. If @var{Module} is not an atom, a type
    error is raised. In all other cases this predicate succeeds
    regardless whether or not the named attribute is present.".

:- meta_predicate get_attr(addmodule, ?).
del_attr(Var, user(_)):-!,
	get_attr(Var, user).

del_attr(Var, Module):-
	(
	    nonvar(Var) ->
	    throw(error(representation_error, get_attr/3-1))
	;
	    \+ atom(Module) ->
	    throw(error(type_error, get_attr/3-2))
	;
	    attributes:get_attribute(Var, As),
	    As = att(_, _, Next), 
	    internal_del_attr(Next, As, Module), !
	;
	    true
	).

internal_del_attr(Next, As, Module):-
	Next = att(M, _, NextNext),
	(
	    M = Module ->
	    '$setarg'(3, As, NextNext, on)  % backtrackable 
	;
	    Module @> M, 
	    internal_del_attr(NextNext, Next, Module)
	).


:- pred get_attrs(Var, Attributes) # "Get all attributes of
@var{Var}. Attributes is a term of the form @tt{att(Module, Value,
MoreAttributes)}, where @var{MoreAttributes} is for the next attribute.".

get_attrs(X, As):-
	attributes:get_attribute(X, att(_, _, As)).

:- pred put_attrs(Var, Attributes) # "Set all attributes of
@var{Var}. See @tt{get_attrs/2} for a description of
@var{Attributes}.".

put_attrs(X, As):-
	(
	    attributes:get_attribute(X, As_) ->
	    As_ = att(_, _, _), 
	    '$setarg'(3, As_, As, on)  % Backtrackable 
	;
	    attributes:attach_attribute(X, att(X, false, As))
	).

:- pred del_attrs(Var) # "If Var is an attributed variable, delete all
its attributes. In all other cases, this predicate succeeds without
side-effects.".

del_attrs(Var):-
	(
	    attributes:get_attribute(Var, att(_, _, _)) ->
	    attributes:detach_attribute(Var)
	;
	    true
	).

%%% term_attrs/2 %%% 

:- pred attrvarset(X, Vars) # 

"@var{AttVars} is a list of all attributed variables in Term and its
attributes. I.e., @tt{attvarset/2} works recursively through
attributes. This predicate is Cycle-safe. The goal
term_attvars(Term,[]) is optimized to be an efficient test that
@var{Term} has no attributes. I.e., scanning the term is aborted after
the first attributed variable is found. ".


attrvarset(X, Vars):- 
	(
	    Vars == [] ->
	    attrvarset_(X, [], [])       % Optimized call in call of nil list
	;
	    attrvarset_(X, Vars_, []),   % Call attrvarset_/3 with a fresh 2nd arg
	    untag(Vars_),                % Untag attributed variables 
	    Vars = Vars_                 % 
	).

% Important. The calls to attrvaset_/3 should respect the following
% convention: Vars (2nd argument) should be either:
%   - a free variable 
%   - or a nil list 
attrvarset_(X, Vars, Tail) :-
        var(X), !,                           % X is var
	(
	    attributes:get_attribute(X, A),  % X is attr variable
            A = att(_, false, As) ->         % X is untagged 
	    Vars = [X|Tail0],                % Vars is not nil 
	    % tag variable
	    '$setarg'(2, A, true, true),     % then tag X (unbacktrble setarg)
	    attrvarset_(As, Tail0, Tail)     % recursive call on attributes
	;
	    Vars = Tail
	).
attrvarset_([H|T], Vars, Tail) :- !,
        attrvarset_(H, Vars,  Tail0),
        attrvarset_(T, Tail0, Tail).
attrvarset_(Term, Vars, Tail) :-
        functor(Term, _, A),
        go_inside(A, Term, Vars, Tail).

go_inside(0, _, Tail, Tail) :- !.
go_inside(N, T, Bag,  Tail) :-
        Nth is N-1,
        arg(N, T, ARG),
        attrvarset_(ARG, Bag, Tail0),
        go_inside(Nth, T, Tail0, Tail).


untag([]).
untag([H|T]):-
	attributes:get_attribute(H, A),	
	'$setarg'(2, A, true, true),        % then tag X (unbacktrble setarg)
	untag(T).

%%% copy_term/3 %%% 

:- data 'copy of'/1.

% :- pred copy_term_nat(Term, Copy) # 
	
%"@var{Copy} is a renaming ot @var{Term} such that brand new variables
%have been substituted for all variables in @var{Term} (as
%@tt{term_basic:copy_term/2}). However if any of the variables of @var{Term} have
%attributes, the copied variables will be replaced by an fresh variables.".

% copy_term_nat(Term, _Copy):-
% 	attrvarset(Term, Vs),
% 	delete_attributes(Vs), 
% 	assertz_fact('copy of'(Term)), 
% 	fail.
% copy_term_nat(_Term, Copy):-
% 	retract_fact('copy of'(Copy)).


:- pred	copy_term(Term, Copy, Gs)  # 

"Creates a regular term @var{Copy} as a copy of @var{Term} (without any
attributes), and a list @var{Gs} of goals that when executed reinstate all
attributes onto Copy. The nonterminal @tt{attribute_goal/1}, as defined
in the modules the attributes stem from, is used to convert attributes
to lists of goals.".

copy_term(Term, Copy, Gs) :-
	attrvarset(Term, Vs),
	(   
	    Vs == []
	->  
	    Gs = [],
	    assertz_fact('copy of'(Term)), 
	    retract_fact('copy of'(Copy))
        ; 
	    attvars_residuals(Vs, Gs_, []),
	    delete_attributes(Vs), 
	    assertz_fact('copy of'(Term-Gs_)), 
	    fail
	;
	    retract_fact('copy of'(Copy-Gs))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attributed variable hooks %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Unify Hooks %%%
	
:- doc(hide, 'attr_rt:unify_hook'/3). 		     
:- multifile 'attr_rt:unify_hook'/3.

:- doc(hide, '$verify_attributes_loop'/2).
:- export('$verify_attributes_loop'/2).
	
'$verify_attributes_loop'(att(Module, Value, Next), Other):-
	'attr_rt:unify_hook'(Module, Value, Other), 
	'$verify_attributes_loop'(Next, Other).
'$verify_attributes_loop'([], _).


%%% Portray Hooks %%% 

:- use_module(library(format), [format/2]).

:- doc(hide, portray_attribute/2).
:- doc(hide, 'attr_rt:portray_hook'/3).

:- multifile portray_attribute/2.
:- multifile 'attr_rt:portray_hook'/3.

portray_attribute(att(_, _, As), Var):-
	format("{", []),
	portray_attrs(As, Var),
	format("}", []).

portray_attrs([], _).
portray_attrs(att(Name, Value, Rest), Var) :-
	portray_attr(Name, Value, Var),
	(   
	    Rest == [] -> 
	    true
	;   format(", ", []),
	    portray_attrs(Rest, Var)
	).

portray_attr(Name, Value, Var) :-
	(
	    'attr_rt:portray_hook'(Name, Value, Var)
	->
	    true
	;   
	    format("~w = ...", [Name])
	).

%%% Residual Goals Hooks %%% 

:- use_module(library(dynamic), [current_predicate/2]).

:- doc(hide, 'attr_rt:attribute_goal'/3).
:- multifile 'attr_rt:attribute_goal'/3.

attvars_residuals([]) --> [].
attvars_residuals([V|Vs]) -->
	(   { get_attrs(V, As) }
	->  attvar_residuals(As, V)
	;   []
	),
	attvars_residuals(Vs).

attvar_residuals([], _) --> [].
attvar_residuals(att(Module,Value,As), V) -->
	(   
	    { nonvar(V) } ->
	    % a previous projection predicate could have instantiated
	    % this variable, for example, to avoid redundant goal
	    []
	;   
	    { dynamic:current_predicate(attribute_goal/2, Module) } ->	
	    { 'attr_rt:attribute_goal'(Module, V, Goal) }, 
	    dot_list(Goal)
	;	
	    [attr_rt:put_attr(V, Module, Value)]
	),
	attvar_residuals(As, V).

dot_list((A,B)) --> !, dot_list(A), dot_list(B).
dot_list(A)	--> [A].

delete_attributes([]).
delete_attributes([V|Vs]) :-
	del_attrs(V),
	delete_attributes(Vs).



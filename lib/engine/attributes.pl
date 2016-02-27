:- module(attributes, [
        attach_attribute/2, get_attribute/2, update_attribute/2,
        detach_attribute/1],
        [assertions]).

:- comment(title,"Attributed variables").

:- comment(author,"Christian Holzbaur").
:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Carro").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"This library implements @index{attributed variables},
which provide a mechanism for extensible unification.").

% Compiled inline -- these are hooks for the interpreter.

:- pred attach_attribute(Var,Attr) : var * nonvar
        # "Attach attribute @var{Attr} to @var{Var}.".

attach_attribute(X, Y) :- attach_attribute(X, Y).

:- pred get_attribute(Var,Attr) : var(Var) => nonvar(Attr)
        # "Unify @var{Attr} with the attribute of @var{Var}, or fail if
          @var{Var} has no attribute.".

get_attribute(X, Y) :- get_attribute(X, Y).

:- pred update_attribute(Var,Attr) : var * nonvar
        # "Change the attribute of attributed variable @var{Var} to
          @var{Attr}.".

update_attribute(X, Y) :- update_attribute(X, Y).

:- pred detach_attribute(Var) : var
        # "Take out the attribute from the  attributed variable @var{Var}.".

detach_attribute(X) :- detach_attribute(X).

:- comment(version_maintenance,dir('../../version')).



:- use_module(library('assertions/meta_props')).

:- multifile callme/2.

callme(P,X):- call(P,X), !.

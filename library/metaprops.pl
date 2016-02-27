
:- use_package(hiord).

:- use_module(library('assertions/meta_props'),[prop/2, regtype/2]).

:- multifile callme/2.

callme(P,X):- call(P,X), !.

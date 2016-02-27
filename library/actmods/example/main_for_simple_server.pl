:- syntax([]).
/* Module to be activated */
:- use_module(simple_server).
/* Method of storing addresses */
:- use_module(library('actmods/filebased_publish')).

:- use_module(library('actmods/actmod_server'), [actmodmain/0]).

:- meta_predicate exe(?,fact).
/* One fact for each exported predicate */
exe(population(X,Y), population(X,Y)).

main :- actmodmain.

:- include(library(persdb)).

%% Declare the directory associated to the key "db" where the
%% persistence sets of the persistent predicates are stored:
persistent_dir(db,'./').

%% Declare a persistent predicate:
:- persistent(bar/1, db).

%% Read a term, storing it in a new fact of the persistent predicate
%% and list all the current facts of that predicate
main:-
     read(X),
     assertz_fact(bar(X)),
     findall(Y,bar(Y),L),
     write(L).

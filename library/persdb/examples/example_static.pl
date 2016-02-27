:- include(library(persdb)).

%% Required declarations for persistent_dir/2.
:- multifile persistent_dir/2.
:- data persistent_dir/2.

%% Declare the directory associated to the key "db" where the
%% persistence sets of the persistent predicates are stored:
persistent_dir(db,'./').

%% Declare a persistent predicate:
:- persistent(bar/1, db).

%% Read a term, storing it in a new fact of the persistent predicate
%% and list all the current facts of that predicate
main:-
     read(X),
     passertz_fact(bar(X)),
     findall(Y,bar(Y),L),
     write(L).

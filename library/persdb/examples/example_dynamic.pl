:- include(library(persdb)).

%% Required declarations for persistent_dir/2.
:- multifile persistent_dir/2.
:- data persistent_dir/2.

main([X]):-
%%   Declare the directory associated to the key "db" 
     asserta_fact(persistent_dir(db,'./')),
%%   Declare the predicate bar/1 as dynamic (and data) at run-time  
     data(bar/1),
%%   Declare the predicate bar/1 as persistent at run-time  
     make_persistent(bar/1, db),
     passertz_fact(bar(X)),
     findall(Y, bar(Y), L),
     write(L).    

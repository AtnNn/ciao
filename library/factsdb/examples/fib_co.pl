:- use_package(ciaopp).

:- use_module(library('factsdb/factsdb_rt'),[asserta_fact/1,assertz_fact/1,call/1,current_fact/1,retract_fact/1]).

:- meta_predicate '$factsdb$cached_goal'(fact,?,?,?).

:- multifile '$factsdb$cached_goal'/4.

:- discontiguous '$factsdb$cached_goal'/4.

:- redefining(asserta_fact/1).

:- redefining(assertz_fact/1).

:- redefining(current_fact/1).

:- redefining(retract_fact/1).

:- multifile file_alias/2.

:- data file_alias/2.

file_alias(fib0db,persdb).

fib0(_1,_2) :-
        factsdb_rt:call(fib0(_1,_2)).

'$factsdb$cached_goal'(fib0,fib0,2,fib0db).

:- data fib0/2.

fib(M,N) :-
        fib0(M,N),
        !.

fib(M,N) :-
        M>1,
        M1 is M-1,
        M2 is M-2,
        fib(M1,N1),
        fib(M2,N2),
        N is N1+N2,
        factsdb_rt:asserta_fact(fib0(M,N)).


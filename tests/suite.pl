
%% :- module(suite, [main/0]).

:- use_module(knights_2).
:- use_module(size).
:- use_module(speed).
:- use_module(trie).
:- use_module(wumpus).
:- use_module(fib).
:- use_module(bn).
:- use_module(guardians).
:- use_module(jugs).

:- use_module(library(write)).
:- use_module(library(prolog_sys)).

main:-
        line,
        write('Executing suite of benchmarks'), nl,
        knights,
        size,
        queens,
        trie,
        wumpus,
        fib,
        bignums,
        guardians,
        jugs,
        statistics.


knights:-
        line,
        write('knights tour.  Should fail.'), nl,
        knights(4, _Board).
knights.


size:-
        line,
        write('Memory operations.  Should give info about mem. and gc'), nl,
        mem.

queens:-
        line,
        write('All solutions to the 11 queens problem'), nl,
        speed.


trie:- 
        line,
        write('Construct a trie and search some words in it'), nl,
        consulta([variable, speed, queen, var],['knights_2.pl',
        mundo, 'robot.pl', 'size.pl', 'trie.pl', 'wumpus.pl', 'speed.pl'],
        Donde),
        write(donde = Donde), nl.

wumpus:-
        line,
        write('Solve a wumpus world'), nl,
        w(mundo),
        nl.

fib:-
        line,
        write('Working out the inverse fibonacci...'), nl,
        do_fib.
        
bignums:-
        line,
        write('Playing around with big integers...'), nl,
        do_bignums.

guardians:-
        line,
        write('Solving the guardians and locks problem...'), nl,
        guardians(1000,200,Unlocked),
        write('Unlocked cells: '),
        write(Unlocked),
        write('.'), nl.

jugs:-
        line,
        write('Solving the jugs problem...'), nl,
        solve_jugs(Solution),
        write('Solution: '),
        write(Solution),
        write('.'), nl,
        line.


multi(0, _What).
multi(N, What):-
        N > 0,
        launch_goal(What),
        N1 is N - 1,
        multi(N1, What).



line:- write(
'***************************************************************************'
), nl.

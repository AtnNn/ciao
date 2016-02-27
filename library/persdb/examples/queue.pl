:- module(queue, [main/0],[]).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(aggregates)).

:- include(library(persdb)).

:- multifile persistent_dir/2.
:- data persistent_dir/2.

:- persistent(queue/1, queue_dir).

persistent_dir(queue_dir,'./pers').

main:-
     write('Action ( in(Term). | out. | list. | halt. ): '),
     read(A),
     (  handle_action(A)
     -> true
     ;  write('Unknown command.'), nl ),
     main.

handle_action(halt) :-
     halt.
handle_action(in(Term)) :-
     passertz_fact(queue(Term)),
     main.
handle_action(out) :-
     (  pretract_fact(queue(Term))
     -> write('Out '), write(Term)
     ;  write('FIFO empty.') ),
     nl,
     main.
handle_action(list) :-
     findall(Term,pcurrent_fact(queue(Term)),Terms),
     write('Contents: '), write(Terms), nl,
     main.
     


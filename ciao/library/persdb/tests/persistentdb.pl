:- module(persistentdb, [main/0], [assertions, unittestprops, fsyntax]).

:- use_module(library(format)).
:- use_module(library(system)).
:- use_module(library(terms)).

:- load_test_module(library(component_registry), []).

:- use_module(ciaodesrc(makedir('ConfigValues')), [setlocalciao/1, ciaoc/1]).
:- use_module(library(make(system_extra)), [do/2]).

:- data where_command/2 .

command_name(queue).
command_name(example_dynamic).
command_name(example_static).

ciaoc_compile(What) :-
	do([~setlocalciao, ' ', ~ciaoc, ' ', What], nofail).

initialize :-
	absolute_file_name(library(persdb), PersDB),
	atom_concat(Dir, 'persdb.pl', PersDB),
	command_name(Command),
	atom_concat([Dir, 'tests/', Command], WCommand),
	assertz_fact(where_command(Command,WCommand)),
	fail
    ;
	true.

:- test main.

main :- 
	initialize,
        format("Compiling queue~n", []),
        where_command(queue, Queue),
%        do_command(['ciaoc-1.9', ' ', Queue]),
        ciaoc_compile(Queue),
        format("Starting queue process~n", []),
        exec(Queue, Stdin,_),
        format(Stdin, "in(~a).~n", [a]),
        format(Stdin, "in(~a).~n", [b]),
        format(Stdin, "halt.~n", []),
	flush_output(Stdin),
	nl,
	format("Waiting for next persdb test~n~n", []),
	pause(1),
	ex_s_main,	
	nl,
	format("Waiting for next persdb test~n~n", []),
	pause(1),
	ex_d_main,
        format("Not doing more persdb/queue tests! Please write some more!~n",[]).

ex_s_main :-
        format("Compiling an static example of persdb~n", []),
        where_command(example_static, Static),
%        do_command(['ciaoc-1.9', ' ', Static]),
        ciaoc_compile(Static),
        format("Starting static example process~n", []),
	sub_main(Static, 5), nl.

ex_d_main :-
        format("Compiling a dynamic example of persdb~n", []),
        where_command(example_dynamic, Dynamic),
%    do_command(['ciaoc-1.9', ' ', Dynamic]),
        ciaoc_compile(Dynamic),
        format("Starting dynamic example process~n", []),
	sub_main(Dynamic, 5), nl.


sub_main(Command, N) :-
	N > 0, !,
	exec(Command, Stdin,_),
	pause(1),
	number_codes(N,CN), atom_codes(AN,CN),
        format(Stdin, "~a.~n", [AN]),
	flush_output(Stdin),
	N1 is N - 1,
	sub_main(Command, N1).
sub_main(_,_).





 %%         format("Restarting queue process~n", []),
 %%         exec(Queue, Stdin1, Stdout1),
 %%         format(Stdin1, "out.~n",[]),
 %%         get_line(Stdout1, OutAt1),
%%        format(Stdin1, "halt.~n", []).
%%        format("read first: ~s~n", [OutAt1]).
 %%         format(Stdin1, "out.~n",[]),
 %%         read(Stdout1, OutAt2),
 %%         format(Stdin1, "halt.~n",[]),
 %%         (
 %%             [OutAt1, OutAt2] = [At1, At2] ->
 %%              format("Ok with persistent predicates~n", [])
 %%         ;
 %%             format("Problems with persistent predicates: wrote ~w, read ~w~n",
 %%             [[At1, At2], [OutAt1, OutAt2]])
 %%         ).

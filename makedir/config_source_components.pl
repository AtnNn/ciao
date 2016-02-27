:- module(_, [main/1], []).

:- use_module(library(system), [working_directory/2]).
:- use_module(library(component_registry(component_scan))).

main([]) :-
	working_directory(Dir, Dir),
	component_scan(Dir).
main([Dir]) :-
	component_scan(Dir).

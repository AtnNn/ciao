:- module(_, [main/1], []).

:- use_module(library(system), [working_directory/2]).
:- use_module(library(bundle_registry(bundle_scan))).

main([]) :-
	working_directory(Dir, Dir),
	bundle_scan(Dir).
main([Dir]) :-
	bundle_scan(Dir).

:- module(_, _, []).

:- use_module(library(system)).

:- use_module(.(config_stage1)).

main([]) :-
	working_directory(CiaoDESrc, CiaoDESrc),
	config_source_components(CiaoDESrc).
main([CiaoDESrc]) :-
	config_source_components(CiaoDESrc).

:- package(pkgs_output_debug).
:- load_compilation_module(library('pkgs_output_debug/pkgs_output_debug_tr')).
% To use it when it is not a system library.
% :- load_compilation_module(.(pkgs_output_debug_tr)).
:- add_sentence_trans(pkgs_output_debug/3, 760).
% :- add_clause_trans(debugger_pkg/3).
% :- translation_predname(debugger_pkg/3).


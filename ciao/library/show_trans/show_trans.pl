:- package(show_trans).
% :- doc(bug, "This package duplicates some functionality of expander").
:- load_compilation_module(library(show_trans(show_trans_tr))).
:- add_clause_trans(show/3).

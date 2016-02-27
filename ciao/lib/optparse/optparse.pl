:- package(optparse).
:- load_compilation_module(library(optparse(optparse_tr))).
% note: priority before fsyntax (similar to menu, make, dcg, etc.)
:- add_sentence_trans(optparse_tr/3, 350).

:- discontiguous exec_simple_option/4.
:- discontiguous flag_option/3.

:- use_module(library(optparse(optparse_rt)), [parse_args/4]).

parse_args(Args) :- parse_args(Args, exec_option, default_action, usage).

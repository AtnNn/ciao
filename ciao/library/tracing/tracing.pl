:- package(tracing).

:- load_compilation_module(library(tracing(tracing_expand))).

% note: apply before 'byrdbox'
:- add_sentence_trans(expand_tracing/3, 9050).

% Defines spy/1 and nospy/1
% Adds expansion (after mine!)
:- redefining(spy/1).
:- redefining(nospy/1).
:- use_package(library(byrdbox)).

:- use_module(library(tracing(traces))).

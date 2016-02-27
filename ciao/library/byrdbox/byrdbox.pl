:- package(byrdbox).

:- load_compilation_module(library(byrdbox(byrdbox_expand))).

% note: after 'tracing' expansion
:- add_sentence_trans(expand_byrdbox/3, 9060).

:- use_module(library(byrdbox(byrd))).

:- new_declaration(nospy/1).
:- new_declaration(spy/1).

:- op(1190, fy,(nospy)).
:- op(1190, fy,(spy)).

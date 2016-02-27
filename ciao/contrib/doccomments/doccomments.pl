:- package(doccomments).
:- load_compilation_module(library(doccomments(doccomments_tr))).

% note: this package must be executed after 'condcomp' and before any
%       other expansions (otherwise it could inject fake facts in the
%       program).
:- add_sentence_trans(doccomments_sentence/3, 150).
:- add_term_trans(doccomments_term/3, 150).

% tell the reader to parse '%!' as special sentences
:- set_prolog_flag(doccomments, on).







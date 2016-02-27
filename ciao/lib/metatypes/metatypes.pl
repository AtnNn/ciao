:- package(metatypes).
:- load_compilation_module(library(metatypes(metatypes_tr))).
% note: priority like regtypes-2
:- add_sentence_trans(expand_metatypes/2, 208). % TODO: Right priority?

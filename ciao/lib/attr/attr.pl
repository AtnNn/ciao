:- package(attr).

:- use_module(library(attr(attr_rt)), [get_attr/2, put_attr/2, del_attr/2]).

:- load_compilation_module(library(attr(attr_tr))).
:- add_sentence_trans(attr_tr:sentence/3).



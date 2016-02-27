:- multifile primitive_meta_predicate/2.

:- comment(hide, primitive_meta_predicate/2).

:- load_compilation_module(engine(metatr)).
:- add_sentence_trans(prim_meta_tr/3).

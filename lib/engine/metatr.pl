:- module(metatr, [prim_meta_tr/3], []).

:- use_module(engine(term_basic), [functor/3]).

prim_meta_tr((:- primitive_meta_predicate(MP)),
             [(:- meta_predicate(MP)),
              (primitive_meta_predicate(Pat, M))], M) :-
        functor(MP, F, A),
        functor(Pat, F, A).

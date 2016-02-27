% The Ciao make package

:- include(library('make/make_ops')).

:- discontiguous do_target/1.
:- discontiguous target_exists/1.
:- discontiguous target_deps/2.

:- discontiguous target_comment/1.

:- discontiguous do_dependency/3.
:- discontiguous dependency_exists/2.

:- discontiguous dependency_precond/3.

:- discontiguous dependency_comment/3.

:- load_compilation_module(library('make/make_tr')).
:- add_sentence_trans(defdep/3).

:- use_module(library('make/make_rt'),
              [make/1,make_option/1,
	       make_toplevel/1,dyn_load_cfg_module_into_make/1]).



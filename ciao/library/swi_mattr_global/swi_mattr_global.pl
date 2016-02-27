:- package(swi_mattr_global).
:- load_compilation_module(library(swi_mattr_global(swi_mattr_global_tr))).

:- add_goal_trans( change_atts/2 ).
:- add_sentence_trans( add_atts_code/3 ).

:- use_package( mattr_global ).


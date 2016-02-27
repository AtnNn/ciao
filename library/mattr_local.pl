:- load_compilation_module( library('mattr_local/mattr_local_trans') ).
:- add_sentence_trans( lmattr_def/3 ).
:- add_term_trans( lmattr_redef/3 ).
:- op(1150, fx, [attribute] ).
:- op(1150, fx, ['$attribute_local']).

:- use_package( functions ).

:- use_module( library( 'mattr_local/mattr_local_code' ) ).


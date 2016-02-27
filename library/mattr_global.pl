:- load_compilation_module( library( 'mattr_global/mattr_global_trans' ) ).
:- add_sentence_trans( mattr_def/3).
:- add_term_trans( mattr_redef/3 ).
:- op(1150, fx, [attribute_priority]).

%% this is to comunicate with local attributes.
:- op(1150, fx, ['$attribute_local']).
:- new_declaration( '$attribute_local'/1 ).

:- use_module( library('mattr_global/mattr_global_code') ).



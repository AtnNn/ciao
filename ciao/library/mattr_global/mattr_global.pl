:- package(mattr_global).
:- load_compilation_module( library( 'mattr_global/mattr_global_trans' ) ).
:- add_sentence_trans( mattr_def/3).
:- add_goal_trans( mattr_redef/3 ).
:- op(1150, fx, [attribute_priority]).

%% this is to comunicate with local attributes.
:- op(1150, fx, ['$attribute_local']).
:- new_declaration( '$attribute_local'/1 ).

:- new_declaration( no_portray_mattr/0 ).

:- use_module(library(mattr_global(mattr_global_code))).

%:- use_module( engine( internals ) , ['$predicate_property'/3] ).


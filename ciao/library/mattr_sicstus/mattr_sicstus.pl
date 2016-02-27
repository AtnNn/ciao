:- package(mattr_sicstus).
:- load_compilation_module(library(mattr_sicstus(mattr_sicstus_trans))).
:- add_sentence_trans(lsmattr_def/3).
:- add_goal_trans(mattr_sics_redef/3).
:- op(1150, fx, [attribute]).
:- op(1150, fx, ['$attribute_local']).

:- use_module(library(mattr_sicstus(mattr_sicstus_code))).
:- use_module(library(mattr_global(mattr_global_code))).

call_list([]).
call_list([A|As]) :-
	call(A),
	call_list(As).

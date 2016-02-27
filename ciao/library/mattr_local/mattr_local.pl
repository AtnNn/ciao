:- package(mattr_local).
:- load_compilation_module(library(mattr_local(mattr_local_trans))).
:- add_sentence_trans(lmattr_def/3).
:- add_goal_trans(lmattr_redef/3).
:- op(1150, fx, [attribute]).
:- op(1150, fx, ['$attribute_local']).

:- use_package(fsyntax).

:- use_module(library(mattr_local(mattr_local_code))).

:- op(55, xfx, '@').
:- fun_eval (@) /2.

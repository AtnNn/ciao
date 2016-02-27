:- module(dynmods, [use_module/1]).

:- use_module(library(compiler),[use_module/3]).

:- meta_predicate use_module(addmodule).

:- impl_defined([use_module/1]).

use_module(Mod,This) :- use_module(Mod,all,This).

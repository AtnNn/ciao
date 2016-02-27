:- package(rescostcenter).

:- use_module(library(profiler(profiler_rt)),           []).
:- use_module(library(profiler(profiler_utils_native)), []).

:- new_declaration(check_point/1).
:- op(1150, fx, [check_point]).

:- load_compilation_module(library(resdefs(rescostcenter_tr))).
:- add_sentence_trans(cost_center_sentence_tr/3).
:- add_goal_trans(cost_center_goal_tr/3).
:- use_package(resdefs).
:- use_module(library(resdefs(rescostcenter_rt))).

% This is expanded to define all the cost center based resources.
:- resource_cost_center.

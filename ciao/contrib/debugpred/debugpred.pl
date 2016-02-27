:- package(debugpred).
% A package to define special debug predicates what can be enabled
% (executed) or disabled (replaced by 'true') based on a single
% directive.

:- load_compilation_module(library(debugpred(debugpred_tr))).

%:- use_module(library(debugpred(debugpred_rt))).

% :- add_term_trans(debugpred_term_tr/3).

:- add_sentence_trans(debugpred_sentence_tr/3, 9010).
:- add_goal_trans(debugpred_goal_tr/3, 9010).

:- use_module(library(lists)).

:- op(1150, fx, [debugpred]).

:- debugpredstatus(on).

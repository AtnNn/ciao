:- use_module(eval).
:- load_compilation_module(expand). % May be expand_real or expand_rational
:- add_goal_trans(expand/2).

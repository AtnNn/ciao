
:- load_compilation_module(library('srcbyrd/srcbyrd_tr')).
:- use_module(library('debugger/debugger_lib'),
        [debug_module/1,
         spy/1,
         get_debugger_state/1]).
 
:- add_clause_trans(srcdbg_expand/4).
:- initialization(debugger_init).

:- use_module(library('srcbyrd/srcbyrd_rt')).

debugger_init:-
        trace,
        this_module(M),
        debug_module(M).

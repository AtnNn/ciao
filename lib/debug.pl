
:- load_compilation_module(library('srcbyrd/srcbyrd_tr')).
:- use_module(library('debugger/debugger_lib'),[debug_module/1,spy/1]).

:- add_clause_trans(srcdbg_expand/4).

%:- initialization(initialize_srcdebugger).
:- initialization((this_module(M),debug_module(M))).
:- initialization(debug).

:- use_module(library('srcbyrd/srcbyrd_rt')).

:- use_package([]).

:- use_module(toplev).
:- use_module(library(libpaths), [get_alias_path/0]).
:- use_module(library(compiler),
        [make_po/1, unload/1,
         set_debug_mode/1, set_nodebug_mode/1]).
:- use_module(library('compiler/exemaker'),
        [make_actmod/2, force_lazy/1, undo_force_lazy/1,
         dynamic_search_path/1]).
:- use_module(library('compiler/c_itf'),
        [multifile/1]).
:- use_module(library(debugger),
        [trace/0, notrace/0, debug/0, nodebug/0, spy/1, nospy/1,
 	nospyall/0, debugging/0, leash/1, maxdepth/1, 
	breakpt/6,nobreakpt/6,nobreakall/0,list_breakpt/0, 
	call_in_module/2]).
:- use_module(library(operators), [op/3]).

main :-
        get_alias_path,
        this_module(ThisModule),
        asserta_fact(shell_module(ThisModule)),
        shell_start.

aborting :- shell_abort.

call_user(X) :- call(X).



:- module(ciaosh,[main/0, '$shell_abort'/0,
                  displayversion/0, version/1,
                  new_declaration/1, new_declaration/2,
                  load_compilation_module/1, add_sentence_trans/1,
                  add_term_trans/1, add_goal_trans/1,
                  % up/0 & top/0 checked explicitly
                  use_module/1, use_module/2, ensure_loaded/1,
                  make_exec/2,
                  include/1, use_package/1,
                  consult/1, compile/1, '.'/2,
                  debug_module/1, nodebug_module/1,
                  debug_module_source/1,
		  display_debugged/0,
		  top_prompt/2
		 ],
                 [dcg,assertions]).

:- ensure_loaded(ciaoshcope).
:- use_module(user, ['$shell_module'/1, '$shell_call'/1]).
:- use_module(library('compiler/exemaker'),
        [make_exec/2]).
:- use_module(library(compiler),
        [use_module/3, ensure_loaded/2,
         set_debug_mode/1, set_nodebug_mode/1, mode_of_module/2,
         set_debug_module/1, set_nodebug_module/1,
         set_debug_module_source/1]).
:- use_module(library(goal_trans), [add_goal_trans/2]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(errhandle)).
:- use_module(library(ttyout)).
:- use_module(library(write), [write/1, write_term/3]).
:- use_module(library(read), [read_top_level/3, read_term/3]).
:- use_module(library(operators), [op/3]).
:- use_module(library(sort), [sort/2, keysort/2]).
:- use_module(library(attrdump), [copy_extract_attr/3]).
:- use_module(library(debugger)).
:- use_module(library('compiler/translation'),
        [expand_term/4, add_sentence_trans/2, add_term_trans/2]).
:- use_module(library('compiler/c_itf'),
	[interpret_srcdbg/1,multifile/1, default_shell_package/1]).
:- use_module(engine(internals),
        ['$bootversion'/0, '$setarg'/4,
         '$open'/3, '$abolish'/1,'$empty_gcdef_bin'/0]).
:- use_module(engine(hiord_rt),
	[call/1, '$nodebug_call'/1]).
:- use_module(library(lists),[difference/3]).
:- use_module(library(format),[format/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(libpaths), [get_alias_path/0]).

:- redefining(make_exec/2).
:- redefining(debug_module/1).
:- redefining(debug_module_source/1).
:- redefining(nodebug_module/1).

:- multifile exit_hook/0, after_query_hook/0, after_solution_hook/0.

:- multifile define_flag/3.

define_flag(prompt_alternatives_no_bindings, [on,off], off).

:- data shell_module/1. % Module where queries are called

main :-
        get_alias_path,
        '$shell_module'(Module),
        asserta_fact(shell_module(Module)),
        '$abolish'('user:main'),
        retract_fact('$imports'(user(_),ciaosh,main,0,_)),
	current_prolog_flag(argv, Args),
	interpret_args(Args, true),
        displayversion,
        op(900,fy,[(spy),(nospy)]),
        shell_body,
	( '$nodebug_call'(exit_hook), fail ; true).

interpret_args([], Load_CiaoRC) :- !,
	( Load_CiaoRC = true -> 
            include_if_exists('~/.ciaorc')
        ;
	    true
	).
interpret_args(['-f'|R], _) :- !, % fast start
	interpret_args(R, false).
interpret_args(['--version'], _) :- !,
        '$bootversion', % Display Ciao version
         halt.
interpret_args(['-l',File|R], _) :- !,
        include_if_exists(File),
	interpret_args(R, false).
interpret_args(['-u',File|R], Load_CiaoRC) :- !,
        use_module(File),
	interpret_args(R, Load_CiaoRC).
interpret_args(['-p',Prompt|R], Load_CiaoRC) :- !,
	top_prompt(_, Prompt),
	interpret_args(R, Load_CiaoRC).
interpret_args(_WinMesh, _) :-
        get_os('Win32'), !, % For windows shortcuts
        include_if_exists('~/.ciaorc').
interpret_args(_Args, _) :-
        display('Usage: ciaosh [-f] [-l <File>] [-u <File>] [-p <Prompt>]'),
        nl,
        halt(1).

include_if_exists(File) :-
        ( file_exists(File) ->
            include(File)
        ; prolog_flag(quiet, QF, warning),
	  default_shell_package(Package),
          use_package(Package),
          prolog_flag(quiet, _, QF) 
        ).

'$shell_abort' :-
	message('{ Execution aborted }'),
	shell_body,
	( '$nodebug_call'(exit_hook), fail ; true).

shell_body :-
        intercept(error_protect(top_shell_env),
                  control_c,
                  do_interrupt_command(0'\n)).

top_shell_env :-
        reset_query_level,
        catch(shell_env(_Vars), go_top, top_shell_env).

shell_env(Vars) :-
        repeat,
        shell_query(Vars, Query),
	( '$nodebug_call'(after_query_hook), fail ; true ),
	Query == end_of_file,
	!.

:- push_prolog_flag(multi_arity_warnings,off).

:- data top_prompt/1.

:- pop_prolog_flag(multi_arity_warnings).

:- data top_prompt_base/1.

top_prompt_base('?- ').

% Actually, sets top_prompt_base, but since seen externally, used simpler name
top_prompt( OLD , NEW ) :-
	top_prompt_base( OLD ),
	retract_fact( top_prompt_base( OLD ) ),
	asserta_fact( top_prompt_base( NEW ) ).

shell_query(Variables, Query) :-
        '$empty_gcdef_bin', % Really get rid of abolished predicates
        debugger_info,
        current_fact(top_prompt(TP)),
        prompt(Prompt, TP),
	( true ; prompt(_, Prompt), fail),
	catch(get_query(Query, Variables),
              error(syntax_error(L0,L1,Msg,ErrorLoc), _),
              (Query = fail, handle_syntax_error(L0,L1,Msg,ErrorLoc))),
        prompt(_, Prompt),
        !,
        ( Query == top ->
            ttynl, throw(go_top)
        ; valid_solution(Query, Variables) ->
            ttynl, ttydisplay(yes)
        ;   ttynl, ttydisplay(no)
        ),
        ttynl.
shell_query(_Variables, end_of_file).

debugger_info :- 
        get_debugger_state(State),
	arg(1, State, T),
        ( T = off, ! 
        ; ttydisplay('{'),ttydisplay(T),ttydisplay('}\n')
        ).

get_query(Query, Dict) :-
        read_term(user, RawQuery, [dictionary(Dict),variable_names(VarNames)]),
        shell_expand(RawQuery, VarNames, Query),
        Query\==end_of_file,
        Query\== up.

handle_syntax_error(L0,L1,Msg,ErrorLoc) :-
        display(user_error, '{SYNTAX '),
        message_lns(error, L0, L1,
                    [[](Msg),'\n',[](ErrorLoc),'\n}']).

valid_solution(Query, Variables) :-
        ( adjust_debugger ; switch_off_debugger, fail),
        '$shell_call'(Query),
        ( switch_off_debugger ; adjust_debugger, fail),
        ( '$nodebug_call'(after_solution_hook), fail ; true ),
        answer_constraints(Variables, Dict, Constraints),
        solution_vars(Dict, Eqs, []),
        prettyvars([Eqs|Constraints]),
        display_solution(Eqs, Constraints, Variables).

display_solution(Eqs, Constraints, Variables) :-
        display_bindings(Eqs, '', Sep0),
        display_constraints(Constraints, Sep0, Sep),
        ok_solution(Sep, Eqs, Constraints, Variables).

:- multifile dump/3. /* For clp[qr] .DCG. */

answer_constraints(Variables, Dict, Constraints) :-
        dump(Variables, Dict, Constraints), !.
answer_constraints(Variables, Dict, Constraints) :-
        copy_extract_attr(Variables, Dict, Constraints).

solution_vars(D) --> {var(D)}, !.
solution_vars(dic(Var,[Val|_],L,R)) -->
 	solution_vars(L),
        solution_var(Var, Val),
	solution_vars(R).

solution_var([0'_|_], _) --> !.  % Do not display vars starting with "_"
solution_var(Var, Val) --> {var(Val)}, !,
        {atom_codes(AtomVar, Var), Val='$VAR'(AtomVar)}.
solution_var(Var, Val) -->
        [Var = Val].

display_bindings([], Sep, Sep).
display_bindings([Var=Val|Eqs], Sep0, Sep) :-
        ttydisplay(Sep0), ttynl,
        display_string(Var),
        ttydisplay(' = '),
        write_term(user, Val, [quoted(true), portrayed(true),
                               numbervars(true), priority(699)]),
        display_bindings(Eqs, ',', Sep).


display_constraints([], Sep, Sep).
display_constraints([Goal|Gs], Sep0, Sep) :-
        display_goal(Goal, Sep0, Sep1),
        display_constraints(Gs, Sep1, Sep).

display_goal(true, Sep, Sep) :- !.
display_goal((G1,G2), Sep0, Sep) :- !,
	display_goal(G1, Sep0, Sep1),
 	display_goal(G2, Sep1, Sep).
display_goal(G, Sep, ',') :-
	ttydisplay(Sep), ttynl,
        write_term(user, G, [quoted(true), portrayed(true),
                             numbervars(true)]).

ok_solution('', _, _, _) :-
        current_prolog_flag(prompt_alternatives_no_bindings, off), !.
ok_solution(Sep, Eqs, Constraints, Variables) :-
        ( Sep = '' -> ttydisplay('OK') ; true ),
        ttyput(0' ), ttyput(0'?), ttyput(0' ),
        ttyflush,
        ttyget(C),
        ( C = 10                            % end of line
        ; C = 0'y, ttyskip(10)              % y(es)
        ; C = 0', ->                        % add another question
            ttyskip(10),
            ttynl,
            inc_query_level,
            shell_env(Variables),
            dec_query_level,
            display_solution(Eqs, Constraints, Variables)
        ; ttyskip(10), fail                 % another solution
        ).


% This is alike the one in library(write), except that variable names
% start with "_"

prettyvars(Term) :-
	collect_vars(Term, Vars0, []),
	keysort(Vars0, Vars),
	pretty_vars(Vars, 0).

collect_vars(Var) -->
	{var(Var)}, !, [Var-[]].
collect_vars([X|Xs]) --> !,
	collect_vars(X),
	collect_vars(Xs).
collect_vars(X) -->
	{functor(X, _, A)},
	collect_vars_(0, A, X).

collect_vars_(A, A, _) --> !.
collect_vars_(A0, A, X) -->
	{A1 is A0+1},
	{arg(A1, X, X1)},
	collect_vars(X1),
	collect_vars_(A1, A, X).

pretty_vars([], _).
pretty_vars([X,Y|Xs], N0) :-
	X==Y, !,
        name_var(X, N0),
	N is N0+1,
	pretty_vars_(Xs, X, N).
pretty_vars(['$VAR'('_')-[]|Xs], N0) :-
	pretty_vars(Xs, N0).

pretty_vars_([X|Xs], Y, N0) :-
	X==Y, !,
	pretty_vars_(Xs, Y, N0).
pretty_vars_(Xs, _, N0) :-
	pretty_vars(Xs, N0).

name_var('$VAR'(Name)-[], N) :-
        Letter is N mod 26 + 0'A,
        ( N>=26 ->
            Rest is N//26,
            number_codes(Rest, Index)
        ; Index = ""
        ),
        atom_codes(Name, [0'_ , Letter | Index]).

:- data querylevel/1.

reset_query_level :-
        retractall_fact(querylevel(_)),
        asserta_fact(querylevel(0)),
        set_top_prompt(0).

inc_query_level :-
        retract_fact(querylevel(N)),
        N1 is N+1,
        asserta_fact(querylevel(N1)),
        set_top_prompt(N1).

dec_query_level :-
        retract_fact(querylevel(N)),
        N1 is N-1,
        asserta_fact(querylevel(N1)),
        set_top_prompt(N1).

set_top_prompt(0) :- !,
        retractall_fact(top_prompt(_)),
	top_prompt_base(P),
        asserta_fact(top_prompt(P)).

set_top_prompt(N) :-
        number_codes(N, NS),
        atom_codes(NA, NS),
	top_prompt_base(P),
        atom_concat(NA, ' ', NS1),
        atom_concat(NS1, P, TP),
        retractall_fact(top_prompt(_)),
        asserta_fact(top_prompt(TP)).

:- data '$current version'/1.

displayversion :-              % shall use current output
	(   '$bootversion', 
	    current_fact('$current version'(Msg)),
	    nl, write(Msg), nl,
	    fail
	;   true
	).

version(A) :-
        nonvar(A), !,
	assertz_fact('$current version'(A)).
version(_) :- throw(error(instantiation_error,version/1-1)).

shell_expand(V, _, Query) :- var(V), !, Query = call(V).
shell_expand((:- Decl), VarNames, Query) :-
        current_fact(shell_module(ShMod)),
        expand_term((:- Decl), ShMod, VarNames, Query),
        ( Query = true -> true ; true). % unify Query if a var
shell_expand(RawQuery, VarNames, Query) :-
        current_fact(shell_module(ShMod)),
        expand_term(('SHELL':-RawQuery), ShMod, VarNames, Expansion),
        ( Expansion = ('SHELL':-Query), !
        ; Query = fail,
          message(error, ['unexpected answer from expansion: ',Expansion])
        ).


/* Including files in shell */

:- data new_decl/1.

include(File) :-
        absolute_file_name(File, '_opt', '.pl', '.', SourceFile, _, _),
        message(['{Including ',SourceFile]),
        '$open'(SourceFile, r, Stream),
        include_st(Stream),
        close(Stream),
        message('}').

include_st(Stream) :-
        current_fact(shell_module(ShMod)),
        repeat,
	  read_term(Stream, RawData, [variable_names(VarNames),lines(L0,L1)]),
          expand_term(RawData, ShMod, VarNames, Data0),
	  nonvar(Data0),
	  ( Data0 = [_|_] ->
	      member(Data1, Data0)
	  ; Data1 = Data0
	  ),
        ( Data1 = end_of_file, !
        ; interpret_data(Data1, L0, L1),
          fail).

interpret_data((?- Goal), _, _) :- !,
        '$shell_call'(Goal), !.
interpret_data((:- Decl), L0, L1) :- !,
        ( current_fact(new_decl(Decl)) ->
            true
        ; shell_directive(Decl) ->
            call(Decl)
        ; bad_shell_directive(Decl, L0, L1)
        ).
interpret_data(Clause, _, _) :-
        '$shell_call'(assertz(Clause)).

bad_shell_directive(Decl, L0, L1) :-
        functor(Decl,F,A),
        message_lns(error, L0, L1,[~~(F/A),' directive not allowed in shell']).

shell_directive(use_module(_)).
shell_directive(use_module(_,_)).
shell_directive(ensure_loaded(_)).
shell_directive(include(_)).
shell_directive(use_package(_)).
shell_directive(set_prolog_flag(_,_)).
shell_directive(push_prolog_flag(_,_)).
shell_directive(pop_prolog_flag(_)).
shell_directive(op(_,_,_)).
shell_directive(new_declaration(_,_)).
shell_directive(new_declaration(_)).
shell_directive(load_compilation_module(_)).
shell_directive(add_sentence_trans(_)).
shell_directive(add_term_trans(_)).
shell_directive(add_goal_trans(_)).
shell_directive(multifile(_)).

use_module(M) :-
        use_module(M, all).

use_module(M, Imports) :-
        shell_module(Module),
        use_module(M, Imports, Module).

ensure_loaded([]) :- !.
ensure_loaded([File|Files]) :- !,
        shell_module(Module), % JF[] added module
        compiler:ensure_loaded(File, Module), % JF[] added module
        ensure_loaded(Files).
ensure_loaded(File) :-
        shell_module(Module), % JF[] added module
        compiler:ensure_loaded(File, Module). % JF[] added module

[File|Files] :-
        ( Files = [] -> AllFiles = File ; AllFiles = [File|Files] ),
	%% JF[] removed obsolete message
        %message(note,[[File|Files],' is obsolete, use ',
        %             ensure_loaded(AllFiles),' instead']),
        ensure_loaded([File|Files]).

consult([]) :- !.
consult([File|Files]) :- !,
        consult(File),
        consult(Files).
consult(File) :-
        set_debug_mode(File),
        ensure_loaded(File).

compile([]) :- !.
compile([File|Files]) :- !,
        compile(File),
        compile(Files).
compile(File) :-
        set_nodebug_mode(File),
        ensure_loaded(File).

make_exec(Files, ExecName) :-
        ( Files = [_|_] ->
            exemaker:make_exec(Files, ExecName)
        ; exemaker:make_exec([Files], ExecName)
        ).

use_package([]) :- !.
use_package([F|Fs]) :- !,
        use_package(F),
        use_package(Fs).
use_package(F) :- atom(F), !,
        include(library(F)).
use_package(F) :- functor(F,_,1), !,
        include(F).
use_package(F) :-
        message(error, ['Bad package file ',~~(F)]).

new_declaration(S, _) :- new_declaration(S).

new_declaration(S) :-
        ( S = F/A, functor(D, F, A) ->
          ( current_fact(new_decl(D)) -> true
          ; asserta_fact(new_decl(D))
          )
        ; message(error, ['Bad predicate specifier ',S,
                          'in new_declaration directive'])
        ).

load_compilation_module(File) :-
        this_module(M),
        use_module(File, all, M),   % Here for sentence/term expansions
        shell_module(ShM),
        use_module(File, all, ShM). % In ciaoshcope for goal expansions

add_sentence_trans(P) :-
        current_fact(shell_module(ShMod)),
        add_sentence_trans(ShMod, P), !.
add_sentence_trans(P) :-
        message(warning, [add_sentence_trans(P),' - declaration failed']).

add_term_trans(P) :-
        current_fact(shell_module(ShMod)),
        add_term_trans(ShMod, P), !.
add_term_trans(P) :-
        message(warning, [add_term_trans(P),' - declaration failed']).

add_goal_trans(P) :-
        current_fact(shell_module(ShMod)),
        add_goal_trans(ShMod, P), !.
add_goal_trans(P) :-
        message(warning, [add_goal_trans(P),' - declaration failed']).

debug_module(M):-
        set_debug_module(M),
        debugger:debug_module(M),
        ( mode_of_module(M, Mode), Mode \== interpreted(raw) ->
            message(['{Consider reloading module ',M,'}'])
        ; true
        ),
	display_debugged.

nodebug_module(M):-
        set_nodebug_module(M),
        debugger:nodebug_module(M),
	display_debugged.

debug_module_source(M):-
        set_debug_module_source(M),
        debugger:debug_module_source(M),
        ( mode_of_module(M, Mode), Mode \== interpreted(srcdbg) ->
            message(['{Consider reloading module ',M,'}'])
        ; true
        ),
	display_debugged.

display_debugged :-
        current_debugged(Ms),
 	current_source_debugged(Ss),
	difference(Ms,Ss,M),
        ( M = [] ->
          format(user, '{No module is selected for debugging}~n',[])
        ; format(user, '{Modules selected for debugging: ~w}~n',[M])
        ),
        ( Ss = [] ->
	  format(user, '{No module is selected for source debugging}~n',[])
	; format(user, '{Modules selected for source debugging: ~w}~n',[Ss])
	).

current_source_debugged(Ss) :- 
	findall(S, current_fact(interpret_srcdbg(S)), Ss).

%----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*11+160,2004/01/09,18:06*15+'CET'), "Fixed
   problems related to argument handling.  Changed package loaded by
   default to match the one defined in the compiler. (Daniel Cabeza &
   Jose Morales)").

:- comment(version(1*9+325,2004/03/15,17:44*11+'CET'), "Added
   prompt_alternatives_no_bindings prolog_flag so that, even when the
   query has no variables to report bindings for, backtracking can be
   asked.  (Daniel Cabeza Gras)").

:- comment(version(1*9+97,2003/08/05,19:43*37+'CEST'), "Mode prompt
   modification more standard (prompt/2).  (Manuel Hermenegildo)").

:- comment(version(1*9+96,2003/08/05,18:25*28+'CEST'), "Predicate
   set_base_prompt added. Now it is posible to change the prompt of
   the ciao shell. Several arguments are accepted when
   executing. (David Trallero Mena)").

:- comment(version(1*7+110,2001/06/20,18:40*04+'CEST'), "Added -u option
   mainly to allow loading a module in a new shell with a click in
   Windows. (Daniel Cabeza Gras)").

:- comment(version(1*7+63,2001/03/03,01:02*08+'CET'), "Fixed a bug which
    obliged to qualify calls to main/0 defined in modules.  Now main/0
    is not defined at all at startup.  This involved a redesigning of
    the program.  Also, made the program exit after the --version switch
    is interpreted. (Daniel Cabeza Gras)").

:- comment(version(1*7+10,2000/08/16,12:10*56+'CEST'), "Added
    '--version' switch.  (MCL)").


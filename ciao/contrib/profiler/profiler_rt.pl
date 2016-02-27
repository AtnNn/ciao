:- module(profiler_rt, [
		cc_call/5,
		cc_call_nf/5,
		cc_call_ncnf/4,
		cc_exit/3,
		cc_exit_nc/2,
		cc_exit_ncnf/1,
		cc_redo/4,
		cc_redo_nf/3,
		cc_fail/2,
		cc_fail_nc/1,
		profile_init/0,
		get_profile_active/1,
		profile_module_init/1
	    ], [assertions, nativeprops, foreign_interface]).

:- doc(author, "Edison Mera").

:- doc(module, "Ancillary predicates of the profiler used in the
	code instrumentation.").

:- use_package(library(profiler(profiler_decl))).

:- initialization(profile_init).

:- use_module(library(hashtable), []).
:- use_module(library(hrtime),    []).
:- use_foreign_source(library(profiler)).

:- use_foreign_library('Win32i86', [hashtable_Win32i86]).
:- use_foreign_library('Win32i86', [hrtime_Win32i86]).

:- extra_compiler_opts('-I../hashtable').
:- extra_compiler_opts('-I../hrtime').
:- extra_linker_opts(' -L../hashtable').
:- extra_linker_opts(' -L../hrtime').

:- foreign_inline("
#include <stdio.h>
#include \"ciao_prolog.h\"
#include \"datadefs.h\"
#include \"support.h\"
#include \"predtyp.h\"
#include \"profile_defs.h\"
#include \"timing_defs.h\"
#include \"profiler.h\"
").

:- true pred get_profile_active(go(Active)) :: int
	+ (foreign_low(prolog_get_profile_active)) #

"Unifies Active with 1 if the profiler is turned on, or with 0 otherwise" -->

"
bool_t prolog_get_profile_active(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg,profile),X(0));
}
".

profile_module_init(M) :-
	'$cc$'(M, F, N),
	atom_concat(M,  ':', M0),
	atom_concat(M0, F,   P),
	add_node_cc(P, N),
	fail.
profile_module_init(_).

:- true pred add_node_cc(in(Name), in(Arity)) :: atm * int
	+ (foreign_low(prolog_add_node_cc), not_fails) -->
"
bool_t prolog_add_node_cc(Arg)
	Argdecl;
{
  definition_t *f;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  add_node_cc(active_frame[1].node_table_cc, f);
  return TRUE;
}
".

:- true pred profile_init + (foreign_low(prolog_profile_init)) -->

"
bool_t prolog_profile_init(Arg)
	Argdecl;
{
  profile_init();
  return TRUE;
}
".

cc_fail(_, ChPt) :-
	cc_fail_1(ChPt).
cc_fail(PrevECC, _) :-
	cc_fail_2(PrevECC).

cc_redo(_, ChPt0, ChPt1, CutTo) :-
	cc_redo_1(ChPt0, ChPt1, CutTo).
cc_redo(ActiveCC, _, _, _) :-
	cc_redo_2(ActiveCC).

cc_redo_nf(_, ChPt1, CutTo) :-
	cc_redo_1_nf(ChPt1, CutTo).
cc_redo_nf(ActiveCC, _, _) :-
	cc_redo_2(ActiveCC).

% (2)
:- true pred cc_call(in(Name), in(Arity), in(Hooks), go(PrevECC), go(CutTo))
	:: atm * int * int * int * int + ( foreign_low(prolog_cc_call),
	    not_fails ) -->
"
bool_t prolog_cc_call_ncnf(Argdecl);

bool_t prolog_cc_call(Arg)
    Argdecl;
{
  Unify_constant(ChoiceToInt(w->node),X(4));
  return prolog_cc_call_ncnf(Arg);
}
".

:- true pred cc_redo_1(in(ChPt0), in(ChPt1), in(CutTo)) :: int * int * int
	+ (foreign_low(prolog_cc_redo_1), not_fails) -->
"
bool_t prolog_cc_redo_1(Arg)
    Argdecl;
{
  if (profile) {
    PROFILE__TIME_INI;
    node_t *n1, *n2;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    n1=ChoiceFromInt(X(0));
    n2=ChoiceFromInt(X(1));
    if (n1==n2) {
      DEREF(X(2),X(2));
      w->node=ChoiceFromInt(X(2));
      SetShadowregs(w->node);
    }
    PROFILE__TIME_END;
  } else {
    w->node=w->next_node; /* DOCUT */
  }
  return TRUE;
}
".

:- true pred cc_redo_1_nf(in(ChPt1), in(CutTo)) :: int * int
	+ (foreign_low(prolog_cc_redo_1_nf), not_fails) -->
"
bool_t prolog_cc_redo_1_nf(Arg)
    Argdecl;
{
  if (profile) {
    PROFILE__TIME_INI;
    node_t *n1, *n2;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    n1=ChoiceFromInt(X(1));
    n2=ChoiceFromInt(X(0));
    if (n1==n2) {
      w->node=n1;
      SetShadowregs(w->node);
    }
    PROFILE__TIME_END;
  } else {
    w->node=w->next_node; /* DOCUT */
  }
  return TRUE;
}
".

:- true pred cc_redo_2(in(ActiveCC)) :: int
	+ (foreign_low(prolog_cc_redo_2), fails) -->
"
bool_t prolog_cc_redo_2(Arg)
    Argdecl;
{
  if (profile) {
    ENG_LINT d;
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->entry_time-=d;
    active_ecc=(edge_cc_t *)TermToPointer(X(0));
#if defined(PROFILE)
    if (profile_hooks) {
      if (active_ecc->hooks)
        ENABLE_HOOKS(active_ecc->functor[0])
      else
        DISABLE_HOOKS
    }
#endif
    active_ecc->entry_port=ENTER_REDO;
    active_ecc->entry_time+=d;
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-redo\",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
  }
  return FALSE;
}
".

:- true pred cc_exit(in(PrevECC), go(ActiveCC), go(ChPt))
	:: int * int * int
	+ (foreign_low(prolog_cc_exit), not_fails) -->
"
void prolog_cc_exit_common(Arg)
    Argdecl;
{
  ENG_LINT d;
  DEREF(X(0),X(0));
  ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-exit\",0,0,
    active_ecc->functor[0],active_ecc->functor[1]);
#if defined(PROFILE)
  if (profile_hooks && active_ecc->hooks) {
    d=tick_last_addition-tick_profiling;
    tick_profiling+=tick_ini_profiling-tick_last_addition;
  } else
#endif
    d=tick_ini_profiling-tick_profiling;
  active_ecc->counts[active_ecc->entry_port][LEAVE_EXIT]++;
  active_ecc->times[active_ecc->entry_port][LEAVE_EXIT]+=d-active_ecc->entry_time;
  active_ecc->entry_time=0;
#if defined(PROFILE)
  active_ecc->cuts[active_ecc->entry_port][LEAVE_EXIT]+=active_ecc->entry_cuts;
  active_ecc->scuts[active_ecc->entry_port][LEAVE_EXIT]+=active_ecc->entry_scuts;
  active_ecc->entry_cuts=0;
  active_ecc->entry_scuts=0;
#endif
  active_ecc=(edge_cc_t *)TermToPointer(X(0));
#if defined(PROFILE)
  if (profile_hooks) {
    if (active_ecc->hooks)
      ENABLE_HOOKS(active_ecc->functor[0])
    else
      DISABLE_HOOKS
  }
#endif
  active_ecc->entry_time+=d;
  tick_last_addition=tick_ini_profiling;
}

bool_t prolog_cc_exit(Arg)
    Argdecl;
{
  Unify_constant(ChoiceToInt(w->node),X(2));
  if (profile) {
    bool_t r;
    CIAO_REGISTER edge_cc_t *cc;
    PROFILE__TIME_INI;
    cc=active_ecc;
    prolog_cc_exit_common(Arg);
    r=cunify(Arg,PointerToTerm(cc),X(1));
    PROFILE__TIME_END;
    return r;
  }
  else
    return TRUE;
}
".

:- true pred cc_fail_1(go(ChPt)) :: int
	+ (foreign_low(prolog_cc_fail_1), not_fails) -->
"
bool_t prolog_cc_fail_1(Arg)
    Argdecl;
{
  if (profile) {
    Unify_constant(ChoiceToInt(w->node),X(0));
  } else {
    w->node=w->next_node; /* DOCUT */
  }
  return TRUE;
}
".

:- true pred cc_fail_2(in(PrevECC)) :: int
	+ (foreign_low(prolog_cc_fail_2), fails) -->
"
bool_t prolog_cc_fail_2(Arg)
    Argdecl;
{
  if (profile) {
    ENG_LINT d;
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-fail\",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->counts[active_ecc->entry_port][LEAVE_FAIL]++;
    active_ecc->times[active_ecc->entry_port][LEAVE_FAIL]+=d-active_ecc->entry_time;
    active_ecc->entry_time=0;
#if defined(PROFILE)
    active_ecc->cuts[active_ecc->entry_port][LEAVE_FAIL]+=active_ecc->entry_cuts;
    active_ecc->scuts[active_ecc->entry_port][LEAVE_FAIL]+=active_ecc->entry_scuts;
    active_ecc->entry_cuts=0;
    active_ecc->entry_scuts=0;
#endif
    active_ecc=(edge_cc_t *)TermToPointer(X(0));
#if defined(PROFILE)
    if (profile_hooks) {
      if (active_ecc->hooks)
        ENABLE_HOOKS(active_ecc->functor[0])
      else
        DISABLE_HOOKS
    }
#endif
    active_ecc->entry_time+=d;
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
  }
  return FALSE;
}
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Specialized versions of hooks used when some static properties has      %%
%%  been inferred (like non failure or determinism).                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cc_fail_nc(_).
cc_fail_nc(PrevECC) :-
	cc_fail_2(PrevECC).

:- true pred cc_exit_nc(in(PrevECC), in(CutTo)) :: int * int
	+ (foreign_low(prolog_cc_exit_nc), not_fails) -->
"
bool_t prolog_cc_exit_nc(Arg)
    Argdecl;
{
  if (profile) {
    PROFILE__TIME_INI;
    prolog_cc_exit_common(Arg);
    DEREF(X(1),X(1));
    w->node=ChoiceFromInt(X(1));
    SetShadowregs(w->node);
    PROFILE__TIME_END;
  }
  return TRUE;
}
".

:- true pred cc_call_ncnf(in(Name), in(Arity), in(Hooks), go(PrevECC))
	:: atm * int * int * int
	+ (foreign_low(prolog_cc_call_ncnf), not_fails) -->
"
bool_t prolog_cc_call_ncnf(Arg)
    Argdecl;
{
  if (profile) {
    bool_t r;
    ENG_LINT d;
    definition_t *f[2];
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    f[0]=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
    f[1]=active_ecc->functor[0];
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->entry_time-=d;
    r=cunify(Arg,PointerToTerm(active_ecc),X(3));
    active_ecc=get_edge_cc(active_frame->edge_table_cc, f);
#if defined(PROFILE)
    DEREF(X(2),X(2));
    active_ecc->hooks=GetInteger(X(2));
    if (profile_hooks) {
      if (active_ecc->hooks) {
        ENABLE_HOOKS(f[0]);
      }
      else {
	DISABLE_HOOKS;
      }
    }
#endif
    active_ecc->entry_port=ENTER_CALL;
    active_ecc->entry_time+=d;
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-call\",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
    return r;
  }
  else
    return TRUE;
}
".

:- true pred cc_exit_ncnf(in(PrevECC)) :: int
	+ (foreign_low(prolog_cc_exit_ncnf), not_fails) -->
"
bool_t prolog_cc_exit_ncnf(Arg)
    Argdecl;
{
  if (profile) {
    PROFILE__TIME_INI;
    prolog_cc_exit_common(Arg);
    PROFILE__TIME_END;
  }
  return TRUE;
}
".

:- true pred cc_call_nf(in(Name), in(Arity), in(Hooks), go(PrevECC),
	    go(CutTo)) :: atm * int * int * int * int
	+ (foreign_low(prolog_cc_call_nf), not_fails) -->
"
bool_t prolog_cc_call_nf(Arg)
    Argdecl;
{
  Unify_constant(ChoiceToInt(w->node),X(4));
  return prolog_cc_call_ncnf(Arg);
}
".

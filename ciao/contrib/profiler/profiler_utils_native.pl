:- module(profiler_utils_native, [
		profile_dump/0,
		using_timestamp/1,

		get_trace_active/1,
		set_trace_active/1,
		get_hooks_active/1,
		set_hooks_active/1,

		do_profile_reset/0,

		cost_center_edge_value/4,
		cost_center_node_value/3,
		cost_center_global_value/2
	    ], [assertions, nativeprops, foreign_interface]).

:- doc(author, "Edison Mera").
:- doc(title, "Profiler Utils").

:- doc(module, "This inteface emulates that of the SWI-Prolog profiler.").

:- use_module(library(profiler(profiler_rt)), []).

:- use_foreign_library('Win32i86', [hashtable_Win32i86]).
:- use_foreign_library('Win32i86', [hrtime_Win32i86]).
:- use_foreign_library('Win32i86', [profiler_rt_Win32i86]).
:- use_foreign_library('Win32i86', [profiler_utils_base_Win32i86]).

:- extra_compiler_opts('-I../hashtable').
:- extra_compiler_opts('-I../hrtime').

:- extra_linker_opts(' -L../hashtable').
:- extra_linker_opts(' -L../hrtime').
:- extra_linker_opts(' -L.').

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

:- doc(bug, "profile_dump/0 must be implemented in Prolog").

:- doc(bug, "Predicate profile_info/1 is implemented using
	temporary files instead of pipe/2 to avoid a bug that hangs
	this predicate. --EMM").

:- export(dump_node_table_cc/0).

:- true pred dump_node_table_cc + (foreign_low(prolog_dump_node_table_cc))
	--> "
bool_t prolog_dump_node_table_cc(Arg)
	Argdecl;
{
  if (ht_first(active_frame[1].node_table_cc)) do {
      definition_t *r;
      char buffer[STATICMAXATOM+MAXSTRNUMBERSIZE+1];
      r=*(definition_t **)ht_stuff(active_frame[1].node_table_cc);
      fprintf(Output_Stream_Ptr->streamfile, \"%s\\n\", functor_name(buffer, r));
    } while (ht_next(active_frame[1].node_table_cc));
  return TRUE;
}
".

:- export(have_overhead/2).

:- true pred have_overhead(in(Name), in(Arity)) :: atm * int +
	(foreign_low(prolog_have_overhead)) -->
"
bool_t prolog_have_overhead(Arg)
	Argdecl;
{
  bool_t result;
  definition_t *f;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  result=functor_have_overhead(active_frame+1,f);
  return result;
}
".

:- true pred profile_dump + (foreign_low(prolog_profile_dump)) # "Show the
   information collected by the profiler." -->

"
bool_t prolog_profile_dump(Arg)
     Argdecl;
{
  profile_dump(active_frame+1, Output_Stream_Ptr->streamfile);
  return TRUE;
}

".

:- true pred do_profile_reset + (foreign_low(prolog_profile_reset)) #
"Restart the profiler.  This option erases previously collected
   information." -->

"
bool_t prolog_profile_reset(Arg)
     Argdecl;
{
  profile_reset(active_frame+1);
  return TRUE;
}
".

:- true pred get_trace_active(go(Active)) :: int +
	(foreign_low(prolog_get_trace_active)) #

	"Return 1 if the trace is active, or 0 otherwise." -->

"
bool_t prolog_get_trace_active(Arg)
     Argdecl;
{
#if defined(PROFILE__TRACER)
  return cunify(Arg,MakeSmall(profile_trace),X(0));
#else
  return cunify(Arg,MakeSmall(0),X(0));
#endif
}
".

:- true pred set_trace_active(in(Active)) :: int +
	(foreign_low(prolog_set_trace_active)) #

	"If @var{Active} is 0, turn off the trace, otherwise it is turned on." 
--> 

"
bool_t prolog_set_trace_active(Arg)
     Argdecl;
{
#if defined(PROFILE__TRACER)
  ERR__FUNCTOR(\"set_trace_active\", 1);
  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  profile_trace=GetInteger(X(0));
  return TRUE;
#else
  return FALSE;
#endif
}
".


:- true pred get_hooks_active(go(Active)) :: int +
	(foreign_low(prolog_get_hooks_active)) #

	"Return 1 if the hooks are active, or 0 otherwise." -->

"
bool_t prolog_get_hooks_active(Arg)
     Argdecl;
{
#if defined(PROFILE)
  return cunify(Arg,MakeSmall(profile_hooks),X(0));
#else
  return cunify(Arg,MakeSmall(0),X(0));
#endif
}
".

:- true pred set_hooks_active(in(Active)) :: int +
	(foreign_low(prolog_set_hooks_active)) #

	"If Active is 0, turn off the hooks, otherwise turn them on." -->

"
bool_t prolog_set_hooks_active(Arg)
     Argdecl;
{
#if defined(PROFILE)
  ERR__FUNCTOR(\"set_hooks_active\", 1);
  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  profile_hooks=GetInteger(X(0));
  return TRUE;
#else
  return FALSE;
#endif
}
".

:- true pred using_timestamp(go(UsingTimeStamp)) :: int +
	(foreign_low(prolog_using_timestamp)) #

"Unifies @var{UsingTimeStamp} with 1 if the profile is using timestamp
to measure the time, otherwise, unifies it with 0." -->

"
bool_t prolog_using_timestamp(Arg)
    Argdecl;
{
  int using_timestamp;
#if defined(USE_TIMESTAMP)
  using_timestamp=1;
#else
  using_timestamp=0;
#endif
  return cunify(Arg,MakeSmall(using_timestamp),X(0));
}
".

:- export(total_time/1).
:- true pred total_time(go(TotalTime)) :: num +
	(foreign(total_time), returns(TotalTime)) -->

"
double total_time()
{
  return (double)(tick_last_addition - tick_start);
}
".

cost_center_edge_value(Name0/Arity0, Name/Arity, res(Res, Type), Counts) :-
	cost_center_edge_value_1(Type, Res, Name0, Arity0, Name, Arity,
	    Counts).

cost_center_edge_value_1(call_exit, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 0, Value).
cost_center_edge_value_1(call_fail, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 1, Value).
cost_center_edge_value_1(redo_exit, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 0, Value).
cost_center_edge_value_1(redo_fail, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 1, Value).
cost_center_edge_value_1(call, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 0, V0),
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 1, V1),
	Value is V0 + V1.
cost_center_edge_value_1(redo, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 0, V0),
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 1, V1),
	Value is V0 + V1.

cost_center_edge_value_2(counts, Name0, Arity0, Name, Arity, Enter, Leave,
	    Counts) :-
	cost_center_edge_counts(Name0, Arity0, Name, Arity, Enter, Leave,
	    Counts).
cost_center_edge_value_2(ticks, Name0, Arity0, Name, Arity, Enter, Leave,
	    Ticks) :-
	cost_center_edge_ticks(Name0, Arity0, Name, Arity, Enter, Leave,
	    Ticks).

:- true pred cost_center_edge_counts(Name0, Arity0, Name, Arity, Enter,
	    Leave, Counts) :: atm * int * atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_edge_counts)) -->

"
bool_t prolog_cost_center_edge_counts(Arg)
    Argdecl;
{
  edge_cc_t *ecc;
  definition_t *f[2];
  int enter, leave;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));

  f[0]=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  f[1]=GET_DEFINITION(GetString(X(2)), GetInteger(X(3)));
  ecc=get_edge_cc(active_frame->edge_table_cc, f);

  enter=GetInteger(X(4));
  leave=GetInteger(X(5));
  return cunify(Arg,MakeSmall(ecc->counts[enter][leave]),X(6));
}
".

:- true pred cost_center_edge_ticks(Name0, Arity0, Name, Arity, Enter,
	    Leave, Ticks) :: atm * int * atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_edge_ticks)) -->

"
bool_t prolog_cost_center_edge_ticks(Arg)
    Argdecl;
{
  edge_cc_t *ecc;
  definition_t *f[2];
  int enter, leave;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));

  f[0]=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  f[1]=GET_DEFINITION(GetString(X(2)), GetInteger(X(3)));
  ecc=get_edge_cc(active_frame->edge_table_cc, f);

  enter=GetInteger(X(4));
  leave=GetInteger(X(5));
  return cunify(Arg,MakeFloat(Arg,ecc->times[enter][leave]),X(6));
}
".

cost_center_node_value(Name/Arity, res(Res, Type), Counts) :-
	cost_center_node_value_1(Type, Res, Name, Arity, Counts).

cost_center_node_value_1(call_exit, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 0, 0, Value).
cost_center_node_value_1(call_fail, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 0, 1, Value).
cost_center_node_value_1(redo_exit, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 1, 0, Value).
cost_center_node_value_1(redo_fail, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 1, 1, Value).
cost_center_node_value_1(call, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 0, 0, V0),
	cost_center_node_value_2(Res, Name, Arity, 0, 1, V1),
	Value is V0 + V1.
cost_center_node_value_1(redo, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 1, 0, V0),
	cost_center_node_value_2(Res, Name, Arity, 1, 1, V1),
	Value is V0 + V1.

cost_center_node_value_2(counts, Name, Arity, Enter, Leave, Counts) :-
	cost_center_node_counts(Name, Arity, Enter, Leave, Counts).
cost_center_node_value_2(ticks, Name, Arity, Enter, Leave, Ticks) :-
	cost_center_node_ticks(Name, Arity, Enter, Leave, Ticks).

:- true pred cost_center_node_counts(Name, Arity, Enter,
	    Leave, Counts) :: atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_node_counts)) -->

"
bool_t prolog_cost_center_node_counts(Arg)
    Argdecl;
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  unsigned long int counts = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  enter=GetInteger(X(2));
  leave=GetInteger(X(3));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      if (ecc->functor[0]==f) {
        counts+=ecc->counts[enter][leave];
      }
    }
  while (ht_next(cct));
  return cunify(Arg,MakeSmall(counts),X(4));
}
".

:- true pred cost_center_node_ticks(Name, Arity, Enter,
	    Leave, Ticks) :: atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_node_ticks)) -->

"
bool_t prolog_cost_center_node_ticks(Arg)
    Argdecl;
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  ENG_LINT times = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  enter=GetInteger(X(2));
  leave=GetInteger(X(3));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      if (ecc->functor[0]==f) {
        times+=ecc->times[enter][leave];
      }
    }
  while (ht_next(cct));
  return cunify(Arg,MakeFloat(Arg,times),X(4));
}
".

cost_center_global_value(res(Res, Type), Counts) :-
	cost_center_global_value_1(Type, Res, Counts).

cost_center_global_value_1(call_exit, Res, Value) :-
	cost_center_global_value_2(Res, 0, 0, Value).
cost_center_global_value_1(call_fail, Res, Value) :-
	cost_center_global_value_2(Res, 0, 1, Value).
cost_center_global_value_1(redo_exit, Res, Value) :-
	cost_center_global_value_2(Res, 1, 0, Value).
cost_center_global_value_1(redo_fail, Res, Value) :-
	cost_center_global_value_2(Res, 1, 1, Value).
cost_center_global_value_1(call, Res, Value) :-
	cost_center_global_value_2(Res, 0, 0, V0),
	cost_center_global_value_2(Res, 0, 1, V1),
	Value is V0 + V1.
cost_center_global_value_1(redo, Res, Value) :-
	cost_center_global_value_2(Res, 1, 0, V0),
	cost_center_global_value_2(Res, 1, 1, V1),
	Value is V0 + V1.

cost_center_global_value_2(counts, Enter, Leave, Counts) :-
	cost_center_global_counts(Enter, Leave, Counts).
cost_center_global_value_2(ticks, Enter, Leave, Ticks) :-
	cost_center_global_ticks(Enter, Leave, Ticks).

:- true pred cost_center_global_counts(Enter, Leave, Counts) :: int * int * int
	+ (foreign_low(prolog_cost_center_global_counts)) -->

"
bool_t prolog_cost_center_global_counts(Arg)
    Argdecl;
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  unsigned long int counts = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  enter=GetInteger(X(0));
  leave=GetInteger(X(1));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      counts+=ecc->counts[enter][leave];
    }
  while (ht_next(cct));
  return cunify(Arg,MakeSmall(counts),X(2));
}
".


:- true pred cost_center_global_ticks(Enter, Leave, Counts) :: int * int * int
	+ (foreign_low(prolog_cost_center_global_ticks)) -->

"
bool_t prolog_cost_center_global_ticks(Arg)
    Argdecl;
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  ENG_LINT times = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  enter=GetInteger(X(0));
  leave=GetInteger(X(1));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      times+=ecc->times[enter][leave];
    }
  while (ht_next(cct));
  return cunify(Arg,MakeFloat(Arg,times),X(2));
}
".

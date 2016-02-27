:- module(profiler_utils_base,
	    [
		profile_error/1,
		profile_enter_call/0,
		profile_enter_redo/0,
		profile_leave_exit/0,
		profile_leave_fail/0,
		profile_leave_error/0],
	    [assertions, nativeprops, foreign_interface]).

:- use_foreign_library('Win32i86', [hashtable_Win32i86]).
:- use_foreign_library('Win32i86', [hrtime_Win32i86]).
:- use_foreign_library('Win32i86', [profiler_rt_Win32i86]).

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

static void profile_enter(int entry_port, definition_t *functor) {
  definition_t *f[2]={NULL, NULL};
  ENG_LINT d;
  if (active_frame<prof_frames) {
    tick_start+=tick_ini_profiling-tick_last_addition;
    stop_on_pred_calls=TRUE;
    profile=TRUE;
    d=tick_ini_profiling-tick_profiling;
  } else {
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
  }
  active_frame++;
  active_ecc=get_edge_cc(active_frame->edge_table_cc, f);
  active_ecc->entry_port=ENTER_CALL;
#if defined(PROFILE)
  active_ecc->hooks=profile_rcc; /* Don't accumulate costs in rcc */
  if (profile_hooks) {
    if (active_ecc->hooks)
      ENABLE_HOOKS(functor)
    else
      DISABLE_HOOKS;
  }
#endif
  tick_last_addition=tick_ini_profiling;
  active_ecc->entry_time=d;
}

static void profile_leave(int output_port) {
  ENG_LINT d;
#if defined(PROFILE)
  if (profile_hooks && active_ecc->hooks) {
    d=tick_last_addition-tick_profiling;
    tick_profiling+=tick_ini_profiling-tick_last_addition;
  } else
#endif
    d=tick_ini_profiling-tick_profiling;
  active_ecc->counts[active_ecc->entry_port][output_port]++;
  active_ecc->times[active_ecc->entry_port][output_port]+=d-active_ecc->entry_time;
  active_ecc->entry_time=0;
  active_ecc=NULL;
#if defined(PROFILE)
  if (profile_hooks) {
    DISABLE_HOOKS;
  }
#endif
  if (active_frame==prof_frames) {
    stop_on_pred_calls=predtrace;
    profile=FALSE;
  }
  tick_last_addition=tick_ini_profiling;
  active_frame--;
}
").

profile_leave_fail.
profile_leave_fail :- profile_leave_fail_1.

profile_enter_redo.
profile_enter_redo :- profile_enter_redo_1.

profile_error(E) :-
	profile_leave_error,
	throw(E).

% Debugger hook ??? Try to figure out if this is required:
% profile_start :- profile_start.

:- true pred profile_enter_call
	+ (foreign_low(prolog_profile_enter_call), not_fails)
#
	"Turn on the profiler." -->

"
void profile_enter_call_(void)
{
  PROFILE__TIME_INI;
  profile_enter(ENTER_CALL, profile_enter_call);
  PROFILE__TIME_END;
}

bool_t prolog_profile_enter_call(Arg)
     Argdecl;
{
  profile_enter_call_();
  return TRUE;
}
".

:- true pred profile_enter_redo_1
	+ (foreign_low(prolog_profile_enter_redo_1), fails)
#
	"Turn on the profiler." -->
"
bool_t prolog_profile_enter_redo_1(Arg)
     Argdecl;
{
  PROFILE__TIME_INI;
  profile_enter(ENTER_REDO, profile_enter_redo_1);
  PROFILE__TIME_END;
  return FALSE;
}
".

:- true pred profile_leave_exit
	+ (foreign_low(prolog_profile_leave_exit), not_fails)
# "Turn off the profiler." -->
"
void profile_leave_exit_(void)
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_EXIT);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
}

bool_t prolog_profile_leave_exit(Arg)
     Argdecl;
{
  profile_leave_exit_();
  return TRUE;
}
".

:- true pred profile_leave_fail_1
	+ (foreign_low(prolog_profile_leave_fail_1), fails)
# "Turn off the profiler." -->
"
bool_t prolog_profile_leave_fail_1(Arg)
     Argdecl;
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_FAIL);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
  return FALSE;
}
".

:- true pred profile_leave_error + (foreign_low(prolog_profile_leave_error)) #
	"Turn off the profiler." -->

"
bool_t prolog_profile_leave_error(Arg)
     Argdecl;
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_EXIT);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
  return TRUE;
}
".

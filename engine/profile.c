#include "datadefs.h"
#include "initial.h"
#include "predtyp.h"
#include "profile_defs.h"
#include "timing_defs.h"

#include <stdlib.h>
#include <math.h>

#if defined(PROFILE)
extern BOOL prof_include_time;

static int compare_times(const void *arg1, const void *arg2);

static char* predicate_type(int i);

ENG_FLT time_last_addition;
struct definition *last_called_predicate = NULL;

void add_to_profiling(functor)
  struct definition *functor;
{
  ENG_FLT time_now;

  functor->no_of_calls++;

  if (prof_include_time){
    time_now = usertime();
    if (last_called_predicate) 
      last_called_predicate->time_spent += 
        (unsigned long int)((time_now - time_last_addition)*1e6);
    time_last_addition = time_now;
    last_called_predicate = functor;
  }
}


void dump_profile(void)
{
  struct sw_on_key *table = (struct sw_on_key *)*predicates_location;
  struct sw_on_key_node *keyval;
  int j = SwitchSize(*predicates_location);
  int realsize = 0, 
      tot_reductions = 0;
  struct definition **pred_table, *d;
  unsigned long int total_time = 1;  /* microsecs; avoid dividing by zero */

  printf("\n\nProfile information:\n");

  for (--j; j>=0; --j) {           /* Find how many preds. we have called */
    keyval = &table->tab.asnode[j];
    if ((d  = keyval->value.def) &&
        d -> predtyp != ENTER_UNDEFINED &&
        d->no_of_calls){
      realsize++;
      tot_reductions += d->no_of_calls;
      total_time += d->time_spent;
    }
  }

  printf("%d predicates called, %d calls made, %.2f secs. accumulated time\n", 
         realsize, 
         tot_reductions,
         (double)total_time/1e6);
  pred_table =                         /* Make a table with room for them */
    (struct definition **)checkalloc(realsize*sizeof(struct definition *));

  j = SwitchSize(*predicates_location);
  realsize = 0;
  for (--j; j>=0; --j) {
    keyval = &table->tab.asnode[j];
    if ((d  = keyval->value.def) &&
        d -> predtyp != ENTER_UNDEFINED &&
        d->no_of_calls)
      pred_table[realsize++] = d;
  }

  qsort(pred_table, realsize, sizeof(struct definition *), compare_times);
  
  printf("Calls \t\t Time (rough) \t\t Type    Spec\n");
  printf("===== \t\t ============ \t\t ====    ====\n");
           
  while(realsize >0){
    d = pred_table[--realsize];
    printf("%ld (%.2f%%) \t %f (%.2f%%) \t %s  %s/%d\n",
           d->no_of_calls,
           (double)(d->no_of_calls)*100.0/(double)tot_reductions,
           ((double)d->time_spent)/1.0e6,
           (double)(d->time_spent)*100.0/(double)total_time,
           predicate_type(d->predtyp),
           GetString(d->printname),
           d->arity);
  }
  checkdealloc(pred_table);
}

static int compare_times(arg1, arg2)
     const void *arg1, *arg2;
{
  struct definition **pred1, **pred2;
  
  pred1 = (struct definition **)arg1;
  pred2 = (struct definition **)arg2;
  
  if ((*pred1)->no_of_calls > (*pred2)->no_of_calls)
    return (1);
  if ((*pred1)->no_of_calls < (*pred2)->no_of_calls)
    return (-1);
  return (0);
}

char * predicate_type(int t)
{
  switch (t) {
  case ENTER_COMPACTCODE:
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE:
  case ENTER_PROFILEDCODE_INDEXED: return "Emul  " ;
  case ENTER_FASTCODE:
  case ENTER_FASTCODE_INDEXED:     return "Fast  " ;
  case ENTER_UNDEFINED:            return "Undef " ;
  case ENTER_C:                    return "C     " ;
  case ENTER_INTERPRETED:          return "Interp" ;
  case BUILTIN_ABORT:
  case BUILTIN_APPLY:
  case BUILTIN_CALL:
  case BUILTIN_SYSCALL:
  case BUILTIN_NODEBUGCALL:
  case BUILTIN_TRUE:
  case BUILTIN_FAIL:
  case BUILTIN_CURRENT_INSTANCE:
  case BUILTIN_RESTORE:
  case BUILTIN_COMPILE_TERM:
  case BUILTIN_GELER:
  case BUILTIN_INSTANCE:
  case BUILTIN_DIF:               return "Built " ;
  default:                        return "Other " ;
  }
}

#endif

#include "datadefs.h"
#include "initial.h"
#include "predtyp.h"
#include "profile_defs.h"
#include "timing_defs.h"
#include "alloc_defs.h"

#include <stdlib.h>
#include <math.h>

#if defined(PROFILE)
extern BOOL prof_include_time;

static int compare_times(const void *arg1, const void *arg2);

/* extern char* predicate_type(int i); */

ENG_FLT time_last_addition;
struct definition *last_called_predicate = NULL;


int call_stack_point = 0;

struct call_stack_node int_call_stack[CALL_STACK_SIZE];

struct call_stack_node *call_stack = int_call_stack;

void add_to_profiling(functor,w)
  struct definition *functor;
  struct worker *w;
{
  ENG_FLT time_now;
/*   int last_cp = ChoiceToInt(w->node); */
  struct call_stack_node *csn;
  int cframe;
  functor->no_of_calls++;

  ComputeA(w->local_top,w->node);
  cframe = (TAGGED *)w->local_top-w->stack_start;
/*   if((call_stack_point<0) */
/*   { */
/*     call_stack_point++; */
/*       csn=&call_stack[call_stack_point]; */
/*       csn->functor=functor; */
/*       csn->frame=cframe; */
/*       csn->no_of_calls=1; */
/*   } else { */
    
/*   } */

  call_stack_point++;
  csn=&call_stack[call_stack_point];
  csn->functor=functor;
  csn->choice=ChoiceToInt(w->node);
  csn->frame=cframe;

/*     printf("+call-point=%d ", call_stack_point); */
/*     printf("%s \t", predicate_type(functor->predtyp)); */
/*     if(IsString(functor->printname)) */
/*       printf("%s/", GetString(functor->printname)); */
/*     else */
/*       printf("*** Unknown term ***\/"); */
/*     printf("%d\n", functor->arity); */

/*   if((call_stack_point<0) */
/*      ||((csn = &call_stack[call_stack_point])->functor!=functor) */
/*      ||(csn->frame!=cframe)) */
/*     { */
/*       call_stack_point++; */
/*       csn=&call_stack[call_stack_point]; */
/*       csn->functor=functor; */
/* /\*      csn->node=w->node; *\/ */
/*       csn->frame=cframe; */
/*       csn->no_of_calls=1; */
/*     } */
/*   else */
/*     csn->no_of_calls++; */

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
    tot_reductions = 0,
    tot_fails = 0,
    tot_redos = 0;

  struct definition **pred_table, *d;
  unsigned long int total_time = 1;  /* microsecs; avoid dividing by zero */

  printf("\n\nProfile information:\n");

  for (--j; j>=0; --j) {           /* Find how many preds. we have called */
    keyval = &table->tab.asnode[j];
    if ((d = keyval->value.def) &&
        d -> predtyp != ENTER_UNDEFINED &&
        d->no_of_calls){
      realsize++;      
      tot_reductions += d->no_of_calls;
      tot_fails += d->no_of_fails;
      tot_redos += d->no_of_redos;
      total_time += d->time_spent;
    }
  }

  printf("%d predicates called, %d calls made, %d fails, %d redos %.2f secs. accumulated time\n", 
         realsize,
         tot_reductions,
	 tot_fails,
	 tot_redos,
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
  
  printf("Calls \t Fails \t Redos \t\t\t Time (rough) \t\t Type    Spec\n");
  printf("===== \t ===== \t ===== \t\t\t ============ \t\t ====    ====\n");
           
  while(realsize >0){
    d = pred_table[--realsize];
    printf("%ld \t %ld \t %ld \t (%.2f%%) \t %f (%.2f%%) \t %s  %s/%d\n",
           d->no_of_calls,
	   d->no_of_fails,
	   d->no_of_redos,
           (double)(d->no_of_calls)*100.0/(double)tot_reductions,
           ((double)d->time_spent)/1.0e6,
           (double)(d->time_spent)*100.0/(double)total_time,
           predicate_type(d->predtyp),
           GetString(d->printname),
           d->arity);
  }
  checkdealloc((TAGGED *)pred_table, realsize*sizeof(struct definition *));
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

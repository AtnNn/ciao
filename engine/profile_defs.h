#if defined(PROFILE)
void dump_profile(void);
void add_to_profiling(struct definition *functor, struct worker *w);
char * predicate_type(int t);

#define CALL_STACK_SIZE 1048576
 /* ~ 12MB */

struct call_stack_node {
  struct definition *functor;
  int choice;
  int frame;
/*   int no_of_calls; */
};

struct call_stack_node *call_stack;

int call_stack_point;

struct definition *last_called_predicate;

#define PROFILE__RESET_CALL_STACK \
{ \
  call_stack_point = -1; /* empty stack */ \
  call_stack[0].functor=NULL; \
}

#define PROFILE__FAIL_HOOK \
{ \
  struct definition *pf=NULL;   /* EMM */ \
  int cframe; \
  int choice; \
  pf=NULL; \
  /* now redo from the call_stack up to the first actual choice point */ \
  ComputeA(w->local_top,w->node); \
  /* cframe = (TAGGED *)w->node->local_top - w->stack_start; */ \
  choice = ChoiceToInt(w->node); \
  while(((int)(call_stack[call_stack_point].choice)>=choice)&&(call_stack_point>=0)) \
  { \
    pf = call_stack[call_stack_point].functor; \
/*     printf("+fail-point=%d ", call_stack_point); */ \
/*     printf("%s \t", predicate_type(pf->predtyp)); */ \
/*     if(IsString(pf->printname)) */ \
/*       printf("%s/", GetString(pf->printname)); */ \
/*     else */ \
/*       printf("*** Unknown term ***\/"); */ \
/*     printf("%d\n", pf->arity); */ \
    pf->no_of_fails ++;/* = call_stack[call_stack_point].no_of_calls; */ \
    call_stack[call_stack_point].functor=NULL; \
    call_stack_point--; \
  } \
  if(call_stack_point>=0) { \
    pf = call_stack[call_stack_point].functor; \
    if(pf!=NULL) { \
/*       printf(" redo-point=%d ", call_stack_point); */ \
/*       printf("%s \t", predicate_type(pf->predtyp)); */ \
/*       if(IsString(pf->printname)) */ \
/*         printf("%s/", GetString(pf->printname)); */ \
/*       else */ \
/* 	printf("*** Unknown term ***\/"); */ \
/*       printf("%d ", pf->arity); */ \
/*       if(w->node->next_alt) */ \
/* 	printf("clause %d \n", w->node->next_alt->number); */ \
/*       else */ \
/* 	printf("No more alts\n"); */ \
      pf->no_of_redos ++; \
    } \
  } \
}

#else
#define PROFILE__FAIL_HOOK
#define PROFILE__RESET_CALL_STACK
#endif

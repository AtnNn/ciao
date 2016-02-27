/* Copyright (C) 1996,1997,1998, UPM-CLIP */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>


/* INITIALIZATIONS   and support functions for initializations  */

#include "datadefs.h"
#include "support.h"
#include "predtyp.h"
#include "wambuiltin.h"
#include "wamfunction.h"
/*#include "installibdir.h"*/
#include "threads.h"
#include "task_areas.h"

/* declarations for global functions accessed here */

#include "initial_defs.h"
#include "alloc_defs.h"
#include "interrupt_defs.h"
#include "unix_utils_defs.h"
#include "term_support_defs.h"
#include "support_defs.h"
#include "timing_defs.h"
#include "locks_defs.h"
#include "prolog_tasks_defs.h"
#include "indexing_defs.h"                               /* JFMC: abolish */


/* local declarations */

static BOOL prolog_atom_mode(Argdecl);
static struct definition *define_builtin(char *pname, int instr, int arity);
static void classify_atom(struct atom *s);
static BOOL prolog_ciaolibdir(Argdecl);
static void initialize_intrinsics(void);
static void deffunction(char *atom, int arity, CInfo proc, int funcno);
static void define_functions(void);


/*
  void compute_cwd();
  void control_c_normal PROTO((Argdecl));
  void init_streams_each_time();
  BOOL prolog_init_radix();
  void enable_conditions();
*/

struct statistics stats = {0.0, 
                           0, 
                           0, 
                           0, 
                           0.0,
                           0, 
                           0, 
                           0.0, 
                           0.0,
                           0.0,
                           0.0};                                /* Shared */

struct sw_on_key_node **atmtab; /* Shared --- but need lock when accessing /
                                   reallocing it! */
struct sw_on_key *prolog_atoms;  /* Shared -- need lock when adding atoms */
static char                  /* Shared -- for adding new atoms; need lock */
  *prolog_chars=NULL,
  *prolog_chars_end=NULL;

CInfo builtintab[64];                                           /* Shared */


                /* Shared -- related with the hashing of arith. functions */
struct sw_on_key *switch_on_function;

#if defined(MARKERS)
extern BOOL prolog_launch_goal PROTO((struct worker *w));
extern BOOL prolog_wait_for_goal PROTO((struct worker *w));
extern BOOL prolog_fail_goal PROTO((struct worker *w));
extern BOOL prolog_redo_goal PROTO((struct worker *w));
#endif

extern struct worker *create_wam_storage PROTO((void));
extern void numstack_init PROTO((Argdecl));

extern BOOL prolog_bootversion PROTO((struct worker *w));
extern BOOL prolog_sourcepath PROTO((struct worker *w));
extern BOOL prolog_open PROTO((struct worker *w));
extern BOOL prolog_close PROTO((struct worker *w));
extern BOOL prolog_unix_popen PROTO((struct worker *w));

extern BOOL set_predtrace PROTO((struct worker *w));

extern BOOL prolog_display PROTO((struct worker *w));
extern BOOL prolog_display2 PROTO((struct worker *w));
extern BOOL prolog_displayq PROTO((struct worker *w));
extern BOOL prolog_displayq2 PROTO((struct worker *w));
extern BOOL prolog_stream_code PROTO((struct worker *w));
extern BOOL prolog_current_input PROTO((struct worker *w));
extern BOOL prolog_set_input PROTO((struct worker *w));
extern BOOL prolog_current_output PROTO((struct worker *w));
extern BOOL prolog_set_output PROTO((struct worker *w));
extern BOOL character_count PROTO((struct worker *w));
extern BOOL line_position PROTO((struct worker *w));
extern BOOL line_count PROTO((struct worker *w));
/*
extern BOOL prolog_connect_to_socket PROTO((struct worker *w));
extern BOOL prolog_bind_socket PROTO((struct worker *w));
extern BOOL prolog_socket_accept PROTO((struct worker *w));
extern BOOL prolog_select_socket PROTO((struct worker *w));
*/
extern BOOL code_class PROTO((struct worker *w));
extern BOOL flush_output PROTO((struct worker *w));
extern BOOL flush_output1 PROTO((struct worker *w));
extern BOOL getct PROTO((struct worker *w));
extern BOOL getct1 PROTO((struct worker *w));
extern BOOL get PROTO((struct worker *w));
extern BOOL get2 PROTO((struct worker *w));
extern BOOL get1 PROTO((struct worker *w));
extern BOOL get12 PROTO((struct worker *w));
extern BOOL peek PROTO((struct worker *w));
extern BOOL peek2 PROTO((struct worker *w));
extern BOOL nl PROTO((struct worker *w));
extern BOOL nl1 PROTO((struct worker *w));
extern BOOL put PROTO((struct worker *w));
extern BOOL put2 PROTO((struct worker *w));
extern BOOL skip PROTO((struct worker *w));
extern BOOL skip2 PROTO((struct worker *w));
extern BOOL tab PROTO((struct worker *w));
extern BOOL tab2 PROTO((struct worker *w));
extern BOOL prolog_clearerr PROTO((struct worker *w));
extern BOOL prolog_erase PROTO((struct worker *w));
extern BOOL prolog_purge PROTO((struct worker *w));
extern BOOL prolog_ptr_ref PROTO((struct worker *w));
extern BOOL inserta PROTO((struct worker *w));
extern BOOL insertz PROTO((struct worker *w));
extern BOOL make_bytecode_object PROTO((struct worker *w));
extern BOOL prolog_name PROTO((struct worker *w));
extern BOOL prolog_atom_codes PROTO((struct worker *w));
extern BOOL prolog_number_codes PROTO((struct worker *w));
extern BOOL prolog_atom_length PROTO((struct worker *w));
extern BOOL prolog_sub_atom PROTO((struct worker *w));
extern BOOL prolog_atom_concat PROTO((struct worker *w));
extern BOOL prolog_copy_term PROTO((struct worker *w));
extern BOOL context_switch PROTO((struct worker *w));
extern BOOL prolog_abolish PROTO((struct worker *w)); /* JFMC (prolog_) */
extern BOOL erase_clause PROTO((struct worker *w)); 
extern BOOL clause_number PROTO((struct worker *w));
extern BOOL compiled_clause PROTO((struct worker *w));
extern BOOL define_predicate PROTO((struct worker *w));
extern BOOL empty_gcdef_bin PROTO((struct worker *w));
extern BOOL metachoice PROTO((struct worker *w));
extern BOOL metacut PROTO((struct worker *w));
extern BOOL retry_cut PROTO((struct worker *w));
extern BOOL frozen PROTO((struct worker *w));
extern BOOL defrost PROTO((struct worker *w));
extern BOOL setarg PROTO((struct worker *w));
extern BOOL undo PROTO((struct worker *w));
extern BOOL prolog_qread PROTO((struct worker *w));
extern BOOL push_qlinfo PROTO((struct worker *w));
extern BOOL pop_qlinfo PROTO((struct worker *w));
extern BOOL set_glv PROTO((struct worker *w));
extern BOOL get_glv PROTO((struct worker *w));
extern BOOL prolog_erase_atom PROTO((struct worker *w));
extern BOOL prolog_current_executable PROTO((struct worker *w));
extern BOOL prompt PROTO((struct worker *w));
extern BOOL unknown PROTO((struct worker *w));
extern BOOL constraint_list PROTO((struct worker *w));
extern BOOL prolog_eq PROTO((struct worker *w));
extern BOOL prolog_repeat PROTO((struct worker *w));
extern BOOL current_atom PROTO((struct worker *w));
extern BOOL current_clauses PROTO((struct worker *w));
extern BOOL close_predicate PROTO((struct worker *w));
extern BOOL open_predicate PROTO((struct worker *w));
extern BOOL first_instance PROTO((struct worker *w));
extern BOOL current_stream PROTO((struct worker *w));
extern BOOL current_predicate PROTO((struct worker *w));
extern BOOL predicate_property PROTO((struct worker *w));
extern BOOL prolog_runtime PROTO((struct worker *w));
/* extern BOOL prolog_walltime PROTO((struct worker *w)); */
extern BOOL prolog_datime PROTO((struct worker *w));
extern BOOL prolog_time PROTO((struct worker *w));
extern BOOL termheap_usage PROTO((struct worker *w));
extern BOOL envstack_usage PROTO((struct worker *w));
extern BOOL trail_usage PROTO((struct worker *w));
extern BOOL choice_usage PROTO((struct worker *w));
extern BOOL statistics PROTO((struct worker *w));
extern BOOL program_usage PROTO((struct worker *w));
extern BOOL internal_symbol_usage PROTO((struct worker *w));
extern BOOL total_usage PROTO((struct worker *w));
extern BOOL stack_shift_usage PROTO((struct worker *w));
/* extern BOOL leash_mode PROTO((struct worker *w)); */ /* removed (DCG) */
/* extern BOOL maxdepth PROTO((struct worker *w)); */
/* extern BOOL printdepth PROTO((struct worker *w)); */
/* extern BOOL breaklevel PROTO((struct worker *w)); */
extern BOOL compiling PROTO((struct worker *w));
extern BOOL ferror_flag PROTO((struct worker *w));
/*extern BOOL single_var_flag PROTO((struct worker *w));*/
/* extern BOOL character_escapes_flag PROTO((struct worker *w)); */
/* extern BOOL redefine_flag PROTO((struct worker *w)); */
extern BOOL quiet_flag PROTO((struct worker *w));
extern BOOL spypoint PROTO((struct worker *w));
extern BOOL debugger_state PROTO((struct worker *w));
extern BOOL debugger_mode PROTO((struct worker *w));
extern BOOL prolog_radix PROTO((struct worker *w));
extern BOOL gc_mode PROTO((struct worker *w));
extern BOOL gc_trace PROTO((struct worker *w));
extern BOOL gc_margin PROTO((struct worker *w));
extern BOOL gc_usage PROTO((struct worker *w));
extern BOOL gc_start PROTO((struct worker *w));


extern BOOL prolog_unix_cd PROTO((struct worker *w));
extern BOOL prolog_unix_shell0 PROTO((struct worker *w));
extern BOOL prolog_unix_shell2 PROTO((struct worker *w));
extern BOOL prolog_unix_system2 PROTO((struct worker *w));
extern BOOL prolog_exec PROTO((struct worker *w));
extern BOOL prolog_unix_argv PROTO((struct worker *w));
extern BOOL prolog_unix_exit PROTO((struct worker *w));
extern BOOL prolog_unix_mktemp PROTO((struct worker *w));
extern BOOL prolog_unix_access PROTO((struct worker *w));
extern BOOL prolog_directory_files PROTO((struct worker *w));
extern BOOL prolog_file_properties PROTO((struct worker *w));
extern BOOL prolog_unix_chmod PROTO((struct worker *w));
extern BOOL prolog_unix_umask PROTO((struct worker *w));
extern BOOL prolog_unix_delete PROTO((struct worker *w));
extern BOOL prolog_current_host PROTO((struct worker *w));
extern BOOL prolog_getenvstr PROTO((struct worker *w));
/* extern BOOL prolog_pause PROTO((struct worker *w)); */
/* extern BOOL prolog_getpid PROTO((struct worker *w)); */


/*
extern BOOL prolog_load_foreign_files PROTO((struct worker *w));
extern BOOL prolog_prepare_foreign_files PROTO((struct worker *w));
extern BOOL prolog_foreign_base PROTO((struct worker *w));
*/
extern BOOL prolog_find_file PROTO((struct worker *w));
extern BOOL prolog_format_print_integer PROTO((struct worker *w));
extern BOOL prolog_format_print_float PROTO((struct worker *w));
extern BOOL prolog_save PROTO((struct worker *w));
extern BOOL prolog_savecode PROTO((struct worker *w));
#if defined(OLD_DATABASE)
extern BOOL current_key PROTO((struct worker *w));
#endif
extern BOOL large_data PROTO((struct worker *w));
extern BOOL prolog_interpreted_clause PROTO((struct worker *w));
extern BOOL set_property PROTO((struct worker *w));

#if defined(UNDEFINED)
extern BOOL prolog_launch_goal_plus  PROTO((struct worker *w));
#endif


extern BOOL prolog_eng_call                 PROTO((struct worker *w));
extern BOOL prolog_eng_backtrack            PROTO((struct worker *w));
extern BOOL prolog_eng_cut                  PROTO((struct worker *w));


#if defined(UNDEFINED)
extern BOOL prolog_launch_goal       PROTO((struct worker *w));
extern BOOL prolog_launch_goal_shv   PROTO((struct worker *w));
extern BOOL prolog_launch_goal_cont  PROTO((struct worker *w));
#endif
extern BOOL prolog_tasks_status      PROTO((struct worker *w));
extern BOOL prolog_kill_thread       PROTO((struct worker *w));
extern BOOL prolog_kill_other_threads PROTO((struct worker *w));
extern BOOL prolog_thread_self       PROTO((struct worker *w));
extern BOOL prolog_join_goal         PROTO((struct worker *w));
#if defined(UNDEFINED)
extern BOOL prolog_backtrack_goal    PROTO((struct worker *w));
#endif
extern BOOL prolog_release_goal      PROTO((struct worker *w));



extern BOOL prolog_lock_atom         PROTO((struct worker *w));
extern BOOL prolog_unlock_atom       PROTO((struct worker *w));
extern BOOL prolog_lock_atom_state   PROTO((struct worker *w));
extern BOOL prolog_unlock_predicate  PROTO((struct worker *w));

#if defined(GAUGE)
extern BOOL emulated_clause_counters PROTO((struct worker *w));
extern BOOL counter_values PROTO((struct worker *w));
extern BOOL reset_counters PROTO((struct worker *w));
#endif

                                                            
extern BOOL prolog_walltime PROTO((struct worker *w));             /* MCL */
extern BOOL prolog_pause PROTO((struct worker *w));                /* MCL */
extern BOOL prolog_getpid PROTO((struct worker *w));               /* MCL */
extern BOOL prolog_getarch PROTO((struct worker *w));               /* MCL */
extern BOOL prolog_getos PROTO((struct worker *w));               /* MCL */

extern BOOL prolog_dynlink PROTO((struct worker *w));              /* MCL */
extern BOOL prolog_dynunlink PROTO((struct worker *w));            /* JFMC */

/* GLOBAL DATA STRUCTURES */

BOOL predtrace     = FALSE;            /* trace predicate calls -- Shared */
BOOL profile       = FALSE;        /* profile program execution -- Shared */
BOOL prof_include_time = FALSE;      /* include time in profile -- Shared */
BOOL stop_on_pred_calls = FALSE;            /* profile or trace -- Shared */
#if defined(DEBUG)
BOOL debug_gc      = FALSE;         /* debug garbage collection -- Shared */
BOOL debug_threads = FALSE;            /* debug thread creation -- Shared */
BOOL debug_mem = FALSE;              /* debug memory manegement -- Shared */
BOOL debug_conc = FALSE;                   /* debug concurrency -- Shared */
#endif

/* Shared? Not easy: they have to do with the lifetime of dyn. predicates  */
CLOCK def_clock=0, use_clock=0;

/* TAGGED *heap_start , *heap_end, *heap_warn, *heap_warn_soft, stack_start,
   *stack_end, *stack_warn, tagged_choice_start, choice_start, choice_end,
   *trail_start, *trail_end; */

/* char *atom_buffer; */ /* Non shared */
/* int atom_buffer_length; */ /* Non shared */

struct stream_node *stream_user_input;                          /* Shared */
struct stream_node *stream_user_output;                         /* Shared */
struct stream_node *stream_user_error;                          /* Shared */


struct stream_node *root_stream_ptr;               /* Shared and _locked_ */

struct sw_on_key  *prolog_predicates = NULL;                    /* Shared */
/*struct sw_on_key *user_predicates = NULL;*/
struct sw_on_key **predicates_location = &prolog_predicates;    /* Shared */

#if defined(THREADS) && defined(USE_POSIX_THREADS)
pthread_attr_t detached_thread;
pthread_attr_t joinable_thread;
#endif

/* Database locks */
/* All-predicates lock */
LOCK_ST prolog_predicates_st;                                  /* Storage */
LOCK    prolog_predicates_l;                             /* Pointer to it */

/* Per clause lock (but only one; used in '$inserta' and '$insertz') */
/*
LOCK_ST clause_insertion_st;
LOCK    clause_insertion_l;
*/

/* Memory management lock (for internal structures) */
LOCK_ST mem_mng_st;
LOCK    mem_mng_l;

/* Lock to ensure that terms in launch_goal/[1,2,3] are actually copied by the 
   launched thread before returning to our tasks. */
LOCK_ST launch_goal_st;
LOCK    launch_goal_l;

/* Lock to ensure that no two threads try to backtrack at the same time in
   the same goal. */
LOCK_ST backtrack_goal_st;
LOCK    backtrack_goal_l;

/* We are giving worker IDs from a pool, in order to speed up task
   creation. This pool needs exclusive access. */

LOCK_ST worker_id_pool_st;
LOCK    worker_id_pool_l;


/* Counts mutually exclusive operations */
#if defined(DBG)
unsigned long int ops_counter = 0;
LOCK_ST ops_counter_st;
LOCK ops_counter_l;
#endif

BOOL wam_initialized = FALSE;

/* support for tokenizer and for writing out atoms */

#define IsLowerChar(X)  (symbolchar[X]==1)
#define IsUpperChar(X)  (symbolchar[X]==2)
#define IsDigit(X)      (symbolchar[X]==3)
#define IsSymbolChar(X) (symbolchar[X]==4)

/* Shared .... */

char symbolchar[256];

#if defined(MARKERS)
 TAGGED atom_success;
 TAGGED atom_failure;
#endif


 TAGGED atom_share;             /* "share" */
 TAGGED atom_noshare;           /* "noshare" */
 TAGGED atom_nil;		/* "[]" */
 TAGGED atom_list;		/* "." */
 TAGGED atom_user_input;	/* "user_input" */ 
 TAGGED atom_user_output;	/* "user_output" */
 TAGGED atom_user_error;	/* "user_error" */
 TAGGED atom_read;		/* "read"  */
 TAGGED atom_write;		/* "write" */
 TAGGED atom_append;		/* "append" */
 TAGGED atom_socket;		/* "socket" */
 TAGGED atom_symlink;		/* "symlink" */
 TAGGED atom_regular;		/* "regular" */
 TAGGED atom_directory;		/* "directory" */
 TAGGED atom_fifo;		/* "fifo" */
 TAGGED atom_unknown;		/* "unknown" */
 TAGGED atom_prolog;		/* "prolog"  */
 TAGGED atom_user;		/* "user" */
 TAGGED atom_lessthan;		/* "<" */
 TAGGED atom_greaterthan;	/* ">" */
 TAGGED atom_equal;		/* "=" */
 TAGGED atom_off;		/* "off" */
 TAGGED atom_on;		/* "on" */
 TAGGED atom_error;		/* "error" */
 TAGGED atom_trace;		/* "trace" */
 TAGGED atom_debug;		/* "debug" */
 TAGGED atom_fail;		/* "fail" */
 TAGGED atom_all;		/* "all" */
 TAGGED atom_terse;		/* "terse" */
 TAGGED atom_verbose;		/* "verbose" */
 TAGGED atom_compiled;		/* "compiled" */
 TAGGED atom_interpreted;	/* "interpreted" */
 TAGGED atom_builtin;		/* "built_in" */
 TAGGED atom_true;		/* "true" */
 TAGGED atom_retry_hook;	/* "$$retry_hook" */
 TAGGED atom_unprofiled;	/* "unprofiled" */
 TAGGED atom_profiled;   	/* "profiled" */
/* TAGGED atom_public; */		/* "public" */
 TAGGED atom_concurrent;	/* "concurrent" */
 TAGGED atom_wait;		/* "wait" */
 TAGGED atom_dynamic;		/* "dynamic" */
 TAGGED atom_multifile;		/* "multifile" */
 TAGGED atom_installibdir;

 TAGGED atom_block;      	/* "block" */
 TAGGED atom_no_block;	        /* "no_block" */


 TAGGED atom_self;                   /* "self" */
 TAGGED atom_create;                 /* "create" */



#if defined(GAUGE)
 TAGGED atom_counter;           /* "counter" */
#endif

 TAGGED functor_neck;
 TAGGED functor_list;
 TAGGED functor_cut;
 TAGGED functor_minus;
 TAGGED functor_slash;
 TAGGED functor_and;
 TAGGED functor_functor;
 TAGGED functor_tagged;
 TAGGED functor_emul_entry;
 TAGGED functor_builtin;
 TAGGED functor_Dref;
 TAGGED functor_Dstream;
 TAGGED functor_Dsetarg;
 TAGGED functor_large;
 TAGGED functor_long;

 TAGGED current_prompt;
 TAGGED current_unknown;
/*  TAGGED current_leash_mode; */
/*  TAGGED current_maxdepth; */
/*  TAGGED current_printdepth; */
/*  TAGGED current_breaklevel; */
 TAGGED current_compiling;
 TAGGED current_ferror_flag;
/*  TAGGED current_single_var_flag; */
/*  TAGGED current_character_escapes_flag; */
/*  TAGGED current_redefine_flag; */
 TAGGED current_quiet_flag;
 TAGGED current_gcmode;
 TAGGED current_gctrace;
 TAGGED current_gcmargin;
/* TAGGED current_debugger_state; */ /* Now private */
/* TAGGED current_debugger_mode;  */  /* Now private */
 TAGGED current_radix;
 
 struct try_node *address_nd_repeat;
 struct try_node *address_nd_current_instance;
 struct try_node *address_nd_current_atom;
 struct try_node *address_nd_current_predicate;
 struct try_node *address_nd_predicate_property;
 struct try_node *address_nd_current_stream;
 struct try_node *address_nd_atom_concat;

 struct definition *address_true;
 struct definition *address_fail;
/*
#if !defined(ATTRVARS)
 struct definition *address_fast_apply;
 struct definition *address_slow_apply;
 struct definition *address_apply;
#endif
*/
 struct definition *address_call;
#if defined(INTERNAL_CALLING)
 struct definition *address_internal_call = NULL;
#endif
 struct definition *address_interpret_goal;
 struct definition *address_call_with_cont;                     /* call/3 */
 struct definition *address_interpret_compiled_goal;
 struct definition *address_interpret_c_goal;
 struct definition *address_undefined_goal;
 struct definition *address_help; 
 struct definition *address_restart; 
 struct definition *address_trace;
 struct definition *address_getct;
 struct definition *address_getct1;
 struct definition *address_get;
 struct definition *address_get2;
 struct definition *address_get1;
 struct definition *address_get12;
 struct definition *address_peek;
 struct definition *address_peek2;
 struct definition *address_skip;
 struct definition *address_skip2;
 struct definition *address_error;        /* Handle errors in Prolog (DCG)*/

                                          /* Attributed variables support */
struct definition *address_pending_unifications;
struct definition *address_uvc;
struct definition *address_ucc;


 /*-----------------------------------------------------------------
   INITIALIZATION support functions  
   -----------------------------------------------------------------*/


static void classify_atom(s)
     REGISTER struct atom *s;
{
  REGISTER unsigned char *cp = (unsigned char *)s->name;
  unsigned char c0 = cp[0];
  unsigned char c1 = cp[1];
  unsigned char c2 = cp[2];
  REGISTER unsigned char c;
  BOOL seen_alpha = FALSE;
  BOOL seen_symbol = FALSE;
  
  s->has_dquote = FALSE;	/* TRUE if symbolchars only */
  s->has_squote = FALSE;	/* TRUE if ! ; [] {} OR contains a "'" */
  s->has_special = FALSE;	/* TRUE if needs quoting */
  while ((c = *cp++)) {
    if (c=='\'')
      s->has_squote = s->has_special = TRUE;
    else if (IsLowerChar(c) || IsUpperChar(c) || IsDigit(c))
      seen_alpha = TRUE;
    else if (IsSymbolChar(c))
      seen_symbol = TRUE;
    else s->has_special = TRUE;
  }

  s->has_dquote = (!s->has_special & !seen_alpha);
  s->has_special |= (seen_alpha==seen_symbol);

  /* check cases '!' ';' '[]' '{}' '_...' 'A...' '9...' '.' '/ * ...' */
  /* NB: No point in quoting '... * /' */
    
  if (s->has_special && !s->has_squote &&
      ((c0=='!' && c1==0) ||
       (c0==';' && c1==0) ||
       (c0=='[' && c1==']' && c2==0) ||
       (c0=='{' && c1=='}' && c2==0)))
    s->has_special=FALSE, s->has_squote=TRUE;
  else if (!s->has_special &&
	   (IsUpperChar(c0) ||
	    IsDigit(c0) ||
	    (c0=='.' && c1==0) ||
	    (c0=='/' && c1=='*')))
    s->has_special=TRUE;
}


/* This astonishing piece of code makes an atom from its name and index in
   the hash table.  The memory for the (struct atom) is taken from a linear
   chunk of memory within which a pointer is advanced to reclaim memory for
   the atoms.  When there is not enough room for a new atom, a new area of
   memory is allocated.  MCL. */

struct atom *new_atom_check(str,index)  
     unsigned char *str;   
     unsigned int index;
{
  struct atom *s;
  int len = sizeof(struct atom)+strlen(str)+1-ANY;
  
  /* Adjust to a tagged pointer */
  prolog_chars = (char *)((TAGGED)(prolog_chars+3) & ~3); 
  if (prolog_chars+len > prolog_chars_end)
    prolog_chars = (char *)checkalloc(2040),
    prolog_chars_end = prolog_chars+2040;
  
  s = (struct atom *)prolog_chars, prolog_chars+=len;
  s->index = index;
  (void) strcpy(s->name,(char *)str);
  classify_atom(s);
#if defined(THREADS)
  s->atom_lock_l = create_dynamic_lock();               /* Already inited */
  /*  Quite amazingly, the latter seems to be faster than just

    s->atom_lock_l = &s->atom_lock_st;
    Init_lock(s->atom_lock_l);

    in a i586, probably because it helps to keep the size of the atoms 
    smaller, and so it favours the cache behavior.
  */
  s->atom_lock_counter = 1;                           /* MUTEX by default */
#endif
  return s;
}

/*
void reclassify_atoms()
{
  REGISTER int i;

  for (i=0; i<prolog_atoms->count; i++)
    classify_atom(atmtab[i]->value.atomp);
}
*/		


/* $atom_mode(+Atom, -Context)
 * Context = 2'000 for alpha
 * Context = 2'001 for quote
 * Context = 2'010 for other
 * Context = 2'100 for punct, depending on how Atom is printed.
 */
static BOOL prolog_atom_mode(Arg)
     Argdecl;
{
  REGISTER struct atom *atomptr;

  DEREF(X(0),X(0));
  atomptr = TagToAtom(X(0));
  if (atomptr->has_special)
    Unify_constant(MakeSmall(1),X(1))
  else if (atomptr->has_dquote)
    Unify_constant(MakeSmall(2),X(1))
  else if (atomptr->has_squote)
    Unify_constant(MakeSmall(4),X(1))
  else
    Unify_constant(TaggedZero,X(1))

  return TRUE;
}


static BOOL prolog_ciaolibdir(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  return cunify(Arg, X(0), atom_installibdir);
}

static struct definition *define_builtin(pname,instr,arity)
     char *pname;
     int instr;
     int arity;
{
  struct definition *func;
  ENG_INT current_mem = total_mem_count;
  
 /*func = insert_definition(&prolog_predicates,MakeString(pname),arity,TRUE);*/
  Wait_Acquire_lock(prolog_predicates_l);
  func = insert_definition(predicates_location,MakeString(pname),arity,TRUE);
  Release_lock(prolog_predicates_l);
  /*func->properties.public = 1;*/
  SetEnterInstr(func,instr);
  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}

static TAGGED deffunctor(pname,arity)
     char *pname;
     int arity;
{
  return SetArity(MakeString(pname),arity);
}


struct definition *define_c_predicate(pname,procedure,arity)
     char  *pname;
     BOOL (*procedure)();
     int arity;
{
  struct definition *func;
  ENG_INT current_mem = total_mem_count;
  
  /*func = insert_definition(&prolog_predicates,MakeString(pname),arity,TRUE);*/
  Wait_Acquire_lock(prolog_predicates_l);
  func = insert_definition(predicates_location,MakeString(pname),arity,TRUE);
  Release_lock(prolog_predicates_l);
  /* func->properties.public = public; */
  SetEnterInstr(func,ENTER_C);
#if 0 /* was GAUGE */
  {
    func->code.cinfo = 
      (struct c_code_info *)checkalloc(sizeof(struct c_code_info));
    func->code.cinfo->procedure = procedure;
    func->code.cinfo->counter = 0;
  }
#else
  func->code.cinfo = procedure;
#endif
  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}


struct definition *define_c_mod_predicate(module,pname,procedure,arity)
     char *module;                 /* Module in which the definition goes */
     char  *pname;                               /* Prolog predicate name */
     BOOL (*procedure)();                        /* Pointer to C function */
     int arity;                                     /* Arity of predicate */
{
  struct definition *func;
  REGISTER struct sw_on_key_node *keyval;
  /*  TAGGED tagpname = MakeString(pname);*/
  char mod_pname[MAXATOM];   /* Predicate name with module name prepended */
  /*int mod_pname_len;*/
  TAGGED mod_tagpname;    /* Def. of predicate with module name prepended */
  TAGGED key;
  ENG_INT current_mem = total_mem_count;
               
  if (strlen(module) + strlen(pname) > MAXATOM){           /* Check sizes */
    char errmsg[MAXATOM+MAXATOM+512];

    strcpy(errmsg, "Predicate ");
    strcat(errmsg, pname);
    strcat(errmsg, " in module ");
    strcat(errmsg, module);
    strcat(errmsg, " gave a predicate name too long!");
    USAGE_FAULT(errmsg);
  }

  strcpy(mod_pname, module);/* No need to check length -- already and atom */
  strcat(mod_pname, ":");
  strcat(mod_pname, pname);
  mod_tagpname = MakeString(mod_pname);
  key = SetArity(mod_tagpname, arity);

  Wait_Acquire_lock(prolog_predicates_l);

  keyval = (struct sw_on_key_node *)incore_gethash(prolog_predicates,key);

  if (keyval->key)
    func = keyval->value.def;
  else {
    func = new_functor(mod_tagpname,arity);
    /*add_definition(&prolog_predicates,keyval,key,func);*/
    add_definition(predicates_location,keyval,key,func);
  }

  Release_lock(prolog_predicates_l);

  /* Previously in the else-part (DCG) */
  /*func->properties.public = public;*/
  SetEnterInstr(func,ENTER_C);
  func->code.cinfo = procedure;

  INC_MEM_PROG((total_mem_count - current_mem));
  return func;
}


/* JFMC: undefine a predicate defined with define_c_mod_predicate */
void undefine_c_mod_predicate(char *module, char *pname, int arity) {
  struct definition *f;
  char mod_pname[MAXATOM];   /* Predicate name with module name prepended */
  TAGGED mod_tagpname;    /* Def. of predicate with module name prepended */

  strcpy(mod_pname, module);/* No need to check length -- already and atom */
  strcat(mod_pname, ":");
  strcat(mod_pname, pname);
  mod_tagpname = MakeString(mod_pname);

  Wait_Acquire_lock(prolog_predicates_l);
  f = insert_definition(predicates_location, mod_tagpname, arity, FALSE);
  Release_lock(prolog_predicates_l);

  abolish(NULL, f);
}

/*---------------------------------------------------------------
  GENERAL INITIALIZATION
  ------------------------------------------------------------  */


static void initialize_intrinsics()
{

                               /* kprim.c */
#if defined(MARKERS)
  define_c_predicate("launch_goal", prolog_launch_goal, 2);
  define_c_predicate("wait_for_goal", prolog_wait_for_goal, 1);
  define_c_predicate("fail_goal", prolog_fail_goal, 1);
  define_c_predicate("redo_goal", prolog_redo_goal, 1);
#endif
                              /* initial.c */
  
  define_c_predicate("$atom_mode",prolog_atom_mode,2);
  define_c_predicate("ciaolibdir",prolog_ciaolibdir,1);



				/* streams.c */
  
  define_c_predicate("stream_code",prolog_stream_code,2);
  define_c_predicate("$bootversion",prolog_bootversion,0);
  /*define_c_predicate("$source_path_name",prolog_sourcepath,2); */
  define_c_predicate("$open",prolog_open,3);
  define_c_predicate("close",prolog_close,1); 
  define_c_predicate("$unix_popen",prolog_unix_popen,3);
  define_c_predicate("current_input",prolog_current_input,1);
  define_c_predicate("set_input",prolog_set_input,1);
  define_c_predicate("current_output",prolog_current_output,1);
  define_c_predicate("set_output",prolog_set_output,1);
  define_c_predicate("character_count",character_count,2);
  define_c_predicate("line_position",line_position,2);
  define_c_predicate("line_count",line_count,2);


                              /* objareas.c */

  define_c_predicate("$purge",prolog_purge,1);
  define_c_predicate("$erase",prolog_erase,1);
  define_c_predicate("$ptr_ref",prolog_ptr_ref,2);
  define_c_predicate("$inserta",inserta,2);
  define_c_predicate("$insertz",insertz,2);
  define_c_predicate("$make_bytecode_object",make_bytecode_object,4);
/*  define_c_predicate("$context_switch",context_switch,0);*/ 

				/* term_support.c */

  define_c_predicate("name",prolog_name,2);
  define_c_predicate("atom_codes",prolog_atom_codes,2);
  define_c_predicate("number_codes",prolog_number_codes,2);
  define_c_predicate("atom_length",prolog_atom_length,2);
  define_c_predicate("sub_atom",prolog_sub_atom,4);
  define_c_predicate("atom_concat",prolog_atom_concat,3);
  define_c_predicate("copy_term",prolog_copy_term,2);

				/* indexing.c */

  define_c_predicate("$abolish",prolog_abolish,1); /* JFMC */
  define_c_predicate("$define_predicate",define_predicate,2);
  define_c_predicate("$erase_clause",erase_clause,1);
  define_c_predicate("$clause_number",clause_number,2);
  define_c_predicate("$compiled_clause",compiled_clause,4);
  define_c_predicate("$empty_gcdef_bin",empty_gcdef_bin,0);
  define_c_predicate("$set_property",set_property,2);

				/* inout.c */
  
  define_c_predicate("code_class",code_class,2);
  define_c_predicate("flush_output",flush_output,0);
  define_c_predicate("flush_output",flush_output1,1);
  address_getct = define_c_predicate("getct",getct,2);
  address_getct1 = define_c_predicate("getct1",getct1,2);
  address_get = define_c_predicate("get_code",get,1);
  address_get2 = define_c_predicate("get_code",get2,2);
  address_get1 = define_c_predicate("get1_code",get1,1);
  address_get12 = define_c_predicate("get1_code",get12,2);
  address_peek = define_c_predicate("peek_code",peek,1);
  address_peek2 = define_c_predicate("peek_code",peek2,2);
  define_c_predicate("nl",nl,0);
  define_c_predicate("nl",nl1,1);
  define_c_predicate("put_code",put,1);
  define_c_predicate("put_code",put2,2);
  define_c_predicate("tab",tab,1);
  define_c_predicate("tab",tab2,2);
  address_skip = define_c_predicate("skip_code",skip,1);
  address_skip2 = define_c_predicate("skip_code",skip2,2);
  define_c_predicate("display",prolog_display,1);
  define_c_predicate("display",prolog_display2,2);
  define_c_predicate("displayq",prolog_displayq,1);
  define_c_predicate("displayq",prolog_displayq2,2);
  define_c_predicate("clearerr",prolog_clearerr,1);

				/* builtin.c */
  
  define_c_predicate("$ddt",set_predtrace,1);

				/* qread.c */

  define_c_predicate("$qread",prolog_qread,2);
  define_c_predicate("$push_qlinfo",push_qlinfo,0);
  define_c_predicate("$pop_qlinfo",pop_qlinfo,0);

				/* misc.c */
  
  define_c_predicate("$set_global_logical_var", set_glv, 2);
  define_c_predicate("$get_global_logical_var", get_glv, 2);
#if defined(ATOMGC)
  define_c_predicate("$erase_atom", prolog_erase_atom, 1);
#endif
  define_c_mod_predicate("system","current_executable",
                         prolog_current_executable,1);
  define_c_predicate("$prompt",prompt,2);
  define_c_predicate("$frozen",frozen,2);
  define_c_predicate("$defrost",defrost,2);
  define_c_predicate("$setarg",setarg,4);
  define_c_predicate("$undo_goal",undo,1);
  define_c_predicate("$metachoice",metachoice,1);
  define_c_predicate("$metacut",metacut,1);
  define_c_predicate("$retry_cut",retry_cut,2);
  define_c_predicate("$unknown",unknown,2);
/* define_c_predicate("$leash_mode",leash_mode,2);*/
/* define_c_predicate("$maxdepth",maxdepth,2); */
/* define_c_predicate("$printdepth",printdepth,2); */
/* define_c_predicate("$breaklevel",breaklevel,2); */
  define_c_predicate("$compiling",compiling,2);
  define_c_predicate("$ferror_flag",ferror_flag,2);
/* define_c_predicate("$single_var_flag",single_var_flag,2); */
/* define_c_predicate("$character_escapes_flag",character_escapes_flag,2); */
/* define_c_predicate("$multiple_flag",redefine_flag,2);*/
  define_c_predicate("$quiet_flag",quiet_flag,2);
  define_c_predicate("$spypoint",spypoint,3);
  define_c_predicate("$debugger_state",debugger_state,2);
  define_c_predicate("$debugger_mode",debugger_mode,0);
  define_c_predicate("$prolog_radix",prolog_radix,2);
  define_c_predicate("$constraint_list",constraint_list,2);
  define_c_predicate("$eq",prolog_eq,2);
  define_c_predicate("$large_data",large_data,3);
  define_c_predicate("$interpreted_clause",prolog_interpreted_clause,2);
#if defined(UNDEFINED)
  define_c_predicate("launch_goal_plus",prolog_launch_goal_plus,3);
#endif

#if defined(UNDEFINED)
  define_c_predicate("launch_goal",prolog_launch_goal,2);
  define_c_predicate("launch_goal_shv",prolog_launch_goal_shv,2);
  define_c_predicate("launch_goal_cont",prolog_launch_goal_cont,3);
#endif
  

  define_c_predicate("eng_call",       prolog_eng_call, 5);
  define_c_predicate("eng_backtrack",  prolog_eng_backtrack, 2);
  define_c_predicate("eng_cut",        prolog_eng_cut, 1);
  define_c_predicate("eng_release",    prolog_release_goal, 1);
  define_c_predicate("eng_wait",       prolog_join_goal, 1);
  define_c_predicate("eng_kill",       prolog_kill_thread, 1);
  define_c_predicate("eng_killothers", prolog_kill_other_threads, 0);
  define_c_predicate("eng_status",     prolog_tasks_status, 0);
  define_c_predicate("eng_self",       prolog_thread_self, 1);
  /* The next one not yet finished */ 
  /*  define_c_predicate("eng_clean"      prolog_eng_clean, 1);*/


#if defined(UNDEFINED)
  define_c_predicate("goal_status",prolog_tasks_status,0);
  define_c_predicate("kill_goal",prolog_kill_thread,1);
  define_c_predicate("kill_other_goals",prolog_kill_other_threads,0);
  define_c_predicate("goal_self",prolog_thread_self,1);
  define_c_predicate("join_goal",prolog_join_goal,1);
  define_c_predicate("backtrack_goal",prolog_backtrack_goal,1);
  define_c_predicate("release_goal",prolog_release_goal,1);
#endif

  define_c_predicate("lock_atom",prolog_lock_atom,1);
  define_c_predicate("unlock_atom",prolog_unlock_atom,1);
  define_c_predicate("atom_lock_state",prolog_lock_atom_state,2);

  define_c_predicate("$unlock_predicate",prolog_unlock_predicate,1);

				/* unix_utils.c */

  define_c_mod_predicate("system","working_directory",prolog_unix_cd,2);
  define_c_mod_predicate("system","pause", prolog_pause, 1);
  define_c_mod_predicate("system","shell",prolog_unix_shell0,0);
  define_c_mod_predicate("system","shell",prolog_unix_shell2,2);
  define_c_mod_predicate("system","system",prolog_unix_system2,2);
  define_c_predicate("$exec",prolog_exec,4);
  define_c_predicate("$unix_argv",prolog_unix_argv,1);
  /*  define_c_predicate("$unix_exit",prolog_unix_exit,1); //) ( (+ */
  define_c_mod_predicate("system","mktemp",prolog_unix_mktemp,2);
  define_c_mod_predicate("system","file_exists",prolog_unix_access,2);
  define_c_mod_predicate("system","directory_files",prolog_directory_files,2);
  define_c_mod_predicate("system","file_properties",prolog_file_properties,6);
  define_c_mod_predicate("system","chmod",prolog_unix_chmod,2);
  define_c_mod_predicate("system","umask",prolog_unix_umask,2);
  define_c_mod_predicate("system","delete_file",prolog_unix_delete,1);
  define_c_mod_predicate("system","current_host",prolog_current_host,1);
  define_c_mod_predicate("system","getenvstr",prolog_getenvstr, 2);
  define_c_mod_predicate("system","get_pid", prolog_getpid, 1);
  define_c_predicate("get_arch", prolog_getarch, 1);
  define_c_predicate("get_os", prolog_getos, 1);

  define_c_predicate("$find_file",prolog_find_file,8); 


                            /* dynlink.c */

  define_c_predicate("dynlink", prolog_dynlink, 2);
  define_c_predicate("dynunlink", prolog_dynunlink, 1); /* JFMC */


                              /* foreign.c */
  /* -- Taken out foreign.c (DCG)
  define_c_predicate("$load_foreign_files",prolog_load_foreign_files,4);
  define_c_predicate("$prepare_foreign_files",prolog_prepare_foreign_files,3);
  define_c_predicate("$foreign_base",prolog_foreign_base,1);
  */
				/* format.c */
  
  define_c_predicate("$format_print_float",prolog_format_print_float,3);
  define_c_predicate("$format_print_integer",prolog_format_print_integer,3);

				/* timing.c */

  define_c_predicate("$runtime",prolog_runtime,1);
  define_c_predicate("$walltime", prolog_walltime, 1);
  define_c_mod_predicate("system","time",prolog_time,1);
  define_c_mod_predicate("system","datime",prolog_datime,9);


				/* stacks.c */

  define_c_predicate("$termheap_usage",termheap_usage,1);
  define_c_predicate("$envstack_usage",envstack_usage,1);
  define_c_predicate("$trail_usage",trail_usage,1);
  define_c_predicate("$choice_usage",choice_usage,1);
  define_c_predicate("$stack_shift_usage",stack_shift_usage,1);

				/* alloc.c */

  define_c_mod_predicate("prolog_sys","statistics",statistics,0);
  define_c_predicate("$program_usage",program_usage,1);
  define_c_predicate("$internal_symbol_usage",internal_symbol_usage,1);
  define_c_predicate("$total_usage",total_usage,1);

                                 /* heapgc.c */

  define_c_predicate("$gc_mode",gc_mode,2);
  define_c_predicate("$gc_trace",gc_trace,2);
  define_c_predicate("$gc_margin",gc_margin,2);
  define_c_predicate("$gc_usage",gc_usage,1);
  define_c_mod_predicate("prolog_sys","garbage_collect",gc_start,0);

				/* nondet.c */
  
  define_c_predicate("repeat",prolog_repeat,0);
  define_c_mod_predicate("prolog_sys","current_atom",current_atom,1);
  define_c_predicate("current_stream",current_stream,3);
  define_c_mod_predicate("prolog_sys","current_predicate",current_predicate,2);
  define_c_predicate("$predicate_property",predicate_property,3);
  define_c_predicate("$current_clauses",current_clauses,2);

#if defined(OLD_DATABASE)
  define_c_predicate("$current_key",current_key,4);
#endif
  define_c_predicate("$first_instance",first_instance,2);
  define_c_predicate("$close_predicate",close_predicate,1);
  define_c_predicate("$open_predicate",open_predicate,1);

#if defined(GAUGE)
  				/* gauge.c */

  define_c_predicate("$emulated_clause_counters",emulated_clause_counters,4);
  define_c_predicate("$counter_values",counter_values,3);
  define_c_predicate("$reset_counters",reset_counters,2);
#endif

  address_nd_repeat = def_retry_c(nd_repeat,0);
  address_nd_current_atom = def_retry_c(nd_current_atom,2);
  address_nd_current_stream = def_retry_c(nd_current_stream,4);
  address_nd_current_predicate = def_retry_c(nd_current_predicate,4);
  address_nd_predicate_property = def_retry_c(nd_predicate_property,5);
  address_nd_atom_concat = def_retry_c(nd_atom_concat,4);

#if defined(STATICENG)
#include "addmodules.c"
#endif  

}

static void deffunction(atom,arity,proc,funcno)
     char *atom;
     CInfo proc;
     int arity, funcno;
{
  TAGGED k = deffunctor(atom,arity);
  struct sw_on_key_node *node = incore_gethash(switch_on_function,k);

  node->key = k;
  node->value.cinfo = builtintab[funcno] = proc;
}

static void deffunction_nobtin(atom,arity,proc)
     char *atom;
     CInfo proc;
     int arity;
{
  TAGGED k = deffunctor(atom,arity);
  struct sw_on_key_node *node = incore_gethash(switch_on_function,k);

  node->key = k;
  node->value.cinfo = proc;
}


static void define_functions()
{
   /* Note: the size of the hash table (64) needs to be expanded when
      new functions are added */

  switch_on_function = new_switch_on_key(64,NULL);

  deffunction("-",1,(CInfo)fu1_minus,0);
  deffunction("+",1,(CInfo)fu1_plus,1);
  deffunction("--",1,(CInfo)fu1_sub1,2); /* shorthand for 'SUB1 FUNCTION' */
  deffunction("++",1,(CInfo)fu1_add1,3); /* shorthand for 'ADD1 FUNCTION' */
  deffunction("integer",1,(CInfo)fu1_integer,4);
  deffunction("truncate",1,(CInfo)fu1_integer,4); /* alias of integer/1 (ISO)*/
  deffunction("float",1,(CInfo)fu1_float,5);
  deffunction("\\",1,(CInfo)fu1_not,6);
  deffunction("+",2,(CInfo)fu2_plus,7);
  deffunction("-",2,(CInfo)fu2_minus,8);
  deffunction("*",2,(CInfo)fu2_times,9);
  deffunction("/",2,(CInfo)fu2_fdivide,10);
  deffunction("//",2,(CInfo)fu2_idivide,11);
  deffunction("rem",2,(CInfo)fu2_rem,12); /* was "mod" (ISO) */
  deffunction("#",2,(CInfo)fu2_xor,13); /* was "^" */
  deffunction("/\\",2,(CInfo)fu2_and,14);
  deffunction("\\/",2,(CInfo)fu2_or,15);
  deffunction("<<",2,(CInfo)fu2_lsh,16);
  deffunction(">>",2,(CInfo)fu2_rsh,17);
  deffunction("mod",2,(CInfo)fu2_mod,18); /* (ISO) */
  deffunction("abs",1,(CInfo)fu1_abs,19); /* (ISO) */
  deffunction("sign",1,(CInfo)fu1_sign,49); /* (ISO) */
  deffunction_nobtin("gcd",2,(CInfo)fu2_gcd);

/* all of these ISO */
  deffunction("float_integer_part",1,(CInfo)fu1_intpart,50);
  deffunction("float_fractional_part",1,(CInfo)fu1_fractpart,51);
  deffunction("floor",1,(CInfo)fu1_floor,52);
  deffunction("round",1,(CInfo)fu1_round,53);
  deffunction("ceiling",1,(CInfo)fu1_ceil,54);
  deffunction("**",2,(CInfo)fu2_pow,55);
  deffunction_nobtin("exp",1,(CInfo)fu1_exp);
  deffunction_nobtin("log",1,(CInfo)fu1_log);
  deffunction_nobtin("sqrt",1,(CInfo)fu1_sqrt);
  deffunction_nobtin("sin",1,(CInfo)fu1_sin);
  deffunction_nobtin("cos",1,(CInfo)fu1_cos);
  deffunction_nobtin("atan",1,(CInfo)fu1_atan);


 /* 41 & 42 are 68 & 79 in SICStus 2.1 */

  deffunction("ARG FUNCTION",2,(CInfo)fu2_arg,41);
  deffunction("COMPARE FUNCTION",2,(CInfo)fu2_compare,42);

}


void init_kanji()
{
  REGISTER int i;
  REGISTER char *cp;

  for (i=0; i<128; i++)		/* default: whitespace */
      symbolchar[i] = 0;
  for (i=128; i<256; i++)
    symbolchar[i] = 1;		/* accept 128..255 as lowercase */
  
  for (cp="abcdefghijklmnopqrstuvwxyz"; (i = *cp++); )
    symbolchar[i]=1;		/* lowercase */
  for (cp="ABCDEFGHIJKLMNOPQRSTUVWXYZ_"; (i = *cp++); )
    symbolchar[i]=2;		/* uppercase */
  for (cp="0123456789"; (i = *cp++); )
    symbolchar[i]=3;		/* digits */
  for (cp="#$&*+-./:<=>?@^\\`~"; (i = *cp++); )
    symbolchar[i]=4;		/* symbolchars */
  for (cp="!;\"'%(),[]{|}"; (i = *cp++); )
    symbolchar[i]=5;		/* punctuation */
}


void init_latin1()
{
  REGISTER int i;

  init_kanji();
  for (i=128; i<161; i++)	/* 128..160 are whitespace */
    symbolchar[i]=0;
  for (i=161; i<192; i++)	/* 161..191 are symbolchars */
    symbolchar[i]=4;
  for (i=192; i<223; i++)	/* 192..222 are uppercase */
    symbolchar[i]=2;
  for (i=223; i<256; i++)	/* 223..255 are lowercase */
    symbolchar[i]=1;
  symbolchar[215]=4;		/* 215 (mult sign) is a symbolchar */
  symbolchar[247]=4;		/* 247 (div sign) is a symbolchar */
}


/* Initializations that need to be made once only. */

void init_streams()
{
  root_stream_ptr = 
    (struct stream_node *)checkalloc(sizeof(struct stream_node)); 
  root_stream_ptr->label=ERRORTAG;
  root_stream_ptr->streamname=ERRORTAG;
  root_stream_ptr->forward=root_stream_ptr;
  root_stream_ptr->backward=root_stream_ptr;
  root_stream_ptr->last_nl_pos = 0;               /* used for tty streams */
  root_stream_ptr->nl_count = 0;
  root_stream_ptr->char_count = 0;

  stream_user_input = new_stream(ERRORTAG, "r", stdin);
  stream_user_output = new_stream(ERRORTAG, "a", stdout);
  stream_user_error = new_stream(ERRORTAG, "a", stderr);
}

void init_locks(){
#if defined(THREADS)
  prolog_predicates_l = &prolog_predicates_st;
  Init_lock(prolog_predicates_l);

#if defined(DEBUG)
  ops_counter_l = &ops_counter_st;
  Init_lock(ops_counter_l);
#endif

  mem_mng_l = &mem_mng_st;
  Init_lock(mem_mng_l);

  launch_goal_l = &launch_goal_st;
  Init_lock(launch_goal_l);

  worker_id_pool_l = &worker_id_pool_st;
  Init_lock(worker_id_pool_l);

  backtrack_goal_l = &backtrack_goal_st;
  Init_lock(backtrack_goal_l);

  init_dynamic_locks();
#endif
}

#if defined(Win32)
extern char library_directory[];
#else
extern char *library_directory;
#endif

void init_once()
{
  REGISTER int i, j;
  REGISTER char *cp;

  /*struct timeval tp;*/

  /* Init time variables */

  stats.lasttime = stats.starttime = usertime();
  stats.startwalltime = walltime();
  stats.lastwalltime = stats.startwalltime;

#if defined(THREADS) && defined(USE_POSIX_THREADS)
  pthread_attr_init(&detached_thread);
  pthread_attr_setdetachstate(&detached_thread, PTHREAD_CREATE_DETACHED);
  pthread_attr_setscope(&detached_thread, PTHREAD_SCOPE_SYSTEM);

  pthread_attr_init(&joinable_thread);
  pthread_attr_setdetachstate(&joinable_thread, PTHREAD_CREATE_JOINABLE);
  pthread_attr_setscope(&joinable_thread, PTHREAD_SCOPE_SYSTEM);
#endif

#if defined(DBG)
  d_init();
#endif

  /*self = (struct worker *)create_wam_storage();*/

  GETENV(i,cp,"ATMTABSIZE",ATMTABSIZE);
  atmtab =
    (struct sw_on_key_node **)checkalloc(i*sizeof(struct sw_on_key_node *));

  prolog_atoms = new_switch_on_key(2*i,NULL);

#if defined(ATOMGC)
  for (j=0; j < i; j++)
    atmtab[j] = NULL;
#endif

 /*  Database initialization */
  prolog_predicates = new_switch_on_key(2,NULL);

  define_functions();                  /* Uses builtintab up to number 17 */
  
  builtintab[20] = bu1_atom;
  builtintab[21] = bu1_atomic;
  builtintab[22] = bu1_float;
  builtintab[23] = bu1_integer;
  builtintab[24] = bu1_nonvar;
  builtintab[25] = bu1_number;
  builtintab[26] = bu1_var;
  builtintab[27] = bu2_lexeq;
  builtintab[28] = bu2_lexne;
  builtintab[29] = bu2_lexlt;
  builtintab[30] = bu2_lexge;
  builtintab[31] = bu2_lexgt;
  builtintab[32] = bu2_lexle;
  builtintab[33] = bu2_numeq;
  builtintab[34] = bu2_numne;
  builtintab[35] = bu2_numlt;
  builtintab[36] = bu2_numge;
  builtintab[37] = bu2_numgt;
  builtintab[38] = bu2_numle;
  builtintab[39] = bu1_if;
  builtintab[40] = bu2_univ;
  /* builtintab[41] = bu3_arg; */
  /* builtintab[42] = bu3_compare; */
  builtintab[43] = bu3_functor;

 	                    /* Attributed variables support, DMCAI, clp.c */
  {
    extern BOOL 
      fu1_type(),
      fu1_get_attribute(),
      bu2_attach_attribute(),
      bu1_detach_attribute(),
      bu2_update_attribute();
    extern TAGGED
      atm_var, atm_attv, atm_float, atm_int, atm_str, atm_atm, atm_lst;
 
    atm_var   = init_atom_check("var");
    atm_attv  = init_atom_check("attv");
    atm_float = init_atom_check("float");
    atm_int   = init_atom_check("integer");
    atm_str   = init_atom_check("structure");
    atm_atm   = init_atom_check("atom");
    atm_lst   = init_atom_check("list"); 
    
    builtintab[44] = fu1_type;                         
    builtintab[45] = fu1_get_attribute;
    builtintab[46] = bu2_attach_attribute;
    builtintab[47] = bu2_update_attribute;
    builtintab[48] = bu1_detach_attribute;
  }

  
#if defined(MARKERS)
  atom_success=init_atom_check("success");
  atom_failure=init_atom_check("failure");
#endif

  atom_share=init_atom_check("share");
  atom_noshare=init_atom_check("noshare");
  atom_user_input=init_atom_check("user_input");
  atom_user_output=init_atom_check("user_output");
  atom_user_error=init_atom_check("user_error");
  atom_read=init_atom_check("read");
  atom_write=init_atom_check("write");
  atom_append=init_atom_check("append");
  atom_socket=init_atom_check("socket");
  atom_symlink=init_atom_check("symlink");
  atom_regular=init_atom_check("regular");
  atom_directory=init_atom_check("directory");
  atom_fifo=init_atom_check("fifo");
  atom_unknown=init_atom_check("unknown");
  atom_user=init_atom_check("user");
  atom_prolog=init_atom_check("prolog");
  atom_lessthan=init_atom_check("<");
  atom_greaterthan=init_atom_check(">");
  atom_equal=init_atom_check("=");
  atom_list = init_atom_check(".");
  atom_nil = init_atom_check("[]");
  atom_on = init_atom_check("on");
  atom_off = init_atom_check("off");
  atom_error = init_atom_check("error");
  atom_trace = init_atom_check("trace");
  atom_debug = init_atom_check("debug");
  atom_fail = init_atom_check("fail");
  atom_all = init_atom_check("all");
  atom_terse = init_atom_check("terse");
  atom_verbose = init_atom_check("verbose");
  atom_compiled = init_atom_check("compiled");
  atom_interpreted = init_atom_check("interpreted");
  atom_builtin = init_atom_check("built_in");
  atom_true = init_atom_check("true");
  atom_retry_hook = init_atom_check("$$retry_hook");

  atom_unprofiled = init_atom_check("unprofiled");
  atom_profiled = init_atom_check("profiled");

  /* atom_public = init_atom_check("public"); */
  atom_concurrent = init_atom_check("concurrent");
  atom_wait = init_atom_check("wait");
  atom_dynamic = init_atom_check("dynamic");
  atom_multifile = init_atom_check("multifile");

  atom_block = init_atom_check("block");
  atom_no_block = init_atom_check("no_block");


  atom_self = init_atom_check("self");
  atom_create = init_atom_check("create");


#if defined(GAUGE)
  atom_counter = init_atom_check("counter");
#endif

  atom_installibdir = init_atom_check(library_directory);

  current_gcmode = atom_on;
  current_gctrace = atom_off;
  current_gcmargin = MakeSmall(500); /* Quintus has 1024 */
  current_unknown = atom_error;
/*   current_leash_mode = MakeSmall(0xf); */
/*   current_maxdepth = MakeSmall(100000); */
/*   current_printdepth = MakeSmall(10); */
  current_compiling = atom_unprofiled;
  current_ferror_flag = atom_on;
/*   current_single_var_flag = atom_on; */
/*   current_character_escapes_flag = atom_off; */
/*   current_redefine_flag = atom_on; */
  current_quiet_flag = atom_off;
  
  init_streams();

  functor_neck = deffunctor(":-",2);
  functor_list = deffunctor(".",2);
  functor_cut = deffunctor("!",0);
  functor_minus = deffunctor("-",2);
  functor_slash = deffunctor("/",2);
  functor_and = deffunctor(",",2);
  functor_functor = deffunctor("functor",1);
  functor_tagged = deffunctor("tagged",1);
  functor_emul_entry = deffunctor("emul_entry",1);
  functor_builtin = deffunctor("builtin",1);
  functor_Dref = deffunctor("$ref",2);
  functor_Dstream = deffunctor("$stream",2);
  functor_Dsetarg = deffunctor("$setarg",4);
  functor_large = deffunctor("large",2);
  functor_long = deffunctor("long",1);

#if defined(INTERNAL_CALLING)
  address_internal_call = define_builtin("user:internal_call",ENTER_UNDEFINED,0);
#endif

  address_interpret_goal = define_builtin("interpret_goal",ENTER_UNDEFINED,2);
  address_call_with_cont = define_builtin("call_with_cont",ENTER_UNDEFINED,1);
  address_interpret_compiled_goal = define_builtin("interpret_compiled_goal",ENTER_UNDEFINED,2);
  address_undefined_goal = define_builtin("undefined_goal",ENTER_UNDEFINED,1);
  address_trace = define_builtin("debug_goal",ENTER_UNDEFINED,1);
  address_help = define_builtin("control_c_handler",ENTER_UNDEFINED,0);
  address_restart = define_builtin("reboot",ENTER_UNDEFINED,0);
  (void) define_builtin("$geler",BUILTIN_GELER,2);
  (void) define_builtin("$instance",BUILTIN_INSTANCE,3);
  (void) define_builtin("dif",BUILTIN_DIF,2);
  (void) define_builtin("$exit",BUILTIN_ABORT,1);
/*
#if !defined(ATTRVARS)
  address_fast_apply = define_builtin("apply",ENTER_UNDEFINED,2);
  address_slow_apply = define_builtin("debug_apply",ENTER_UNDEFINED,2);
#endif
*/
  address_call = define_builtin("call",BUILTIN_CALL,1);
  (void) define_builtin("SYSCALL",BUILTIN_SYSCALL,1);
  (void) define_builtin("$nodebug_call",BUILTIN_NODEBUGCALL,1);
  address_true = define_builtin("true",BUILTIN_TRUE,0);
  address_fail = define_builtin("fail",BUILTIN_FAIL,0);
  address_error = define_builtin("error",ENTER_UNDEFINED,5);

                                          /* Attributed variables support */
  address_pending_unifications = 
    define_builtin("pending_unifications",ENTER_UNDEFINED,1);
  address_uvc = define_builtin("uvc",ENTER_UNDEFINED,2);
  address_ucc = define_builtin("ucc",ENTER_UNDEFINED,2);

  (void) define_builtin("$current_instance",BUILTIN_CURRENT_INSTANCE,5);
  (void) define_builtin("$compile_term",BUILTIN_COMPILE_TERM,2);
/*
#if !defined(ATTRVARS)
  (void) define_builtin("$apply",BUILTIN_APPLY,2);
#endif
*/
  initialize_intrinsics();
#if defined(MARKERS)
  init_markercode();
#endif
  init_worker_entry_table();
}


void glb_init_each_time()
{

/*
#if !defined(ATTRVARS)
  address_apply = address_fast_apply;
#endif
*/

  address_interpret_c_goal = address_interpret_goal;

  current_radix = MakeSmall(10);
  prolog_init_radix();
  /*current_breaklevel = TaggedZero;*/
  current_prompt = init_atom_check("|: ");
  enable_conditions();
  compute_cwd();
}



void local_init_each_time(Arg)
     Argdecl;
{
  REGISTER struct node *b = InitialNode;
		
 /* Debugger state globals moved to per-thread variables because of
    reallocations of the atoms they point to when expanding the heap ---
    this caused the program to break, since a C variable could be pointing
    to a thread's heap and be reallocated by another thread with wrong
    displacements. */

  /* Initialize debugger variables */

  Current_Debugger_State = atom_nil;
  Current_Debugger_Mode = atom_off;

  /* Worker expansion caused by compiling big clauses */

  Expanded_Worker = NULL;

  /* Initialize garbage collection stats */

  Gc_Total_Grey = 0;

  w->node = b;		                    /* set up initial choicepoint */
  b->frame = w->frame = (struct frame *)Stack_Start;

  TopConcChpt = b;           /* Initialize concurrent topmost choicepoint */

  b->next_insn = exitcode;
  b->next_alt = termcode;
  b->local_top = w->local_top = (struct frame *)Offset(w->frame,EToY0);
  b->global_top = w->global_top = Heap_Start;
  b->trail_top = w->trail_top = Trail_Start;
  b->term[0] = atom_nil;
  ChoiceptMarkPure(b);
  ChoiceptMarkStatic(b);
  ChoiceptMarkNoCVA(b);
				
  w->frame->next_insn = NULL;                     /* set up initial frame */
  w->frame->frame = NULL;
  
  w->next_insn = bootcode;
  w->value_trail = InitialValueTrail;
  NewShadowregs(w->global_top);
  w->next_alt = NULL;
				
  init_streams_each_time(w);       /* set misc. variables, handle signals */
  control_c_normal(w);                               /* For threads also? */
}


/* Init. at boot and after abort. */

void init_each_time(Arg)
     Argdecl;
{
  glb_init_each_time();
  local_init_each_time(Arg);
}


void init_streams_each_time(Arg)
     Argdecl;
{
  Input_Stream_Ptr = stream_user_input;
  Output_Stream_Ptr = stream_user_output;
}

/* Cleanup after abort: shrink stacks to initial sizes. */
void reinitialize(Arg)
     Argdecl;
{
  /* ENG_INT prog_mem = mem_prog_count; */ /* preserve over reallocs */
  int i, j;
  char *cp;

  /*predicates_location = NULL;*/	
  wam_initialized = FALSE;                    /* disable recursive aborts */
  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  if ((j=HeapDifference(Heap_Start,Heap_End)) != i)
    Heap_Start = checkrealloc(Heap_Start,j*sizeof(TAGGED),i*sizeof(TAGGED)),
    Heap_End = HeapOffset(Heap_Start,i);
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  if ((j=StackDifference(Stack_Start,Stack_End)) != i)
    Stack_Start = checkrealloc(Stack_Start,j*sizeof(TAGGED),i*sizeof(TAGGED)),
    Stack_End = StackOffset(Stack_Start,i);
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  if ((j=TrailDifference(Trail_Start,Trail_End)) != i)
    Choice_End = Trail_Start =
      checkrealloc(Trail_Start,j*sizeof(TAGGED),i*sizeof(TAGGED)),
    Choice_Start = Trail_End = TrailOffset(Trail_Start,i),
    Tagged_Choice_Start = Choice_Start + TaggedZero;

  if (Atom_Buffer_Length != MAXATOM)
    Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
				       Atom_Buffer_Length, MAXATOM),
    Atom_Buffer_Length = MAXATOM;
  /*predicates_location = &user_predicates;*/ /* re-enable aborts */

  Heap_Warn_Soft = Heap_Warn = HeapOffset(Heap_End,-CALLPAD);
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);
  /*mem_prog_count = prog_mem;*/
  
  empty_gcdef_bin(Arg);

  fflush(stdout);
  fflush(stderr);
  wam_initialized = TRUE;
}

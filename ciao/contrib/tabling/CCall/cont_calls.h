
#include "datadefs.h"
#include "support.h"
#include "task_areas.h"

#include "engine.h"
#include "tries.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --------------------------- */
/*           Defines           */
/* --------------------------- */

#define	TRUE		1
#define	FALSE		0
#define READY 0
#define EVALUATING 1
#define COMPLETE 2
#define MEMSIZE         512*2*2*2*4*4*4



extern tagged_t *tabled_top;

long memory[MEMSIZE*1024];
long *memory_free = &memory[0];
long *memory_end = &memory[MEMSIZE*1024];

TrNode node_top = NULL;
struct EACH_CALL *TOP_SF=NULL;
try_node_t *address_nd_consume_answer_c;
try_node_t *address_nd_resume_ccalls_c;
try_node_t *address_nd_new_ccall_c;

/* --------------------------- */
/*           Macros            */
/* --------------------------- */
#define CHECK_MEM { \
    if (memory_free > memory_end) {\
      panic("memory exhausted - exiting\n");}}



#define DELE_AND_FREE_WAITS(Xwaits)	\
	{ if (Xwaits) { \
		Xwaits = NULL; } }


#define SETMIN(x, y) if (y < x) x = y;

/* --------------------------- */
/*           Structs           */
/* --------------------------- */

struct untrail_list 
{
  ENGINE_Term value;
  struct untrail_list* next;
};

struct EACH_CALL {

  long dfn, plink, comp;

  ENGINE_Term answers;
  ENGINE_Term last_answer;
  ENGINE_Term conts;
  TrNode trie_cont;

  char *name_cont;

  trie_node_t *trie_answers;
  trie_node_t *node_first;
  trie_node_t *node_last;

  long ans_cnt; /*0-FALSE 1-TRUE*/
  struct EACH_CALL *previous;
};


/* --------------------------- */
/*             API             */
/* --------------------------- */


void panic (char *what);
inline void *mem_alloc(int size);
ENGINE_Term my_make_var(goal_descriptor_t *state);
ENGINE_Term my_make_integer(goal_descriptor_t *state, int i);
ENGINE_Term my_make_float(goal_descriptor_t *state, double f);
ENGINE_Term my_make_list(goal_descriptor_t *state, ENGINE_Term head, ENGINE_Term tail);
ENGINE_Term my_make_functor(goal_descriptor_t *state, char* name, int arity, ENGINE_Term *args);

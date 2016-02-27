
#include "datadefs.h"
#include "support.h"
#include "task_areas.h"
#include "wam.h"

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
extern tagged_t *lider_trail;
extern frame_t *stack_freeze;
extern tagged_t *heap_freeze;

struct EACH_CALL *sid_stack[2048];
int isid_stack;

long memory[MEMSIZE*1024];
long *memory_free = &memory[0];
long *memory_end = &memory[MEMSIZE*1024];

TrNode node_top = NULL;
struct EACH_CALL *TOP_SF=NULL;
try_node_t *address_nd_consume_answer_c;
try_node_t *address_nd_resume_cons_c;
try_node_t *address_nd_prueba_c;

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

//struct answer_list{
//  struct subs_factor* sub_fact;
//  struct answer_list* sig;
//};

struct consumer{
  struct subs_factor* sub_fact;
  int ntrail;
  ENGINE_Term *trail;
  frame_t *frame;
  bcp_t next_insn;
  trie_node_t *last_answer;
  struct EACH_CALL *parent_id;
};

struct cons_list{
  struct consumer* consumer;
  struct cons_list* sig;
};

struct EACH_CALL {

  long dfn, plink;                //to implement completion algorithm 
  long comp;                      //state of the call

  struct subs_factor* sub_fact;   //substitution factor of the generator.
//  struct answer_list* answers;  //substitution factor of the answers (frozen on the heap).

  struct cons_list* cons;         //list of consumers
  struct cons_list* last_cons;    //last consumer

  trie_node_t *trie_answers; //To check for repetitions.

  trie_node_t *first_answer; 
  trie_node_t *last_answer;

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

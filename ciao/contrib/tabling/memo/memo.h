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
#define AMEMO ((struct memo_register*)Arg->misc->memo)

struct EACH_CALL *TOP_SF=NULL;
try_node_t *address_nd_consume_answer_c;

/* --------------------------- */
/*           Macros            */
/* --------------------------- */
#define CHECK_MEM { \
    if (memo->memory_free > memo->memory_end) {\
      panic("memory exhausted - exiting\n");}}



#define DELE_AND_FREE_WAITS(Xwaits)	\
	{ if (Xwaits) { \
		Xwaits = NULL; } }


#define SETMIN(x, y) if (y < x) x = y;

/* --------------------------- */
/*           Structs           */
/* --------------------------- */

typedef struct node_list_ node_list;
struct node_list_ {
  trie_node_t *node;
  node_list_t *node_sig;
};

struct EACH_CALL {
  long comp;

  TrNode trie_cont;

  char *name_cont;

  trie_node_t *trie_answers;
  node_list_t *node_list;
  node_list_t *node_last;

  struct EACH_CALL *previous;
};

/* --------------------------- */
/*             API             */
/* --------------------------- */


void panic (char *what);
inline void *mem_alloc(struct memo_register *memo, int size);
ENGINE_Term my_make_var(goal_descriptor_t *state);
ENGINE_Term my_make_integer(goal_descriptor_t *state, int i);
ENGINE_Term my_make_float(goal_descriptor_t *state, double f);
ENGINE_Term my_make_list(goal_descriptor_t *state, ENGINE_Term head, ENGINE_Term tail);
ENGINE_Term my_make_functor(goal_descriptor_t *state, char* name, int arity, ENGINE_Term *args);

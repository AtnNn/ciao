
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



long memory[MEMSIZE*1024];
long *memory_free = &memory[0];
long *memory_end = &memory[MEMSIZE*1024];

TrNode node_top = NULL;
TrNode trie_list = NULL;
struct EACH_CALL *TOP_SF=NULL;
try_node_t *address_nd_consume_answer_c;

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

struct WAIT_ENT {
  struct EACH_CALL *call_id;
  trie_node_t *vars;
  char* cont_pred;
  
  struct WAIT_ENT *next;
};

struct WAIT_TAB {
    struct WAIT_ENT *firstwait;
    struct WAIT_ENT *lastwait;
};

struct EACH_CALL {

  long dfn, plink, comp;

  struct WAIT_TAB *waits;

  trie_node_t *trie_answers;
  trie_node_t *node_first;
  trie_node_t *node_last;
  long ans_cnt; /*0-FALSE 1-TRUE*/
  struct EACH_CALL *previous;
};


tagged_t *id_solution;


/* --------------------------- */
/*             API             */
/* --------------------------- */


void panic (char *what);
void *mymalloc(int size);
void *dyn_mem_alloc(int size);
void *mem_alloc_n(int n);
void *mem_alloc(int size);


struct WAIT_ENT *newwaitent(void);
struct WAIT_TAB *newwaittab(void);
struct EACH_CALL *handlecall(ENGINE_Term Call);
int set_diff_answer_trie(struct EACH_CALL *call,ENGINE_Term Ans);
int complete (struct EACH_CALL *call, struct EACH_CALL *parentcall);

void print_term_yap(ENGINE_Term term);
int print_struct(void);

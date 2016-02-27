/* --------------------------- */
/*           Defines           */
/* --------------------------- */

#define TERM_STACK_SIZE 1000
#define tagged_t unsigned long int


/* --------------------------- */
/*           Structs           */
/* --------------------------- */

struct global_trie_stats {
  int memory_in_use;
  int memory_max_used;
  int nodes_in_use;
  int nodes_max_used;
  int hashes_in_use;
  int hashes_max_used;
  int buckets_in_use;
  int buckets_max_used;
};

#define MEMORY_IN_USE     (GLOBAL_STATS.memory_in_use)
#define MEMORY_MAX_USED   (GLOBAL_STATS.memory_max_used)
#define NODES_IN_USE      (GLOBAL_STATS.nodes_in_use)
#define NODES_MAX_USED    (GLOBAL_STATS.nodes_max_used)
#define HASHES_IN_USE     (GLOBAL_STATS.hashes_in_use)
#define HASHES_MAX_USED   (GLOBAL_STATS.hashes_max_used)
#define BUCKETS_IN_USE    (GLOBAL_STATS.buckets_in_use)
#define BUCKETS_MAX_USED  (GLOBAL_STATS.buckets_max_used)

struct local_trie_stats{
  int entries;
  int nodes;
  int virtual_nodes;
};

#define TRIE_ENTRIES        (LOCAL_STATS.entries)
#define TRIE_NODES          (LOCAL_STATS.nodes)
#define TRIE_VIRTUAL_NODES  (LOCAL_STATS.virtual_nodes)

typedef struct trie_node_ trie_node_t;
struct trie_node_ {
  ENGINE_Term entry;
  trie_node_t *parent;
  trie_node_t *child;
  trie_node_t *next;
#ifdef ALLOW_REMOVE_TRIE
  trie_node_t *previous;
  int hits;
#endif /* ALLOW_REMOVE_TRIE */
};
typedef struct trie_node_t *TrNode;

#define TrNode_entry(X)     ((X)->entry)
#define TrNode_parent(X)    ((X)->parent)
#define TrNode_child(X)     ((X)->child)
#define TrNode_next(X)      ((X)->next)
#define TrNode_previous(X)  ((X)->previous)
#define TrNode_hits(X)      ((X)->hits)

typedef struct trie_hash {
  ENGINE_Term entry;  /* for compatibility with the trie_node data structure */
  int number_of_buckets;
  int number_of_nodes;
  trie_node_t **buckets;
  struct trie_hash *next;
  struct trie_hash *previous;
} *TrHash;

#define TrHash_mark(X)         ((X)->entry)
#define TrHash_num_buckets(X)  ((X)->number_of_buckets)
#define TrHash_seed(X)         ((X)->number_of_buckets - 1)
#define TrHash_num_nodes(X)    ((X)->number_of_nodes)
#define TrHash_buckets(X)      ((X)->buckets)
#define TrHash_bucket(X,N)     ((X)->buckets + N)
#define TrHash_next(X)         ((X)->next)
#define TrHash_previous(X)     ((X)->previous)

#define TYPE_TR_NODE      trie_node_t
#define TYPE_TR_HASH      struct trie_hash
#define SIZEOF_TR_NODE    sizeof(TYPE_TR_NODE)
#define SIZEOF_TR_HASH    sizeof(TYPE_TR_HASH)
#define SIZEOF_TR_BUCKET  sizeof(TYPE_TR_NODE *)

#define AS_TR_NODE_NEXT(ADDRESS) (TrNode)((int)(ADDRESS) - sizeof(ENGINE_Term) - 2 * sizeof(trie_node_t *))
#define AS_TR_HASH_NEXT(ADDRESS) (TrHash)((int)(ADDRESS) - sizeof(ENGINE_Term) - 2 * sizeof(int) - sizeof(trie_node_t **))



/* --------------------------- */
/*           Macros            */
/* --------------------------- */

#define MkTrieVar(INDEX)          ((INDEX))

#define HASH_MARK                 ((ENGINE_Term) MkTrieVar(TERM_STACK_SIZE))
#define BASE_HASH_BUCKETS         64
#define MAX_NODES_PER_TRIE_LEVEL  8
#define MAX_NODES_PER_BUCKET      (MAX_NODES_PER_TRIE_LEVEL / 2)
#define IS_TRIE_HASH(NODE)        (TrHash_mark(NODE) == HASH_MARK)
#define HASH_TERM(TERM, SEED)     (((TERM) >> 4) & (SEED))

#define STACK_NOT_EMPTY(STACK, STACK_BASE) STACK != STACK_BASE
#define POP_UP(STACK)                      *--STACK
#define POP_DOWN(STACK)                    *++STACK
#define PUSH_UP(STACK, ITEM, STACK_TOP)                              \
        { if (STACK < STACK_TOP)                                     \
            fprintf(stderr, "\nTries module: TERM_STACK full");      \
          *STACK = (ENGINE_Term)(ITEM);                        \
          STACK--;                                                   \
        }
#define PUSH_DOWN(STACK, ITEM, STACK_TOP)                            \
        { if (STACK > STACK_TOP)                                     \
            fprintf(stderr, "\nTries module: TERM_STACK full");      \
          *STACK = (ENGINE_Term)(ITEM);                        \
          STACK++;                                                   \
	}



#define new_struct(STR, STR_TYPE, STR_SIZE)                          \
        STR = (STR_TYPE *) ENGINE_AllocSpaceFromYap(memo,STR_SIZE)
#define free_struct(STR)                                             \
        ENGINE_FreeSpaceFromYap((char *) (STR))
#define free_trie_node(STR)                                          \
        free_struct(STR);                                            \
        STATS_node_dec()
#define free_hash_buckets(STR, NUM_BUCKETS)                          \
        free_struct(STR);                                            \
        STATS_buckets_dec(NUM_BUCKETS)
#define free_trie_hash(STR)                                          \
        free_struct(STR);                                            \
        STATS_hash_dec()



#ifdef ALLOW_REMOVE_TRIE
#define TrNode_allow_remove_trie(TR_NODE, PREVIOUS)                  \
        TrNode_previous(TR_NODE) = PREVIOUS;                         \
        TrNode_hits(TR_NODE) = 0
#else
#define TrNode_allow_remove_trie(TR_NODE, PREVIOUS)
#endif /* ALLOW_REMOVE_TRIE */

#define new_trie_node(TR_NODE, ENTRY, PARENT, CHILD, NEXT, PREVIOUS) \
        new_struct(TR_NODE, TYPE_TR_NODE, SIZEOF_TR_NODE);           \
        TrNode_entry(TR_NODE) = ENTRY;                               \
        TrNode_parent(TR_NODE) = PARENT;                             \
        TrNode_child(TR_NODE) = CHILD;                               \
        TrNode_next(TR_NODE) = NEXT;                                 \
        TrNode_allow_remove_trie(TR_NODE, PREVIOUS);                 \
        STATS_node_inc()
#define new_hash_buckets(TR_HASH, NUM_BUCKETS)                       \
        { int i; void **ptr;                                         \
          new_struct(ptr, void *, NUM_BUCKETS * sizeof(void *));     \
          TrHash_buckets(TR_HASH) = (TYPE_TR_NODE **) ptr;           \
          for (i = NUM_BUCKETS; i != 0; i--)                         \
            *ptr++ = NULL;                                           \
          STATS_buckets_inc(NUM_BUCKETS);                            \
        }
#define new_trie_hash(TR_HASH, NUM_NODES)                            \
        new_struct(TR_HASH, TYPE_TR_HASH, SIZEOF_TR_HASH);           \
        TrHash_mark(TR_HASH) = HASH_MARK;                            \
        TrHash_num_buckets(TR_HASH) = BASE_HASH_BUCKETS;             \
        new_hash_buckets(TR_HASH, BASE_HASH_BUCKETS);                \
        TrHash_num_nodes(TR_HASH) = NUM_NODES;                       \
	TrHash_next(TR_HASH) = HASHES;                               \
        TrHash_previous(TR_HASH) = AS_TR_HASH_NEXT(&HASHES);         \
        if (HASHES)                                                  \
	  TrHash_previous(HASHES) = TR_HASH;                         \
        HASHES = TR_HASH;                                            \
        STATS_hash_inc()

#define STATS_node_inc()                                             \
        NODES_IN_USE++;                                              \
        if (NODES_IN_USE > NODES_MAX_USED)                           \
          NODES_MAX_USED = NODES_IN_USE;                             \
        MEMORY_IN_USE += SIZEOF_TR_NODE;                             \
        if (MEMORY_IN_USE > MEMORY_MAX_USED)                         \
          MEMORY_MAX_USED = MEMORY_IN_USE
#define STATS_node_dec()                                             \
        NODES_IN_USE--;                                              \
        MEMORY_IN_USE -= SIZEOF_TR_NODE
#define STATS_hash_inc()                                             \
        HASHES_IN_USE++;                                             \
        if (HASHES_IN_USE > HASHES_MAX_USED)                         \
          HASHES_MAX_USED = HASHES_IN_USE;                           \
        MEMORY_IN_USE += SIZEOF_TR_HASH;                             \
        if (MEMORY_IN_USE > MEMORY_MAX_USED)                         \
          MEMORY_MAX_USED = MEMORY_IN_USE
#define STATS_hash_dec()                                             \
        HASHES_IN_USE--;                                             \
        MEMORY_IN_USE -= SIZEOF_TR_HASH
#define STATS_buckets_inc(N)                                         \
        BUCKETS_IN_USE += N;                                         \
        if (BUCKETS_IN_USE > BUCKETS_MAX_USED)                       \
          BUCKETS_MAX_USED = BUCKETS_IN_USE;                         \
        MEMORY_IN_USE += (N) * SIZEOF_TR_BUCKET;                     \
        if (MEMORY_IN_USE > MEMORY_MAX_USED)                         \
          MEMORY_MAX_USED = MEMORY_IN_USE
#define STATS_buckets_dec(N)                                         \
        BUCKETS_IN_USE -= N;                                         \
        MEMORY_IN_USE -= (N) * SIZEOF_TR_BUCKET



/* --------------------------- */
/*             API             */
/* --------------------------- */

void init_tries_module(void);
TrNode open_trie(struct memo_register *memo);
void close_trie(TrNode node);
void close_all_tries(void);
TrNode put_trie_entry(struct memo_register *memo, TrNode node, ENGINE_Term entry);
ENGINE_Term get_trie_entry(worker_t *w, TrNode node);
void remove_trie_entry(TrNode node);
void trie_stats(int *nodes, int *hashes, int *buckets, int *memory);
void trie_max_stats(int *nodes, int *hashes, int *buckets, int *memory);
void trie_usage(TrNode node, int *entries, int *nodes, int *virtual_nodes);
void print_trie(TrNode node);

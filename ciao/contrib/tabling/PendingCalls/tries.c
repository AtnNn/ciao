/* -------------------------- */
/*          Includes          */
/* -------------------------- */


#include <stdio.h>
#include <string.h>

#include <termdefs.h>


/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

static TrNode put_trie(TrNode node, ENGINE_Term entry);
static ENGINE_Term get_trie(worker_t *w, TrNode node, ENGINE_Term *stack_list, TrNode *cur_node);
static void free_child_nodes(TrNode node);
static void traverse_trie_usage(TrNode node, int depth);



/* -------------------------- */
/*       Local Variables      */
/* -------------------------- */

static struct global_trie_stats GLOBAL_STATS;
static struct local_trie_stats LOCAL_STATS;
static int MODE;
static TrNode TRIES;
static TrHash HASHES;
static ENGINE_Term TERM_STACK[TERM_STACK_SIZE];
static ENGINE_Term *stack_args, *stack_args_base, *stack_vars, *stack_vars_base;
static int max_index;



/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

static inline
TrNode trie_node_check_insert(TrNode parent, ENGINE_Term t, int is_cte) {
  TrNode child;

  child = TrNode_child(parent);
  if (child == NULL) {
    new_trie_node(child, t, parent, NULL, NULL, NULL);
    if ((t & 0xe0000000) == 0xe0000000 && !is_cte)
      {
	child->arity = ENGINE_ArityOfFunctor(t);
	child->name = (char*) malloc ((strlen(ENGINE_NameOfFunctor(t)) + 1)*sizeof(char));
	strcpy(child->name,ENGINE_NameOfFunctor(t));
      }
    TrNode_child(parent) = child;
    return child;
  } else if (! IS_TRIE_HASH(child)) {
    int count = 0;
    do {
      if (is_cte)
	{
	  if (TrNode_entry(child) == t) return child;
	}
      else 
	{ 
	  if ((t & 0xe0000000) == 0xe0000000)
	    {
	      if (((TrNode_entry(child) & 0xe0000000) == 0xe0000000) && 
		  (TrNode_arity(child) == ENGINE_ArityOfFunctor(t)) && 
		  !strcmp(TrNode_name(child),ENGINE_NameOfFunctor(t)))
		{
		  return child;
		}
	    }
	  else if ((TrNode_entry(child) | 3) == (t | 3)) 
	    {
	    return child;
	    }
	}
      count++;
      child = TrNode_next(child);
    } while (child);
    new_trie_node(child, t, parent, NULL, TrNode_child(parent), NULL);
    if ((t & 0xe0000000) == 0xe0000000 && !is_cte)
      {
	child->arity = ENGINE_ArityOfFunctor(t);
	child->name = (char*) malloc ((strlen(ENGINE_NameOfFunctor(t)) + 1)*sizeof(char));
	strcpy(child->name,ENGINE_NameOfFunctor(t));
      }
#ifdef ALLOW_REMOVE_TRIE
    TrNode_previous(TrNode_child(parent)) = child;
#endif /* ALLOW_REMOVE_TRIE */
    if (++count > MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new trie hash */
      TrHash hash;
      TrNode chain, next, *bucket;
      new_trie_hash(hash, count);
      chain = child;
      do {
        bucket = TrHash_bucket(hash, HASH_TERM(TrNode_entry(chain), BASE_HASH_BUCKETS - 1));
        next = TrNode_next(chain);
        TrNode_next(chain) = *bucket;
#ifdef ALLOW_REMOVE_TRIE
        TrNode_previous(chain) = AS_TR_NODE_NEXT(bucket);
	if (*bucket)
	  TrNode_previous(*bucket) = chain;
#endif /* ALLOW_REMOVE_TRIE */
        *bucket = chain;
        chain = next;
      } while (chain);
      TrNode_child(parent) = (TrNode) hash;
    } else {
      TrNode_child(parent) = child;
    }
    return child;
  } else {
    /* is trie hash */
    TrHash hash;
    TrNode *bucket;
    int count;
    hash = (TrHash) child;
    bucket = TrHash_bucket(hash, HASH_TERM(t, TrHash_seed(hash)));
    child = *bucket;
    count = 0;
    while (child) 
      {
      if (is_cte) { if (TrNode_entry(child) == t) return child;	}
      else 
	{ 
	  if ((t & 0xe0000000) == 0xe0000000)
	    {
	      if (((TrNode_entry(child) & 0xe0000000) == 0xe0000000) && 
		  (TrNode_arity(child) == ENGINE_ArityOfFunctor(t)) && 
		  !strcmp(TrNode_name(child),ENGINE_NameOfFunctor(t)))
		{
		  return child;
		}
	    }
	  else if ((TrNode_entry(child) | 3) == (t | 3)) 
	    {
	    return child;
	    }
	}
	count++;
	child = TrNode_next(child);
      } while (child);
    TrHash_num_nodes(hash)++;
    new_trie_node(child, t, parent, NULL, *bucket, AS_TR_NODE_NEXT(bucket));
    if ((t & 0xe0000000) == 0xe0000000 && !is_cte)
      {
	child->arity = ENGINE_ArityOfFunctor(t);
	child->name = (char*) malloc ((strlen(ENGINE_NameOfFunctor(t)) + 1)*sizeof(char));
	strcpy(child->name,ENGINE_NameOfFunctor(t));
      }

#ifdef ALLOW_REMOVE_TRIE
    if (*bucket)
      TrNode_previous(*bucket) = child;
#endif /* ALLOW_REMOVE_TRIE */
    *bucket = child;
    if (count > MAX_NODES_PER_BUCKET && TrHash_num_nodes(hash) > TrHash_num_buckets(hash)) {
      /* expand trie hash */
      TrNode chain, next, *first_bucket, *new_bucket;
      int seed;
      first_bucket = TrHash_buckets(hash);
      bucket = first_bucket + TrHash_num_buckets(hash);
      TrHash_num_buckets(hash) *= 2;
      new_hash_buckets(hash, TrHash_num_buckets(hash)); 
      seed = TrHash_num_buckets(hash) - 1;
      do {
        if (*--bucket) {
          chain = *bucket;
          do {
            new_bucket = TrHash_bucket(hash, HASH_TERM(TrNode_entry(chain), seed));
            next = TrNode_next(chain);
            TrNode_next(chain) = *new_bucket;
#ifdef ALLOW_REMOVE_TRIE
	    TrNode_previous(chain) = AS_TR_NODE_NEXT(bucket);
	    if (*new_bucket)
	      TrNode_previous(*new_bucket) = chain;
#endif /* ALLOW_REMOVE_TRIE */
            *new_bucket = chain;
            chain = next;
          } while (chain);
        }
      } while (bucket != first_bucket);
      /*
	ARREGÃAR ESTO
	free_hash_buckets(first_bucket, TrHash_num_buckets(hash) / 2);
      */
    }
    return child;
  }
}



/* -------------------------- */
/*            API             */     
/* -------------------------- */

void init_tries_module(void) 
{
  MODE = MODE_STANDARD;
  TRIES = NULL;
  HASHES = NULL;

  MEMORY_IN_USE = 0;
  MEMORY_MAX_USED = 0;
  NODES_IN_USE = 0;
  NODES_MAX_USED = 0;
  HASHES_IN_USE = 0;
  HASHES_MAX_USED = 0;
  BUCKETS_IN_USE = 0;
  BUCKETS_MAX_USED = 0;

  return;
}


TrNode open_trie(void) 
{
  TrNode new_node;

  new_trie_node(new_node, 0, NULL, NULL, TRIES, AS_TR_NODE_NEXT(&TRIES));
#ifdef ALLOW_REMOVE_TRIE
  TrNode_hits(new_node)++;
  if (TRIES)
    TrNode_previous(TRIES) = new_node;
#endif /* ALLOW_REMOVE_TRIE */
  TRIES = new_node;
  return new_node;
}


void close_trie(TrNode node) 
{
  if (TrNode_parent(node))
    fprintf(stderr,"\nTries module: invalid top level node\n");
  if (TrNode_child(node))
    free_child_nodes(TrNode_child(node));
#ifdef ALLOW_REMOVE_TRIE
  if (TrNode_next(node)) {
    TrNode_previous(TrNode_next(node)) = TrNode_previous(node);
    TrNode_next(TrNode_previous(node)) = TrNode_next(node);
  } else
    TrNode_next(TrNode_previous(node)) = NULL;
  free_trie_node(node);  
#else
  TrNode_child(node) = NULL;
#endif /* ALLOW_REMOVE_TRIE */
  return;
}


void close_all_tries(void) 
{
  TrNode aux_node;
  while (TRIES) {
    if (TrNode_child(TRIES))
      free_child_nodes(TrNode_child(TRIES));
    aux_node = TrNode_next(TRIES);
    free_trie_node(TRIES);
    TRIES = aux_node;
  }
  return;
}


TrNode put_trie_entry(TrNode node, ENGINE_Term entry, int mode) 
{
  MODE = mode;
  stack_args_base = stack_args = TERM_STACK;
  stack_vars_base = stack_vars = TERM_STACK + TERM_STACK_SIZE - 1;

  node = put_trie(node, entry);

  while (STACK_NOT_EMPTY(stack_vars++, stack_vars_base)) {
    POP_DOWN(stack_vars);
    CTagToPointer(*stack_vars) = *stack_vars;
  }

#ifdef ALLOW_REMOVE_TRIE
  TrNode_hits(node)++;
#endif /* ALLOW_REMOVE_TRIE */
  return node;
}


ENGINE_Term get_trie_entry(worker_t *Arg, TrNode node, int mode) 
{
  MODE = mode;
  stack_vars_base = stack_vars = TERM_STACK;
  stack_args_base = stack_args = TERM_STACK + TERM_STACK_SIZE - 1;
  max_index = -1;

  return get_trie(Arg, node, stack_args, &node);
}


#ifdef ALLOW_REMOVE_TRIE
void remove_trie_entry(TrNode node) 
{
  TrNode parent;

  TrNode_hits(node)--;
  if (TrNode_child(node))
    return;

  while (TrNode_hits(node) == 0) {
    parent = TrNode_parent(node);
    if (TrNode_previous(node)) {
      if (IS_TRIE_HASH(TrNode_child(parent))) {
	TrHash hash = (TrHash) TrNode_child(parent);
	TrHash_num_nodes(hash)--;
	if (TrHash_num_nodes(hash)) {
	  if (TrNode_next(node)) {
	    TrNode_next(TrNode_previous(node)) = TrNode_next(node);
	    TrNode_previous(TrNode_next(node)) = TrNode_previous(node);
	  } else {
	    TrNode_next(TrNode_previous(node)) = NULL;
	  }
	  free_trie_node(node);
	  return;
	}
	if (TrHash_next(hash)) {
	  TrHash_previous(TrHash_next(hash)) = TrHash_previous(hash);
	  TrHash_next(TrHash_previous(hash)) = TrHash_next(hash);
	} else 
	  TrHash_next(TrHash_previous(hash)) = NULL;
	free_hash_buckets(TrHash_buckets(hash), TrHash_num_buckets(hash));
	free_trie_hash(hash);
      } else {
	if (TrNode_next(node)) {
	  TrNode_next(TrNode_previous(node)) = TrNode_next(node);
	  TrNode_previous(TrNode_next(node)) = TrNode_previous(node);
	} else {
	  TrNode_next(TrNode_previous(node)) = NULL;
	}
	free_trie_node(node);
	return;
      }
    } else if (TrNode_next(node)) {
      TrNode_child(parent) = TrNode_next(node);
      TrNode_previous(TrNode_next(node)) = NULL;
      free_trie_node(node);
      return;
    }
    free_trie_node(node);
    node = parent;
  }

  TrNode_child(node) = NULL;
  return;
}
#endif /* ALLOW_REMOVE_TRIE */


void trie_stats(int *nodes, int *hashes, int *buckets, int *memory) 
{
  *nodes = NODES_IN_USE;
  *hashes = HASHES_IN_USE;
  *buckets = BUCKETS_IN_USE;
  *memory = MEMORY_IN_USE;
}


void trie_max_stats(int *nodes, int *hashes, int *buckets, int *memory) 
{
  *nodes = NODES_MAX_USED;
  *hashes = HASHES_MAX_USED;
  *buckets = BUCKETS_MAX_USED;
  *memory = MEMORY_MAX_USED;
}


void trie_usage(TrNode node, int *entries, int *nodes, int *virtual_nodes) 
{
  TRIE_ENTRIES = 0;
  TRIE_NODES = 0;
  TRIE_VIRTUAL_NODES = 0;
  if (TrNode_child(node))
    traverse_trie_usage(TrNode_child(node), 0);
  *entries = TRIE_ENTRIES;
  *nodes = TRIE_NODES;
  *virtual_nodes = TRIE_VIRTUAL_NODES;
  return;
}



/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

static
TrNode put_trie(TrNode node, ENGINE_Term entry) 
{
  ENGINE_Term t;
  DEREF(t,entry);
  if (IsTrieVar(t)) {
    node = trie_node_check_insert(node, t & 0x0FFFFFFF, 0);
  } else if (ENGINE_IsVarTerm(t)) {
    node = trie_node_check_insert(node, (stack_vars_base - stack_vars) << 1, 0);
      PUSH_UP(stack_vars, t, stack_args);
      t = 0x60000000 | ((stack_vars_base - stack_vars) << 1);
      PUSH_UP(stack_vars, stack_vars, stack_args);
  } else if (ENGINE_IsAtomTerm(t)) {
    node = trie_node_check_insert(node, t, 0);
  } else if (ENGINE_IsIntTerm(t)) {
    node = trie_node_check_insert(node, t, 0);
  } else if (ENGINE_IsFloatTerm(t)) {
    double f;
    volatile ENGINE_Term *p;
    f = ENGINE_FloatOfTerm(t);
    p = (ENGINE_Term *)((void *) &f); /* to avoid gcc warning */
    node = trie_node_check_insert(node, ENGINE_FloatInitTag, 1);
    node = trie_node_check_insert(node, *p, 1);
    node = trie_node_check_insert(node, *(p + 1), 1);
    node = trie_node_check_insert(node, ENGINE_FloatEndTag, 1);
  } else if (ENGINE_IsPairTerm(t)) {
    node = trie_node_check_insert(node, ENGINE_PairInitTag, 1);
    if (MODE == MODE_STANDARD) {
      do {
	node = put_trie(node, ENGINE_HeadOfTerm(t));
	DEREF(t,ENGINE_TailOfTerm(t));
      } while (ENGINE_IsPairTerm(t));
    } else { /* MODE_REVERSE */
      ENGINE_Term *stack_list = stack_args;
      do {
	PUSH_DOWN(stack_args, ENGINE_HeadOfTerm(t), stack_vars);
	DEREF(t,ENGINE_TailOfTerm(t));
      } while (ENGINE_IsPairTerm(t));
      while (STACK_NOT_EMPTY(stack_args, stack_list))
	node = put_trie(node, POP_UP(stack_args));
    }
    node = trie_node_check_insert(node, ENGINE_PairEndTag, 1);
  } else if (ENGINE_IsApplTerm(t)) {
    if (!strcmp(ENGINE_NameOfFunctor(t),",") && ENGINE_ArityOfFunctor(t)  == 2) 
      {
	node = trie_node_check_insert(node, ENGINE_CommaInitTag, 1);
	do 
	  {
	    node = put_trie(node, ENGINE_ArgOfTerm(1, t));
	    DEREF(t,ENGINE_ArgOfTerm(2, t));
	  } while (ENGINE_IsApplTerm(t) && !strcmp(ENGINE_NameOfFunctor(t),",") && ENGINE_ArityOfFunctor(t)  == 2);
	node = put_trie(node, t);
	node = trie_node_check_insert(node, ENGINE_CommaEndTag, 1);
      } 
    else 
      {
	int i;
	node = trie_node_check_insert(node, t, 0);
	for (i = 1; i <= ENGINE_ArityOfFunctor(t); i++)
	  {
	    node = put_trie(node, ENGINE_ArgOfTerm(i, t));
	  }
      }
  } else fprintf(stderr, "\nTries module: unknown type tag\n");
  
  return node;
}


static
ENGINE_Term get_trie(worker_t *Arg, TrNode node, ENGINE_Term *stack_mark, TrNode *cur_node) 
{
  ENGINE_Term t;

  while (TrNode_parent(node)) {
    t = TrNode_entry(node);
    DEREF(t,t); 
    if (ENGINE_IsVarTerm(t)) {
      int index = TrieVarIndex(t);
      if (index > max_index) {
	int i;
	stack_vars = &stack_vars_base[index + 1];
        if (stack_vars > stack_args + 1)
	  fprintf(stderr, "\nTries module: TERM_STACK full");
	for (i = index; i > max_index; i--)
	  stack_vars_base[i] = 0;
	max_index = index;
      }
      if (stack_vars_base[index]) {
	t = stack_vars_base[index];
      } else {
	t = ENGINE_MkVarTerm();
	stack_vars_base[index] = t;
      }
      PUSH_UP(stack_args, t, stack_vars);
    } else if (t == ENGINE_FloatEndTag) {
	volatile double f;
	volatile ENGINE_Term *p;
	p = (ENGINE_Term *)((void *) &f); /* to avoid gcc warning */
	node = TrNode_parent(node);
	*(p + 1) = TrNode_entry(node);
	node = TrNode_parent(node);
	*p = TrNode_entry(node);
	node = TrNode_parent(node); /* ignore ENGINE_FloatInitTag */
	t = ENGINE_MkFloatTerm(f);
	PUSH_UP(stack_args, t, stack_vars);
    } else if (t == ENGINE_FloatInitTag) {
    } else if (t == ENGINE_CommaEndTag) {
         node = TrNode_parent(node);
	 t = get_trie(Arg, node, stack_args, &node);
	 PUSH_UP(stack_args, t, stack_vars);
    } else if (t == ENGINE_CommaInitTag) {
	ENGINE_Term *stack_aux = stack_mark;
	stack_aux--;
	while (STACK_NOT_EMPTY(stack_aux, stack_args)) {
	  t = ENGINE_MkApplTerm(",", 2, stack_aux);
	  *stack_aux = t;
	  stack_aux--;
	}
	stack_args = stack_mark;
      	*cur_node = node;
	return t;
    } else if (ENGINE_IsAtomTerm(t)) {
      PUSH_UP(stack_args, t, stack_vars);
    } else if (ENGINE_IsIntTerm(t)) {
      PUSH_UP(stack_args, t, stack_vars);
    } else if (ENGINE_IsPairTerm(t)) {
      if (t == ENGINE_PairEndTag) {
	if (MODE == MODE_STANDARD) {
	  t = ENGINE_MkAtomTerm("[]");
	  PUSH_UP(stack_args, t, stack_vars);
	  node = TrNode_parent(node);
	  t = get_trie(Arg, node, &stack_args[1], &node);
	} else { /* MODE_REVERSE */
	  node = TrNode_parent(node);
	  t = get_trie(Arg, node, stack_args, &node);
	}
	PUSH_UP(stack_args, t, stack_vars);
      } else if (t == ENGINE_PairInitTag) {
	ENGINE_Term t2;
	if (MODE == MODE_STANDARD) {
	  ENGINE_Term *stack_aux = stack_mark;
	  t = *stack_aux--;
	  while (STACK_NOT_EMPTY(stack_aux, stack_args)) {
	    t2 = *stack_aux--;
	    t = ENGINE_MkPairTerm(t2, t);
	    }
	  stack_args = stack_mark;
	} else { /* MODE_REVERSE */
	  t = ENGINE_MkAtomTerm("[]");
	  while (STACK_NOT_EMPTY(stack_args, stack_mark)) {
	    t2 = POP_DOWN(stack_args);
	    t = ENGINE_MkPairTerm(t2, t);
	  }
	}
	*cur_node = node;
	return t;
      }

    } else if (ENGINE_IsApplTerm(t)) {
      int arity = TrNode_arity(node);
      t = ENGINE_MkApplTerm(TrNode_name(node), arity, stack_args + 1);
      stack_args += arity;
      PUSH_UP(stack_args, t, stack_vars);
    } else
      fprintf(stderr, "\nTries module: unknown type tag\n");
    node = TrNode_parent(node);
  }
  *cur_node = node;
  return t;
}


static
void free_child_nodes(TrNode node) {
  if (IS_TRIE_HASH(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash) node;
    if (TrHash_next(hash)) {
      TrHash_previous(TrHash_next(hash)) = TrHash_previous(hash);
      TrHash_next(TrHash_previous(hash)) = TrHash_next(hash);
    } else 
      TrHash_next(TrHash_previous(hash)) = NULL;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if (*--bucket)
	free_child_nodes(*bucket);
    } while (bucket != first_bucket);
    free_hash_buckets(first_bucket, TrHash_num_buckets(hash));
    free_trie_hash(hash);
    return;
  }
  if (TrNode_next(node))
    free_child_nodes(TrNode_next(node));
  if (TrNode_child(node))
    free_child_nodes(TrNode_child(node));
  free_trie_node(node);
  return;
}


static
void traverse_trie_usage(TrNode node, int depth) {
  if (IS_TRIE_HASH(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash;
    hash = (TrHash) node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if (*--bucket) {
        node = *bucket;
        traverse_trie_usage(node, depth);
      }
    } while (bucket != first_bucket);
    return;
  }

  TRIE_NODES++;
  if (TrNode_next(node))
    traverse_trie_usage(TrNode_next(node), depth);
  depth++;
  if (TrNode_child(node)) {
    traverse_trie_usage(TrNode_child(node), depth);
  } else {
    TRIE_ENTRIES++;
    TRIE_VIRTUAL_NODES+= depth;
  }
  return;
}

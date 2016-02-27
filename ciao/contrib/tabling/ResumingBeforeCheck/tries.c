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
static TrNode TRIES;
static TrHash HASHES;
static ENGINE_Term TERM_STACK[TERM_STACK_SIZE];
static ENGINE_Term *stack_args, *stack_args_base, *stack_vars, *stack_vars_base;
static int max_index;



/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

static inline
TrNode trie_node_check_insert(TrNode parent, ENGINE_Term t) {
  TrNode child;

  child = TrNode_child(parent);
  if (child == NULL) 
    {
      new_trie_node(child, t, parent, NULL, NULL, NULL);
      TrNode_child(parent) = child;
      //      printf("\nNodo %p tiene valor %p\n",child,child->entry);
      return child;
    } else if (! IS_TRIE_HASH(child)) 
      {
	int count = 0;
	do 
	  {
	    if (TrNode_entry(child) == t) return child;
	    count++;
	    child = TrNode_next(child);
	  } 
	while (child);
	new_trie_node(child, t, parent, NULL, TrNode_child(parent), NULL);
#ifdef ALLOW_REMOVE_TRIE
	TrNode_previous(TrNode_child(parent)) = child;
#endif // ALLOW_REMOVE_TRIE 
	if (++count > MAX_NODES_PER_TRIE_LEVEL) 
	  {
	    // alloc a new trie hash
	    TrHash hash;
	    TrNode chain, next, *bucket;
	    new_trie_hash(hash, count);
	    chain = child;
	    do 
	      {
		bucket = TrHash_bucket(hash, HASH_TERM(TrNode_entry(chain), BASE_HASH_BUCKETS - 1));
		next = TrNode_next(chain);
		TrNode_next(chain) = *bucket;
#ifdef ALLOW_REMOVE_TRIE
		TrNode_previous(chain) = AS_TR_NODE_NEXT(bucket);
		if (*bucket) TrNode_previous(*bucket) = chain;
#endif // ALLOW_REMOVE_TRIE 
		*bucket = chain;
		chain = next;
	      } 
	    while (chain);
	    TrNode_child(parent) = (TrNode) hash;
	  } else 
	    {
	      TrNode_child(parent) = child;
	    }
	return child;
      } else 
	{
	  // is trie hash 
	  TrHash hash;
	  TrNode *bucket;
	  int count;
	  hash = (TrHash) child;
	  bucket = TrHash_bucket(hash, HASH_TERM(t, TrHash_seed(hash)));
	  child = *bucket;
	  count = 0;
	  while (child) 
	    {
	      if (TrNode_entry(child) == t) return child;
	      count++;
	      child = TrNode_next(child);
	    } 
	  while (child);
	  TrHash_num_nodes(hash)++;
	  new_trie_node(child, t, parent, NULL, *bucket, AS_TR_NODE_NEXT(bucket));
#ifdef ALLOW_REMOVE_TRIE
	  if (*bucket) TrNode_previous(*bucket) = child;
#endif // ALLOW_REMOVE_TRIE 
	  *bucket = child;
	  if (count > MAX_NODES_PER_BUCKET && TrHash_num_nodes(hash) > TrHash_num_buckets(hash)) 
	    {
	      // expand trie hash 
	      TrNode chain, next, *first_bucket, *new_bucket;
	      int seed;
	      first_bucket = TrHash_buckets(hash);
	      bucket = first_bucket + TrHash_num_buckets(hash);
	      TrHash_num_buckets(hash) *= 2;
	      new_hash_buckets(hash, TrHash_num_buckets(hash)); 
	      seed = TrHash_num_buckets(hash) - 1;
	      do 
		{
		  if (*--bucket) 
		    {
		      chain = *bucket;
		      do 
			{
			  new_bucket = TrHash_bucket(hash, HASH_TERM(TrNode_entry(chain), seed));
			  next = TrNode_next(chain);
			  TrNode_next(chain) = *new_bucket;
#ifdef ALLOW_REMOVE_TRIE
			  TrNode_previous(chain) = AS_TR_NODE_NEXT(bucket);
			  if (*new_bucket) TrNode_previous(*new_bucket) = chain;
#endif // ALLOW_REMOVE_TRIE 
			  *new_bucket = chain;
			  chain = next;
			} 
		      while (chain);
		    }
		} 
	      while (bucket != first_bucket);
	      //ARREGLAR ESTO
	      //free_hash_buckets(first_bucket, TrHash_num_buckets(hash) / 2);
	    }
	  return child;
	}
}

/* -------------------------- */
/*            API             */     
/* -------------------------- */

void init_tries_module(void) 
{
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
#endif // ALLOW_REMOVE_TRIE 
  TRIES = new_node;
  return new_node;
}

TrNode put_trie_entry(TrNode node, ENGINE_Term entry) 
{
  stack_args_base = stack_args = TERM_STACK;
  stack_vars_base = stack_vars = TERM_STACK + TERM_STACK_SIZE - 1;

  //printf("\nantes put entry\n"); fflush(stdout);
  node = put_trie(node, entry);
  //printf("\ndespues put entry\n"); fflush(stdout);

  //Update heap-stack variables to original values
  while (STACK_NOT_EMPTY(stack_vars++, stack_vars_base)) 
    {
      POP_DOWN(stack_vars);
      CTagToPointer(*stack_vars) = *stack_vars;
    }

#ifdef ALLOW_REMOVE_TRIE
  TrNode_hits(node)++;
#endif // ALLOW_REMOVE_TRIE 
  return node;
}  //printf("\nSALE\n",entry,t);


static
TrNode put_trie(TrNode node, ENGINE_Term entry) 
{
  ENGINE_Term t;
  DEREF(t,entry);
  if (IsTrieVar(t)) {
    node = trie_node_check_insert(node, t);
  } else if (ENGINE_IsVarTerm(t)) {
    node = trie_node_check_insert(node, ENGINE_VarTrie | ((stack_vars_base - stack_vars) << 1));
    CTagToPointer(t) = ENGINE_VarTrie | ((stack_vars_base - stack_vars) << 1);
    PUSH_UP(stack_vars, t, stack_args);
    PUSH_UP(stack_vars, stack_vars, stack_args);
  } else if (ENGINE_IsAtomTerm(t)) {
    node = trie_node_check_insert(node, t);
  } else if (ENGINE_IsIntTerm(t)) {
    if (TagIsSmall(t)) node = trie_node_check_insert(node, t);
    else  //large integer
      {
	node = trie_node_check_insert(node, ENGINE_LargeInitTag);
	node = trie_node_check_insert(node, *(TagToPointer(t) + 1));
	node = trie_node_check_insert(node, ENGINE_LargeEndTag);
      }
  } else if (ENGINE_IsFloatTerm(t)) {
	node = trie_node_check_insert(node, ENGINE_FloatInitTag);
	node = trie_node_check_insert(node, *(TagToPointer(t) + 1));
	node = trie_node_check_insert(node, *(TagToPointer(t) + 2));
	node = trie_node_check_insert(node, ENGINE_FloatEndTag);
  } else if (ENGINE_IsPairTerm(t)) {
    node = trie_node_check_insert(node, ENGINE_PairInitTag);
    do 
      {
	node = put_trie(node, ENGINE_HeadOfTerm(t));
	DEREF(t,ENGINE_TailOfTerm(t));
      } while (ENGINE_IsPairTerm(t));
    node = trie_node_check_insert(node, ENGINE_PairEndTag);
  } else if (ENGINE_IsApplTerm(t)) {
    if (!strcmp(ENGINE_NameOfFunctor(t),",") && ENGINE_ArityOfFunctor(t)  == 2) 
      {
	node = trie_node_check_insert(node, ENGINE_CommaInitTag);
	do 
	  {
	    node = put_trie(node, ENGINE_ArgOfTerm(1, t));
	    DEREF(t,ENGINE_ArgOfTerm(2, t));
	  } while (ENGINE_IsApplTerm(t) && !strcmp(ENGINE_NameOfFunctor(t),",") && ENGINE_ArityOfFunctor(t)  == 2);
	node = put_trie(node, t);
	node = trie_node_check_insert(node, ENGINE_CommaEndTag);
      } 
    else 
      {
	int i;
	node = trie_node_check_insert(node, CTagToPointer(t));
	for (i = 1; i <= ENGINE_ArityOfFunctor(t); i++)
	  {
	    node = put_trie(node, ENGINE_ArgOfTerm(i, t));
	  }
      }
  } else fprintf(stderr, "\nTries module: unknown type tag\n");
  
  return node;
}

ENGINE_Term get_trie_entry(worker_t *Arg, TrNode node) 
{
  stack_vars_base = stack_vars = TERM_STACK;
  stack_args_base = stack_args = TERM_STACK + TERM_STACK_SIZE - 1;
  max_index = -1;

  return get_trie(Arg, node, stack_args, &node);
}

static
ENGINE_Term get_trie(worker_t *Arg, TrNode node, ENGINE_Term *stack_mark, TrNode *cur_node) 
{
  ENGINE_Term t;

  while (TrNode_parent(node)) {
    t = TrNode_entry(node);
    //    printf("\nAntes de DEREF %p\n",t);
    //printf("\nTras DEREF %p\n",t);
    if (IsTrieVar(t)) {
      int index = TrieVarIndex(t);
      //printf("\nNodo GET %p tiene valor %p\n",node,node->entry);
      //printf("\nCogemos la variable %p del termino %p\n",index,t);      
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
	//printf("\nRepetimos la variable\n");	
	t = stack_vars_base[index];
      } else {
	//printf("\nVariable nueva\n");	
	t = ENGINE_MkVarTerm();
	stack_vars_base[index] = t;
      }
      PUSH_UP(stack_args, t, stack_vars);
    } else {
      DEREF(t,t); 
      if (t == ENGINE_FloatInitTag) {
      	printf("\nInit  float!!!!!!!!!!!!!!\n"); fflush(stdout);
      } else if (t == ENGINE_LargeInitTag) {
      	printf("\nInit  large!!!!!!!!!!!!!!\n"); fflush(stdout);
      } else if (t == ENGINE_FloatEndTag) {
	volatile double f;
	volatile ENGINE_Term *p;
	p = (ENGINE_Term *)((void *) &f); // to avoid gcc warning 
	node = TrNode_parent(node);
	*(p + 1) = TrNode_entry(node);
	node = TrNode_parent(node);
	*p = TrNode_entry(node);
	node = TrNode_parent(node); 
	t = ENGINE_MkFloatTerm(f);
	PUSH_UP(stack_args, t, stack_vars);
      } else if (t == ENGINE_LargeEndTag) {
	volatile int data;
	node = TrNode_parent(node);
	data = TrNode_entry(node);
	node = TrNode_parent(node); 
	t = ENGINE_MkIntTerm(data);
	PUSH_UP(stack_args, t, stack_vars);
	//node = TrNode_parent(node); // ignore ENGINE_LargInitTag 
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
      } else if (ENGINE_IsAtomTerm(t) && (ENGINE_ArityOfFunctor(&t) == 0)) {
	PUSH_UP(stack_args, t, stack_vars);
      } else if (ENGINE_IsIntTerm(t)) {
	//Cuidado con los enteros largos!!!
	PUSH_UP(stack_args, t, stack_vars);
      } else if (ENGINE_IsPairTerm(t)) {
	if (t == ENGINE_PairEndTag) {
	  t = ENGINE_MkAtomTerm("[]");
	  PUSH_UP(stack_args, t, stack_vars);
	  node = TrNode_parent(node);
	  t = get_trie(Arg, node, &stack_args[1], &node);
	  PUSH_UP(stack_args, t, stack_vars);
	} else if (t == ENGINE_PairInitTag) {
	  ENGINE_Term t2;
	  ENGINE_Term *stack_aux = stack_mark;
	  t = *stack_aux--;
	  while (STACK_NOT_EMPTY(stack_aux, stack_args)) {
	    t2 = *stack_aux--;
	    t = ENGINE_MkPairTerm(t2, t);
	  }
	  stack_args = stack_mark;
	  *cur_node = node;
	  return t;
	}
	
      } else if (ENGINE_IsAtomTerm(t) && (ENGINE_ArityOfFunctor(&t) != 0)) {
	int arity = ENGINE_ArityOfFunctor(&t);
	t = ENGINE_MkApplTerm(ENGINE_NameOfFunctor(&t), arity, stack_args + 1);
	stack_args += arity;
	PUSH_UP(stack_args, t, stack_vars);
      } else
	fprintf(stderr, "\nTries module: unknown type tag\n");
    }
    //printf("\nPasamos de nodo\n");
    node = TrNode_parent(node);
  }
  *cur_node = node;
  return t;
}


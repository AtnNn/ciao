
/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "cont_calls.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

ENGINE_Term my_make_var(goal_descriptor_t *state)
{
  ciao_ensure_heap(state, 1);
  worker_t * w = state->worker_registers;
  ENGINE_Term *pt = w->global_top;
  ENGINE_Term resul = TagHVA(pt);
  HeapPush(pt, resul);
  w->global_top = pt;
  return resul;
}			      

ENGINE_Term my_make_integer(goal_descriptor_t *state, int i) 
{
  ciao_ensure_heap(state, 4);
  worker_t * w = state->worker_registers;
  return MakeInteger(w, i);
}

ENGINE_Term my_make_float(goal_descriptor_t *state, double f) 
{
  ciao_ensure_heap(state, 4);
  worker_t * w = state->worker_registers;
  return MakeFloat(w, f);
}

ENGINE_Term my_make_list(goal_descriptor_t *state, ENGINE_Term head, ENGINE_Term tail) 
{
  ENGINE_Term list;
  ciao_ensure_heap(state, 3);
  worker_t * w = state->worker_registers;
  MakeLST(list, head, tail);
  return list;
}

ENGINE_Term my_make_functor(goal_descriptor_t *state, char* name, int arity, ENGINE_Term *args) 
{
  if (arity == 0) return MakeString((char *)name);
  else if (strcmp(name, ".") == 0 && arity == 2) 
    {
      ENGINE_Term list;
      ciao_ensure_heap(state, 3);
      worker_t * w = state->worker_registers;
      MakeLST(list, args[0], args[1]);
      return list;
    } 
  else 
    {
      int i;
      ENGINE_Term *pt;
      ENGINE_Term functor;
      ciao_ensure_heap(state, 2 + arity);
      functor = SetArity(MakeString((char *)name), arity);
      worker_t * w = state->worker_registers;
      pt = w->global_top;
      HeapPush(pt, functor);
      for (i = 0; i < arity; i++) HeapPush(pt, args[i]);
      w->global_top = pt;  
      return Tag(STR, HeapOffset(pt, -(arity+1)));
    }
}

void *mymalloc(int size) {
    void *t;

    t = (void *)malloc(size);
    if (t == NULL)
      panic("NULL malloc\n");
    bzero(t,size);
    return(t);
}

inline void *dyn_mem_alloc(int size) {
    void *tmp;
    memory_end = memory_end - (size / sizeof(long));
    tmp = (void *)memory_end;
    CHECK_MEM;
    return (tmp);
}

inline void *mem_alloc_n(int n) {
    void *tmp;
    tmp = (void *)memory_free;

    memory_free += n ;
    CHECK_MEM;
    return (tmp);
}


void *mem_alloc(int size) {
    void *tmp;
    tmp = (void *)memory_free;

    memory_free += (size / sizeof(long));
    CHECK_MEM;
    bzero(tmp,size); 
    return (tmp);
}



inline  struct WAIT_ENT *newwaitent() {
    struct WAIT_ENT *b;
    
    b = (struct WAIT_ENT *) dyn_mem_alloc(sizeof(struct WAIT_ENT));
    b->next = NULL;

    return(b);
}



inline struct WAIT_TAB *newwaittab() {
    struct WAIT_TAB *b;

    b = (struct WAIT_TAB *) dyn_mem_alloc(sizeof(struct WAIT_TAB));
    b->firstwait =  b->lastwait = NULL;
    return(b);
}

/* -------------------------- */
/*            API             */     
/* -------------------------- */

void panic (char *what) {
    fprintf(stderr,"panic at :%s\n",what);
    exit(0);
}

struct EACH_CALL *handlecall(ENGINE_Term Call) {	     
  struct EACH_CALL *callid;
  
  TrNode node = put_trie_entry(node_top,Call,MODE_STANDARD);
  
  if (node->child==NULL) {
    
    node->child = (trie_node_t *)mem_alloc(sizeof(struct EACH_CALL));
    callid=(struct EACH_CALL *)mem_alloc(sizeof(struct EACH_CALL));
    callid=(struct EACH_CALL *)(node->child);
      
    if(TOP_SF==NULL){
      callid->dfn=0;
      callid->plink=0;
    } else {
      callid->dfn=((struct EACH_CALL *)TOP_SF)->dfn+1;
      callid->plink=((struct EACH_CALL *)TOP_SF)->dfn+1;	
    }
    callid->previous=TOP_SF;
    TOP_SF=callid;

    callid->comp=READY;
    
    callid->waits = newwaittab();

    if (callid->trie_answers == NULL){
      callid->trie_answers=open_trie();
      callid->node_first=NULL;
      callid->node_last=NULL;
    }
    return callid;
  }else {
    return (struct EACH_CALL *)(node->child);
  }
}

int complete (struct EACH_CALL *call, struct EACH_CALL *parentcallid) {
  struct EACH_CALL *ccall;

  if (call->plink == call->dfn) {
    do {
      if ((long)TOP_SF == (long)call) {
	break;
      }
      ccall=(struct EACH_CALL *)TOP_SF;
      ccall->comp=COMPLETE;
      DELE_AND_FREE_WAITS(ccall->waits);
      TOP_SF=TOP_SF->previous;
    } while ((long)TOP_SF!= (long)call);
    //printf("\nse marca completo\n");
    call->comp=COMPLETE;	    
    DELE_AND_FREE_WAITS(call->waits);	
    
    TOP_SF=TOP_SF->previous;    
  } else {  
    SETMIN(parentcallid->plink,call->plink);
  }
  return TRUE;
}

bool_t put_tabled_call_c(Arg)
     Argdecl;
{

  //printf("\nPut tabled call\n"); fflush(stdout);

  ENGINE_Term Call;
  DEREF(Call, ENGINE_ARG1);  
  
  ENGINE_Term C;
  DEREF(C,ENGINE_ARG2);
  
  if(node_top==NULL) node_top=open_trie();

  struct EACH_CALL *callid = handlecall(Call); 

  ENGINE_Unify(ENGINE_MkIntTerm((int)callid),C);

  if (callid->comp != READY)   
    {
      ENGINE_Term Test;
      DEREF(Test, ENGINE_ARG3);
      ENGINE_Unify(Test,C);
    }
  else callid->comp = EVALUATING;
  return TRUE;
}

bool_t test_complete_c(Arg)
     Argdecl;
{
  //printf("\nTest complete\n"); fflush(stdout);

  ENGINE_Term C;
  DEREF(C, ENGINE_ARG1);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(C);
  complete(callid,callid);
  return TRUE;
}

bool_t is_lider_c(Arg)
     Argdecl;
{
  //printf("\nIs lider\n"); fflush(stdout);

  ENGINE_Term C;
  DEREF(C, ENGINE_ARG1);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(C);
  return (callid->comp == COMPLETE);
}

bool_t tabled_call_c(Arg)
     Argdecl;
{
  //printf("\nTabled call\n"); fflush(stdout);

  push_choicept(Arg,address_nd_tabled_call_c);

  ENGINE_Term Cid;
  DEREF(Cid, ENGINE_ARG1);
  ENGINE_Term Sid;
  DEREF(Sid, ENGINE_ARG2);
  ENGINE_Term Cont;
  DEREF(Cont, ENGINE_ARG3);
  ENGINE_Term Vars;
  DEREF(Vars, ENGINE_ARG4);
  ENGINE_Term F;
  DEREF(F, ENGINE_ARG5);

  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(Sid);
  struct EACH_CALL *parentcallid = (struct EACH_CALL *)ENGINE_IntOfTerm(Cid);

  //printf("\nAntes de SETMIN\n"); fflush(stdout);

  if (callid->comp==EVALUATING) SETMIN(parentcallid->plink, callid->plink);  

  //printf("\nComienza\n"); fflush(stdout);

  if (callid->ans_cnt != 0)
    {
      //printf("\nHay respuestas\n"); fflush(stdout);
      TrNode first_node_aux = callid->node_first;

      ENGINE_Term args[3];
      args[0] = get_trie_entry(Arg,first_node_aux,MODE_STANDARD);
      args[1] = Cid;
      args[2] = Vars;
      ENGINE_Unify(F,ENGINE_MkApplTerm(ENGINE_AtomName(Cont),3,args));	
 
      tagged_t *pointer = (tagged_t*) checkalloc (sizeof(tagged_t));
      tagged_t * term = (tagged_t*) &(Arg->misc->goal_desc_ptr->worker_registers->node->term[5]);
      *term = (tagged_t) ((int)pointer - MallocBase);
      *term = (tagged_t) ((int)*term << 2);
      *term = (tagged_t) ((int)*term | 0x80000000);
      *pointer = (tagged_t)first_node_aux;
      //printf("\nCREA POINTER = %p y valor %p\n",pointer,*pointer);

      return TRUE;
    }
  
      //printf("\nMetera continuacion?\n"); fflush(stdout);
  if(callid->comp==EVALUATING) 
    {
      //printf("\nMete continuacion en %d\n",callid); fflush(stdout);
      
      if (trie_list==NULL) trie_list=open_trie();
      
      register struct WAIT_ENT *waitentp;
      waitentp = newwaitent();
      waitentp->cont_pred = (char *)ENGINE_AtomName(Cont);
      waitentp->call_id = (struct EACH_CALL *)parentcallid; // QUE É PASSADO COMO ARGUMENTO Pidval
      waitentp->vars = put_trie_entry(trie_list,Vars,MODE_STANDARD); 

      if(callid->waits->firstwait==NULL) callid->waits->firstwait=waitentp;
      if(callid->waits->lastwait!=NULL) callid->waits->lastwait->next = waitentp;

      //printf("\nMeto una respuesta - %p\n",callid->waits->firstwait);
    
      callid->waits->lastwait = waitentp;
    }

  pop_choicept(Arg);

  return FALSE;
}

bool_t nd_tabled_call_c(Arg)
     Argdecl;
{
  tagged_t *pointer = (tagged_t*) (ENGINE_ARG6 & 0x0FFFFFFF);
  pointer = (tagged_t*) ((int)pointer >> 2);
  pointer = (tagged_t*) ((int)pointer + MallocBase);

  TrNode first_node_aux = (TrNode) *pointer;
  
  ENGINE_Term Cid;
  DEREF(Cid, ENGINE_ARG1);
  ENGINE_Term Sid;
  DEREF(Sid, ENGINE_ARG2);
  ENGINE_Term Cont;
  DEREF(Cont, ENGINE_ARG3);
  ENGINE_Term Vars;
  DEREF(Vars, ENGINE_ARG4);
  ENGINE_Term F;
  DEREF(F, ENGINE_ARG5);

  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(Sid);
  struct EACH_CALL *parentcallid = (struct EACH_CALL *)ENGINE_IntOfTerm(Cid);

  if(first_node_aux == callid->node_last)
    {
      if(callid->comp==EVALUATING) 
	{
	  if (trie_list==NULL) trie_list=open_trie();
	  
	  struct WAIT_ENT *waitentp;
	  waitentp = newwaitent();
	  waitentp->cont_pred = (char *)ENGINE_AtomName(Cont);
	  waitentp->call_id = (struct EACH_CALL *)parentcallid; // QUE É PASSADO COMO ARGUMENTO Pidval
	  waitentp->vars = put_trie_entry(trie_list,Vars,MODE_STANDARD); 
	  
	  if(callid->waits->firstwait==NULL) callid->waits->firstwait=waitentp;
	  if(callid->waits->lastwait!=NULL) callid->waits->lastwait->next = waitentp;
	  
	  callid->waits->lastwait = waitentp;
	}

      pop_choicept(Arg);
      return FALSE;
    }

  first_node_aux = first_node_aux->child;

  ENGINE_Term args[3];
  args[0] = get_trie_entry(Arg, first_node_aux,MODE_STANDARD);
  args[1] = Cid;
  args[2] = Vars;
  ENGINE_Unify(F,ENGINE_MkApplTerm(ENGINE_AtomName(Cont),3,args));	

  *pointer = (tagged_t) first_node_aux;
  
  return TRUE;
}

bool_t new_answer_c(Arg)
     Argdecl;
{
  //printf("\nNew Answer\n"); fflush(stdout);

  push_choicept(Arg,address_nd_new_answer_c);

  tagged_t *pointer = (tagged_t*) checkalloc (sizeof(tagged_t));
  tagged_t *term = (tagged_t*) &(Arg->misc->goal_desc_ptr->worker_registers->node->term[3]);
  *term = (tagged_t) ((int)pointer - MallocBase);
  *term = (tagged_t) ((int)*term << 2);
  *term = (tagged_t) ((int)*term | 0x80000000);

  ENGINE_Term Answer;
  DEREF(Answer, ENGINE_ARG1);
  ENGINE_Term Sid;
  DEREF(Sid, ENGINE_ARG2);
  ENGINE_Term F;
  DEREF(F, ENGINE_ARG3);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(Sid);

  if ( !set_diff_answer_trie(callid, Answer) )
    {
      pop_choicept(Arg);
      return(0);  
    }

  register struct WAIT_ENT *waitp, *lastwaitp;
  waitp = callid->waits->firstwait;
  lastwaitp = callid->waits->lastwait;
  
  if (waitp==NULL)
    {
      pop_choicept(Arg);
      return FALSE;
    }
  
  ENGINE_Term args[3];

  args[0] = Answer;
	
  args[1] = ENGINE_MkIntTerm((int)waitp->call_id);	
  args[2] = get_trie_entry(Arg, waitp->vars,MODE_STANDARD);

  ENGINE_Unify(F,ENGINE_MkApplTerm(waitp->cont_pred,3,args));	
      
  if (waitp == lastwaitp) *pointer = (tagged_t) NULL;
  else *pointer = (tagged_t) waitp->next;

  return TRUE;
}

bool_t nd_new_answer_c(Arg)
     Argdecl;
{
  //printf("\nnew answer ND\n"); fflush(stdout);

  ENGINE_Term Answer;
  DEREF(Answer, ENGINE_ARG1);
  ENGINE_Term Sid;
  DEREF(Sid, ENGINE_ARG2);
  ENGINE_Term F;
  DEREF(F, ENGINE_ARG3);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(Sid);

  tagged_t *pointer = (tagged_t*) (ENGINE_ARG4 & 0x0FFFFFFF);
  pointer = (tagged_t*) ((int)pointer >> 2);
  pointer = (tagged_t*) ((int)pointer + MallocBase);

  register struct WAIT_ENT *waitp, *lastwaitp;
  waitp = (struct WAIT_ENT *)*pointer;
  lastwaitp = callid->waits->lastwait;
  
  if (waitp==NULL)
    {
      pop_choicept(Arg);
      return FALSE;
    }
  
  ENGINE_Term args[3];

  args[0] = Answer;
	
  args[1] = ENGINE_MkIntTerm((int)waitp->call_id);	
  args[2] = get_trie_entry(Arg, waitp->vars,MODE_STANDARD);

  ENGINE_Unify(F,ENGINE_MkApplTerm(waitp->cont_pred,3,args));	
      
  if (waitp == lastwaitp) *pointer = (tagged_t) NULL;
  else *pointer = (tagged_t) waitp->next;

  return TRUE;
}

int set_diff_answer_trie(struct EACH_CALL *call,ENGINE_Term Ans){
  
  TrNode node;
  node=put_trie_entry(call->trie_answers,Ans,MODE_STANDARD);

  if (node->child==NULL && node != call->node_last){

    if (call->node_first==NULL){
      call->node_first=node;
    }
    if(call->node_last!=NULL){
      call->node_last->child=node;
    }
    
    call->node_last=node;
    call->ans_cnt=1;
    
    return TRUE;
  }else
    return FALSE;
}

bool_t consume_answer_c(Arg)
     Argdecl;
{

  //printf("\nconsume answer\n"); fflush(stdout); 
  push_choicept(Arg,address_nd_consume_answer_c);

  ENGINE_Term Call;
  DEREF(Call, ENGINE_ARG1);
  ENGINE_Term Id;
  DEREF(Id, ENGINE_ARG2);

  struct EACH_CALL *callid;

  if (!ENGINE_IsIntTerm(Id)) {
    pop_choicept(Arg);
    return FALSE;
  } else
    callid=(struct EACH_CALL *) (ENGINE_IntOfTerm(Id));
  
  if(callid->node_first==NULL) {
    pop_choicept(Arg);
    return FALSE;
  }

  id_solution = (tagged_t*) checkalloc (sizeof(tagged_t));

  *id_solution = (tagged_t) callid->node_first;

  TrNode node = (TrNode) *id_solution;

  node=node->child;
  
  if(ENGINE_Unify(Call,get_trie_entry(Arg, callid->node_first,MODE_STANDARD)))
    {
      tagged_t * term = (tagged_t*) &(Arg->misc->goal_desc_ptr->worker_registers->node->term[2]);
      *term = (tagged_t) ((int)id_solution - MallocBase);
      *term = (tagged_t) ((int)*term << 2);
      *term = (tagged_t) ((int)*term | 0x80000000);
      return TRUE;
    }

  pop_choicept(Arg);
  return FALSE;

}
bool_t nd_consume_answer_c(Arg)
     Argdecl;
{

  //printf("\nconsume answer NO DETERMINISTA\n"); fflush(stdout); 

  ENGINE_Term Call;
  DEREF(Call, ENGINE_ARG1);
  
  TrNode node;
  
  id_solution = (tagged_t*) (ENGINE_ARG3 & 0x0FFFFFFF);
  id_solution = (tagged_t*) ((int)id_solution >> 2);
  id_solution = (tagged_t*) ((int)id_solution + MallocBase);
  
  node=(TrNode) *id_solution;

  node=node->child;

  if(node==NULL) {
    //free(id_solution);
    pop_choicept(Arg);
    return FALSE;
  }

  *id_solution = (tagged_t) node;

  if(ENGINE_Unify(Call,get_trie_entry(Arg, node,MODE_STANDARD))) return TRUE;
  
  pop_choicept(Arg);
  return FALSE;  

}

bool_t initial_c(Arg)
     Argdecl;
{
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,3);
  address_nd_tabled_call_c = def_retry_c(nd_tabled_call_c,6);
  address_nd_new_answer_c = def_retry_c(nd_new_answer_c,4);
  init_tries_module();
  return TRUE;
}

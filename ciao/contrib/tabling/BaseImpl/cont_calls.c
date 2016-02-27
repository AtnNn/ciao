
/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "cont_calls.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

inline void *mymalloc(int size) {
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


inline void *mem_alloc(int size) {
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

  //printf("\nAntes de insertar\n"); fflush(stdout);
  TrNode node = put_trie_entry(node_top,ciao_unrefer(Call),MODE_STANDARD);
  //printf("\nTras insertar\n"); fflush(stdout);
  
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
    call->comp=COMPLETE;	    
    DELE_AND_FREE_WAITS(call->waits);	
    
    TOP_SF=TOP_SF->previous;    
  } else {  
    SETMIN(parentcallid->plink,call->plink);
  }
  return TRUE;
}

bool_t tabled_call_c(Arg)
     Argdecl;
{

  //  printf("\nEntra tabled call\n"); fflush(stdout); 
  //printf("\nSupuesto valor w = %p\n",w); fflush(stdout); 
  //printf("\nSupuesto valor  node = %p\n",w->node); fflush(stdout); 
  //printf("\nSupuesto valor = %p\n",w->node->next_insn); fflush(stdout); 
  //printf("\nSupuesto valor = %d\n",FrameSize(w->node->next_insn)); fflush(stdout); 

  init_state_from_WAM(Arg->misc->goal_desc_ptr);

  ENGINE_Term Call = ciao_refer(ENGINE_ARG1);  
  ENGINE_Term C = ciao_refer(ENGINE_ARG2);
  ENGINE_Term Vars = ciao_refer(ENGINE_ARG3);
  ENGINE_Term Cont_pred = ciao_refer(ENGINE_ARG4);
  ENGINE_Term Cont_pred_cont = ciao_refer(ENGINE_ARG5);
  
  struct EACH_CALL *callid, *parentcallid;

  char *name,*name_cont;
  long pidval;
  
  ENGINE_Term f1;
  ENGINE_Term args[3];


  if(node_top==NULL)
    node_top=open_trie();

  if (ENGINE_IsAtomTerm(Cont_pred))
    {
      name=(char *)ENGINE_AtomName(Cont_pred);
    }
  else
    {
      re_ciao_frame_end();
      return(0);
    }
  
  if (ENGINE_IsAtomTerm(Cont_pred_cont))
    {
      name_cont=(char *)ENGINE_AtomName(Cont_pred_cont);
    }
  else
    {
      re_ciao_frame_end();
      return(0);
    }

  callid = handlecall(Call); 

  if(ENGINE_IsIntTerm(C)) {
    pidval=ENGINE_IntOfTerm(C);
    parentcallid = (struct EACH_CALL *)pidval;
  } else {
    ENGINE_Unify(ENGINE_MkIntTerm((int)callid),C);
    parentcallid= callid;
  }
  
  if (callid->comp==READY) {
    callid->comp = EVALUATING;
    ENGINE_Term thisid;

    thisid=ENGINE_MkIntTerm((int)callid);

    ciao_term term_args[2];

    term_args[0]=Call;
    term_args[1]=thisid;

    f1=ENGINE_MkApplTerm(name,2,term_args);

    if (ENGINE_CallProlog(f1));    

    complete(callid,parentcallid);

  } else if (callid->comp==EVALUATING){
    SETMIN(parentcallid->plink, callid->plink); //DETERMINA O MINIMO DOS CICLOS POSITIVOS ENTRE Call e PidVal    
  }

  if (callid->comp==COMPLETE && (strcmp(name_cont,"true")==0))
    {
      re_ciao_frame_end();
      return TRUE;
    }
  
  
  if (callid->ans_cnt != 0 ){   /* has no answer */ //1-True;0-False
    TrNode first_node_aux = callid->node_first;
    int flag_ans=1;
    
    args[1]=C;
    args[2]=Vars;
    while(flag_ans==1) {
      args[0]=ciao_refer(get_trie_entry(first_node_aux,MODE_STANDARD));
      f1=ENGINE_MkApplTerm(name_cont,3,args);
      if(first_node_aux == callid->node_last)
	flag_ans=2;
      else
	first_node_aux=first_node_aux->child;
      
      if (ENGINE_CallProlog(f1));
    }
  }
  
  if(callid->comp==EVALUATING && strcmp(name_cont,"true")!=0) {
    if (trie_list==NULL)
      trie_list=open_trie();
    
    struct WAIT_ENT *waitentp;
    waitentp = newwaitent();
    waitentp->cont_pred = name_cont;
    waitentp->call_id = (struct EACH_CALL *)parentcallid; // QUE É PASSADO COMO ARGUMENTO Pidval
    waitentp->vars = put_trie_entry(trie_list,ciao_unrefer(Vars),MODE_STANDARD); 

    if(callid->waits->firstwait==NULL) {
      callid->waits->firstwait=waitentp;
    }
    if(callid->waits->lastwait!=NULL) {      
      callid->waits->lastwait->next = waitentp;
    }
    
    callid->waits->lastwait = waitentp;

  }
  re_ciao_frame_end();
  return FALSE;
}


bool_t new_answer_c(Arg)
     Argdecl;
{

  //  printf("\nEntra new_answer\n"); fflush(stdout); 

  init_state_from_WAM(Arg->misc->goal_desc_ptr);

  ENGINE_Term   Ans    = ciao_refer(ENGINE_ARG1);
  ENGINE_Term   CallId = ciao_refer(ENGINE_ARG2);

  ENGINE_Term f1;
  ENGINE_Term args[3];
  
  struct EACH_CALL *call;

  if (!ENGINE_IsIntTerm(CallId))
    {
      re_ciao_frame_end();
      return(0);
    }
  else
    call=(struct EACH_CALL *) (ENGINE_IntOfTerm(CallId));
    
  if ( !set_diff_answer_trie(call, Ans) )
    {
      re_ciao_frame_end();
      return(0);  
    }
  {
    register struct WAIT_ENT *waitp, *lastwaitp;
    ENGINE_Term Vars, Id;

    
    waitp = call->waits->firstwait;
    lastwaitp = call->waits->lastwait;

    if (waitp==NULL)
      {
	re_ciao_frame_end();
	return FALSE;
      }

    while (waitp != lastwaitp) {

      Vars=ciao_refer(get_trie_entry(waitp->vars,MODE_STANDARD));

      Id=ENGINE_MkIntTerm((int)waitp->call_id);	
      
      args[0]=Ans;
      args[1]=Id;
      args[2]=Vars;
	
      f1=ENGINE_MkApplTerm(waitp->cont_pred,3,args);
	
      if (ENGINE_CallProlog(f1));
      waitp = waitp->next;
    }

    Vars=ciao_refer(get_trie_entry(waitp->vars,MODE_STANDARD));
    Id=ENGINE_MkIntTerm((int)waitp->call_id);	
    args[0]=Ans;
    args[1]=Id;
    args[2]=Vars;

    f1=ENGINE_MkApplTerm(waitp->cont_pred,3,args);
	
    if (ENGINE_CallProlog(f1));
    re_ciao_frame_end();
    return FALSE;
  }
}

int set_diff_answer_trie(struct EACH_CALL *call,ENGINE_Term Ans){
  
  TrNode node;
  node=put_trie_entry(call->trie_answers,ciao_unrefer(Ans),MODE_STANDARD);

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

//orden push_cjoice
bool_t consume_answer_c(Arg)
     Argdecl;
{

  //  printf("\nEntra consume answer\n"); fflush(stdout); 
  push_choicept(Arg,address_nd_consume_answer_c);

  init_state_from_WAM(Arg->misc->goal_desc_ptr);

  ENGINE_Term Call = ciao_refer(ENGINE_ARG1);
  ENGINE_Term Id = ciao_refer(ENGINE_ARG2);

  struct EACH_CALL *callid;

  if (!ENGINE_IsIntTerm(Id)) {
    re_ciao_frame_end();
    pop_choicept(Arg);
    return FALSE;
  } else
    callid=(struct EACH_CALL *) (ENGINE_IntOfTerm(Id));
  
  if(callid->node_first==NULL) {
    re_ciao_frame_end();
    pop_choicept(Arg);
    return FALSE;
  }


  TrNode node = (TrNode) callid->node_first;

  if(ENGINE_Unify(Call,ciao_refer(get_trie_entry(callid->node_first,MODE_STANDARD))))
    {
      re_ciao_frame_end();
      Arg->misc->goal_desc_ptr->worker_registers->node->term[2] = (tagged_t)node;
      return TRUE;
    }

  //free(id_solution);
  re_ciao_frame_end();
  pop_choicept(Arg);
  return FALSE;

}
bool_t nd_consume_answer_c(Arg)
     Argdecl;
{

  //printf("\nEntra consume answer NO DETERMINISTA\n"); fflush(stdout); 
  init_state_from_WAM(Arg->misc->goal_desc_ptr);

  ENGINE_Term Call = ciao_refer(ENGINE_ARG1);
  
  TrNode node = (TrNode) ENGINE_ARG3;
  
  //#if defined(TABLING)
  //id_solution = Arg->misc->goal_desc_ptr->worker_registers->node->tabling_node;
  //#endif
  
  
  //untrail unification after las consume_answer
  //tagged_t *pt2, t1, a0;

 /*if (TrailYounger(pt2=Arg->misc->goal_desc_ptr->worker_registers->trail_top,t1=(tagged_t)TagToPointer(id_solution->trail))) 
    {
      do
	{
	  a0 = TrailPop(pt2);			
	  if (!IsVar(a0)) goto undo_found;
	  else
	    CTagToPointer(a0) = a0;		    
	}
      while (TrailYounger(pt2,t1));
      Arg->misc->goal_desc_ptr->worker_registers->trail_top = pt2;
    }
  */
  node=node->child;

  if(node==NULL) {
    //free(id_solution);
    re_ciao_frame_end();
    pop_choicept(Arg);
    return FALSE;
  }

  Arg->misc->goal_desc_ptr->worker_registers->node->term[2] = (tagged_t) node;

  if(ENGINE_Unify(Call,ciao_refer(get_trie_entry(node,MODE_STANDARD))))
    {
      re_ciao_frame_end();
      return TRUE;
    }
  
  //free(id_solution);
  re_ciao_frame_end();
  pop_choicept(Arg);
  return FALSE;  

}

bool_t initial_c(Arg)
     Argdecl;
{
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,3);
  init_tries_module();
  return TRUE;
}

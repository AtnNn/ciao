/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "ciao_prolog.h"
#include "cont_calls.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

tagged_t *ini = NULL;
//ENGINE_Term array[TERM_STACK_SIZE];
//

ENGINE_Term my_make_var(goal_descriptor_t *state)
{
  ciao_ensure_heap(state, 1);
  ENGINE_Term resul = TagHVA(GTOP);
  HeapPush(GTOP, resul);
  return resul;
}			      

ENGINE_Term my_make_integer(goal_descriptor_t *state, int i) 
{
  ciao_ensure_heap(state, 4);  //change to in my_heap
  return MakeInteger(REGISTERS, i);
}

ENGINE_Term my_make_float(goal_descriptor_t *state, double f) 
{
  ciao_ensure_heap(state, 4);
  return MakeFloat(REGISTERS, f);
}

ENGINE_Term my_make_list(goal_descriptor_t *state, ENGINE_Term head, ENGINE_Term tail) 
{
  ENGINE_Term list;
  ciao_ensure_heap(state, 3);
  worker_t * w = REGISTERS;
  MakeLST(list, head, tail);
  return list;
}

ENGINE_Term my_make_functor(goal_descriptor_t *state, char* name, int arity, ENGINE_Term *args) 
{
  worker_t * w = REGISTERS;
  if (arity == 0) return MakeString((char *)name);
  else if (strcmp(name, ".") == 0 && arity == 2) 
    {
      ENGINE_Term list;
      ciao_ensure_heap(state, 3);
      MakeLST(list, args[0], args[1]);
      return list;
    } 
  else 
    {
      int i;
      ciao_ensure_heap(state, 2 + arity);
      HeapPush(P_GTOP, SetArity(MakeString((char *)name), arity));
      for (i = 0; i < arity; i++) HeapPush(P_GTOP, args[i]);
      return Tag(STR, HeapOffset(P_GTOP, -(arity+1)));
    }
}

//void show_state(Argdecl)
//{
//  printf("\nlast_insn %p\n",Arg->last_insn);
//  printf("\nnext_node %p\n",Arg->next_node);
//  printf("\ninsn %p\n",Arg->insn);
//  printf("\nglobal_uncond %p\n",Arg->global_uncond);
//  printf("\nlocal_uncond %p\n",Arg->local_uncond);
//  printf("\nglobal_top %p\n",Arg->global_top);
//  printf("\nnext_alt %p\n",Arg->next_alt);
//  printf("\nframe %p\n",Arg->frame);
//  printf("\nframe pre %p\n",Arg->frame->frame);
//  printf("\nnext_insn %p\n",Arg->next_insn);
//  printf("\nlocal_top %p\n",Arg->local_top);
//  printf("\nstack_start %p\n",Arg->stack_start);
//  printf("\nglobal_top %p\n",Arg->global_top);
//  printf("\nheap_start %p\n",Arg->heap_start);
//  printf("\ntrail_top %p\n",Arg->trail_top);
//  printf("\ntrail_start %p\n",Arg->trail_start);
//  printf("\nNext_Node %p\n",Arg->next_node);
//  printf("\nchoice start %p\n",Arg->choice_start);
//  printf("\nNode %p\n",Arg->node);
//  printf("\nNode->trail_top %p\n",Arg->node->trail_top);
//  printf("\nNode->global_top %p\n",Arg->node->global_top);
//  printf("\nNode->frame %p\n",Arg->node->frame);
//  printf("\nNode->local_top %p\n",Arg->node->local_top);
////  tagged_t *ind;
////  printf("\nTRAIL\n");
////  for (ind = Arg->trail_start; ind <= Arg->trail_top; ind++)
////    printf("\n%p = %p",ind,*ind); 
////  printf("\n");
////  printf("\nHEAP\n");
////  for (ind = Arg->heap_start + ((Arg->heap_end - Arg->heap_start)/2); 
////       ind <= Arg->global_top; ind++)
////    printf("\n%p = %p",ind,*ind); 
////  printf("\n");
////  printf("\nFRAMES\n");
////  for (ind = Arg->stack_start; ind <= (tagged_t*)Arg->local_top; ind++)
////    printf("\n%p = %p",ind,*ind); 
////  printf("\n");
////  printf("\nCHOICES\n");
////  for (ind = Arg->choice_start; ind >= (tagged_t*)Arg->node; ind--)
////    printf("\n%p = %p",ind,*ind); 
////  printf("\n");
//}

void freeze_stacks(Argdecl) //warning about problematics stackyounger 
{
  //Last choice is already frozen
  if (heap_freeze == Arg->node->global_top)
    {
      heap_freeze = Arg->global_top;
      if (StackYounger(StackCharOffset(Arg->frame,FrameSize(Arg->next_insn)),stack_freeze))
	stack_freeze = StackCharOffset(Arg->frame,FrameSize(Arg->next_insn));
      return;
    }

  //Updating new values
  heap_freeze = Arg->global_top;
  if (StackYounger(NodeLocalTop(Arg->node), 
		   StackCharOffset(Arg->frame,FrameSize(Arg->next_insn)))) 
    {
      if (StackYounger(stack_freeze,NodeLocalTop(Arg->node))) 
	printf("\nERROR freezing II\n");
      stack_freeze = NodeLocalTop(Arg->node); 
    }
  else 
    {
      if (StackYounger(stack_freeze,StackCharOffset(Arg->frame,FrameSize(Arg->next_insn))))
	printf("\nERROR freezing III\n");
      stack_freeze = StackCharOffset(Arg->frame,FrameSize(Arg->next_insn));
    }

//  printf("\nCongelando heap %p stack %p\n", heap_freeze, stack_freeze);

  //Updating pointers from not frozen choice points.
  node_t *ind;
  for (ind = Arg->node; 
       ind->global_top != (ENGINE_Term *)&heap_freeze;
       ind = ChoiceCharOffset(ind,-ind->next_alt->node_offset))
    {
      ind->global_top = (ENGINE_Term *)&heap_freeze;
      ind->local_top = (frame_t *)&stack_freeze;      
      ind->open_state = stateMVV;      
    }
  stateMVV++;
}

struct consumer* get_consumer(Argdecl, struct subs_factor *sub_fact, struct EACH_CALL *parent_id)
{
//  show_state(Arg);
  struct consumer* res = (struct consumer*) malloc (sizeof(struct consumer));
  
  res->sub_fact = sub_fact;

  res->ntrail = (Arg->trail_top - lider_trail) * 2;
//  printf("\ntrail %d\n",res->ntrail);
  res->trail = (ENGINE_Term*) malloc (res->ntrail * (sizeof(ENGINE_Term)));

  ENGINE_Term *index;
  int i;
  for (index = Arg->trail_top - 1, i = 0; index >= lider_trail; index--)
    {
      res->trail[i++] = *index;
      res->trail[i++] = CTagToPointer(*index);
    }
  res->frame = Arg->frame;
  res->next_insn = Arg->next_insn; //continuation
  res->last_answer = NULL;
  res->parent_id = parent_id;

  freeze_stacks(Arg);

  return res;
}


///* -------------------------- */
///*            API             */     
///* -------------------------- */

inline void *mem_alloc(int size) 
{
    void *tmp;
    tmp = (void *)memory_free;

    memory_free += (size / sizeof(long));
    CHECK_MEM;
    return (tmp);
}

bool_t clean_tables_c(Arg)
     Argdecl;
{
  memory_free = &memory[0];
//  tabled_top = ini;
  node_top = NULL;
  return TRUE;
}

struct EACH_CALL *handlecall(Argdecl, ENGINE_Term Call, struct subs_factor* sub_fact) 
{	     
  struct EACH_CALL *callid;
  TrNode node = put_trie_entry(node_top, Call, sub_fact);

  if (node->child == NULL) 
    {
      callid = (struct EACH_CALL*) mem_alloc (sizeof(struct EACH_CALL));
      node->child = (trie_node_t *) callid;

      callid->sub_fact = sub_fact;

      callid->trie_answers = open_trie();
      callid->first_answer = NULL;
      callid->last_answer = NULL;

      callid->cons = NULL;
      callid->last_cons = NULL;

      if(TOP_SF==NULL) //First tabling predicate
	{
	  lider_trail = Arg->trail_top;

	  heap_freeze = NodeGlobalTop(Arg->node);
	  Arg->node->global_top = (tagged_t*)&heap_freeze;

	  stack_freeze = NodeLocalTop(Arg->node);
	  Arg->node->local_top = (frame_t*)&stack_freeze;
	  callid->dfn = 0;
	  callid->plink = 0;
	} 
      else //Seconds tabling predicates
	{
	  callid->dfn = ((struct EACH_CALL *)TOP_SF)->dfn + 1;
	  callid->plink = ((struct EACH_CALL *)TOP_SF)->dfn + 1;	
	}
      
      callid->previous = TOP_SF;
      TOP_SF = callid;
      callid->comp = READY;
      return callid;
    }
  else return (struct EACH_CALL *)(node->child);
}

int complete (struct EACH_CALL *call, struct EACH_CALL *parentcallid) 
{
  struct EACH_CALL *ccall;

  if (call->plink == call->dfn) 
    {
      do 
	{
	  if ((long)TOP_SF == (long)call) break;
	  ccall = (struct EACH_CALL *)TOP_SF;
	  ccall->comp = COMPLETE;
	  //DELE_AND_FREE_WAITS(ccall->waits);
	  //Liberar estructuras
	  TOP_SF = TOP_SF->previous;
	} while ((long)TOP_SF != (long)call);
      call->comp = COMPLETE;	    
      //DELE_AND_FREE_WAITS(call->waits);	
      //Liberar estructuras
      TOP_SF = TOP_SF->previous;    
      if (TOP_SF == NULL) 
	{
	  lider_trail = NULL;
//	  //Liberar congelados, con que valores???
//	  Arg->node->global_top = heap_freeze;
//	  Arg->node->local_top = stack_freeze;
	}
    } 
  return TRUE;
}

bool_t tabled_call_c(Arg)
     Argdecl;
{
//  printf("\ntabled_call\n"); fflush(stdout);
//  printf("\nINICIO\n");

//  show_state(Arg);

//  printf("\nNode->frame %p\n", Arg->frame);
//  printf("\nNode->frame[1] %p\n", Arg->frame->term[0]);
//  printf("\nNode->frame[2] %p\n", Arg->frame->term[1]);
//  printf("\nNode->frame[3] %p\n", Arg->frame->term[2]);
//  ENGINE_Term ttt;
//  DEREF(ttt,Arg->frame->term[0]);
//  printf("\nDEREF-Node->frame[1] %p\n", ttt);
//  ENGINE_Term ttt2;
//  DEREF(ttt2,Arg->frame->term[1]);
//  printf("\nDEREF-Node->frame[2] %p\n", ttt2);
//  ENGINE_Term ttt3;
//  DEREF(ttt3,Arg->frame->term[2]);
//  printf("\nDEREF-Node->frame[3] %p\n", ttt3);

  if (node_top == NULL) node_top = open_trie();

  struct subs_factor *sub_fact = (struct subs_factor*) malloc (sizeof(struct subs_factor));
  struct EACH_CALL *callid = handlecall(Arg, ENGINE_ARG1, sub_fact); 

//  printf("\nAPILO isid_stack %d++ con %p\n",isid_stack,callid);
  sid_stack[isid_stack++] = callid;

  if (callid->comp == READY) 
    {
//      printf("\nEstamos en un GENERADOR\n");
      callid->comp = EVALUATING;
 
//      printf("\nstart execution GENERATOR %p y plink %d\n", callid, callid->plink); 
//      fflush(stdout);
//      printf("\nNode->frame %p\n", Arg->frame);
//      printf("\nNode->frame[1] %p\n", Arg->frame->term[0]);
//      printf("\nNode->frame[2] %p\n", Arg->frame->term[1]);
//      printf("\nNode->frame[3] %p\n", Arg->frame->term[2]);
//      printf("\nNode->frame[4] %p\n", Arg->frame->term[3]);
//      ENGINE_Term ttt;
//      DEREF(ttt,Arg->frame->term[3]);
//      printf("\nDEREF-Node->frame[4] %p\n", ttt);
//      printf("\nDel nodo %p - valor %p\n",w->node,NodeLocalTop(w->node));
      init_state_from_WAM(Arg->misc->goal_desc_ptr);
      if (StackYounger(stack_freeze,Arg->frame)) printf("\nPROBLEMONES\n");
//      printf("\nstart frame - freeze %p\n",stack_freeze); fflush(stdout);
//      printf("\nDel nodo %p - valor %p\n",w->node,NodeLocalTop(w->node));
//      printf("\nNode->frame %p\n", Arg->frame);
//      printf("\nNode->frame[1] %p\n", Arg->frame->term[0]);
//      printf("\nNode->frame[2] %p\n", Arg->frame->term[1]);
//      printf("\nNode->frame[3] %p\n", Arg->frame->term[2]);
//      printf("\nNode->frame[4] %p\n", Arg->frame->term[3]);
      ciao_commit_call_term(ciao_refer(ENGINE_ARG1));
      re_ciao_frame_end();
//      printf("\nfin execution\n"); fflush(stdout);
//      printf("\nNode->frame %p\n", Arg->frame);
//      printf("\nNode->frame[1] %p\n", Arg->frame->term[0]);
//      printf("\nNode->frame[2] %p\n", Arg->frame->term[1]);
//      printf("\nNode->frame[3] %p\n", Arg->frame->term[2]);
//      printf("\nNode->frame[4] %p\n", Arg->frame->term[3]);
//      ENGINE_Term ttt2;
//      DEREF(ttt2,Arg->frame->term[3]);
//      printf("\nDEREF-Node->frame[4] %p\n", ttt2);

      //Throwing continuations (resume_ccalls) before checking for complete
      struct EACH_CALL *sf = NULL;

//      printf("\ntermina execution GENERATOR %p - RELANZANDO\n", callid); fflush(stdout);
      do
	{
	  if (sf == NULL) sf = TOP_SF;
	  else sf = sf->previous;
//	  printf("\nEstudiando %p\n",sf);
	  struct cons_list *aux;
	  for (aux = sf->cons; aux != NULL; aux = aux->sig)
	    {
//	      printf("\nHay consumers\n"); fflush(stdout);
	      trie_node_t *Answers = aux->consumer->last_answer;
	      if ((sf->last_answer != NULL) && (Answers != sf->last_answer))
		{//if first_ans of first_consumer is NULL, then it is NULL for the others.
//		  printf("\nHay respuestas pendientes\n"); fflush(stdout);
		  if (Answers == NULL) Answers = sf->first_answer;
		  else Answers = Answers->child;
		  aux->consumer->last_answer = Answers;
		  
//		  printf("\nConsumer %p de %p-%p lee respuesta %p\n", 
//			 aux->consumer,sf,aux->consumer->parent_id,Answers);
		  
		  //Updating completion stack
		  //	      printf("\nRELANZO con isid_stack %d++ valor %p\n",
		  //		     isid_stack,aux->consumer->parent_id);
		  sid_stack[isid_stack++] = aux->consumer->parent_id; 
		  
		  //Consumer choice_pt to consume all the answers.
		  push_choicept(Arg, address_nd_resume_cons_c);
		  
		  //Variables for the next execution of nd_resume_cons_c
		  Arg->node->term[0] = (ENGINE_Term)sf;
		  Arg->node->term[1] = (ENGINE_Term)aux;
		  
		  //Reinstalling the trail
		  int iTrail;
		  for (iTrail = 0; iTrail < aux->consumer->ntrail; iTrail = iTrail + 2)
		    {
		      //		  printf("\nUnificando TRAIL %p - %p\n",
		      //			 aux->consumer->trail[iTrail],
		      //			 aux->consumer->trail[iTrail + 1]);
		      if (!ENGINE_Unify(aux->consumer->trail[iTrail],
					aux->consumer->trail[iTrail + 1]))
			printf("\nPROBLEMAS I\n");		  
		    }
		  
		  //Reinstalling subtitution factor from answer
		  
		  struct subs_factor *answer = get_trie_answer(Arg,Answers);
		  int iVars;
		  for (iVars = 0; iVars < answer->size; iVars++)
		    {
		      //printf("\nUnificando ANS %p - %p\n",
		      //aux->consumer->sub_fact->vars[iVars],answer->vars[iVars]);
		      if (!ENGINE_Unify(aux->consumer->sub_fact->vars[iVars],
					answer->vars[iVars])) 
			{
			  printf("\nPROBLEMAS II\n");		  
			  return FALSE;
			}
		    }
		  Arg->next_insn = aux->consumer->next_insn;
		  Arg->frame = aux->consumer->frame;
		  
		  //	      printf("\nNode->frame %p\n", Arg->frame);
		  //	      printf("\nNode->frame[1] %p\n", Arg->frame->term[0]);
		  //	      printf("\nNode->frame[2] %p\n", Arg->frame->term[1]);
		  //	      printf("\nNode->frame[3] %p\n", Arg->frame->term[2]);
		  return TRUE;
		}
	    }
	}
      while(sf != callid);  
//      printf("\nVemos si completa %p\n",callid); 
      complete(callid,callid);  
    }
//  else
//    printf("\nConsumidor de GENERATOR %p\n", callid); fflush(stdout);


//  printf("\nTERMINA GENERATOR o era consumer isid_stack %d--\n",isid_stack);
  isid_stack--;
  if (isid_stack && sid_stack[isid_stack]->comp != COMPLETE) 
    {
//      printf("\n Entra1 y con %d\n",sid_stack[isid_stack]->comp);
      SETMIN(sid_stack[isid_stack - 1]->plink, sid_stack[isid_stack]->plink);  
//      printf("\nPLINK de GENERATOR %p es %p\n", sid_stack[isid_stack - 1],
//	 sid_stack[isid_stack - 1]->plink); fflush(stdout);
    }


  //Reading actual answers
//  printf("\nEsta completo?\n"); fflush(stdout);
  if (callid->comp == COMPLETE) 
    {
//      printf("\n%p Esta completo\n", callid); fflush(stdout);
//      printf("\nHay respuestas?\n"); fflush(stdout);
      if (callid->first_answer != NULL)
	{
//	  printf("\nHay respuestas\n"); fflush(stdout);
	  TrNode node = callid->first_answer;
	  
	  push_choicept(Arg,address_nd_consume_answer_c);
	  Arg->node->term[0] = (ENGINE_Term)sub_fact;
	  Arg->node->term[1] = (ENGINE_Term)node;
	  Arg->node->term[2] = (ENGINE_Term)callid;
	  
	  struct subs_factor *answer = get_trie_answer(Arg,node);
	  
	  int iVars;
	  for (iVars = 0; iVars < answer->size; iVars++)
	    {
//	      printf("\nUnificando ANS %p - %p\n",
//		     sub_fact->vars[iVars],answer->vars[iVars]);
	      if (!ENGINE_Unify(sub_fact->vars[iVars],answer->vars[iVars])) 
		{
		  printf("\nPROBLEMAS III\n");		  
		  DEREF(sub_fact->vars[iVars],sub_fact->vars[iVars]);
		  return FALSE;
		}
	    }

	  return TRUE;        
	}
      else 
	{
//	  printf("\nNO hay respuestas\n"); fflush(stdout);
	  return FALSE;
	}
    }

//  printf("\nCREAMOS CONSUMER\n");
//  printf("\nNode->frame %p\n", Arg->frame);
//  printf("\nNode->frame[1] %p\n", Arg->frame->term[0]);
//  printf("\nNode->frame[2] %p\n", Arg->frame->term[1]);
//  printf("\nNode->frame[3] %p\n", Arg->frame->term[2]);

//  printf("\n%p NO esta completo, padre consumidor %p\n", callid,sid_stack[isid_stack - 1]); 
//  fflush(stdout);
  struct consumer* consumer = get_consumer(Arg, sub_fact, sid_stack[isid_stack - 1]);
  consumer->last_answer = NULL;

  if (callid->last_cons == NULL)
    {
      callid->cons = (struct cons_list*) malloc (sizeof(struct cons_list));
      callid->cons->consumer = consumer;
      callid->cons->sig = NULL;
      callid->last_cons = callid->cons;
    }
  else
    {
      callid->last_cons->sig = (struct cons_list*) malloc (sizeof(struct cons_list));
      callid->last_cons = callid->last_cons->sig;
      callid->last_cons->consumer = consumer;
      callid->last_cons->sig = NULL;
    }

  return FALSE;
}

bool_t nd_consume_answer_c(Arg)
     Argdecl;
{
//  printf("\nnd_consume_answer\n"); fflush(stdout);

  TrNode node = (TrNode) ENGINE_ARG2;
  struct subs_factor *sub_fact = (struct subs_factor*) Arg->node->term[0];

  if(node->child == NULL) 
    {

      pop_choicept(Arg);
      return FALSE;
    }

  node = node->child;
  Arg->node->term[1] = (ENGINE_Term)node;

  struct subs_factor *answer = get_trie_answer(Arg,node);

  int iVars;
  for (iVars = 0; iVars < answer->size; iVars++)
    {
      if (!ENGINE_Unify(sub_fact->vars[iVars],answer->vars[iVars])) 
	{
	  printf("\nPROBLEMAS IV\n");		  
	  return FALSE;
	}
    }
  return TRUE;
}

bool_t nd_prueba_c(Arg)
     Argdecl;
{
  pop_choicept(Arg);
  return TRUE;
}


bool_t nd_resume_cons_c(Arg)
     Argdecl;
{
//  printf("\nnd_resume_cons\n"); fflush(stdout);

//  int k;
//  for (k = 0; k < isid_stack; k++)
//    printf("\nSID[%d] = %p\n",k,sid_stack[k]);
//  printf("\nisid_stack %d\n",isid_stack);

  struct EACH_CALL *sf = (struct EACH_CALL*) ENGINE_ARG1;
  struct cons_list *aux = (struct cons_list*) ENGINE_ARG2;

  trie_node_t *Answers = aux->consumer->last_answer;

  if (Answers != sf->last_answer)
    {      
      Answers = Answers->child;
      aux->consumer->last_answer = Answers;

//	      printf("\nConsumer %p de %p-%p lee respuesta %p\n", 
//		     aux->consumer,sf,aux->consumer->parent_id,Answers);

      //Reinstalling the trail
      int iTrail;
      for (iTrail = 0; iTrail < aux->consumer->ntrail; iTrail = iTrail + 2)
	{
//	  printf("\nTRAIL unificando %p - %p\n",
//		 aux->consumer->trail[iTrail],aux->consumer->trail[iTrail+1]);
	  if (!ENGINE_Unify(aux->consumer->trail[iTrail],
			    aux->consumer->trail[iTrail + 1]))
		    printf("\nPROBLEMAS V\n");		  
	}

      //Reinstalling subtitution factor from answer
      
      struct subs_factor *answer = get_trie_answer(Arg,Answers);
      int iVars;
      for (iVars = 0; iVars < answer->size; iVars++)
	{
//	  printf("\nANS unificando %p - %p\n",
//		 aux->consumer->sub_fact->vars[iVars],answer->vars[iVars]);
	  if (!ENGINE_Unify(aux->consumer->sub_fact->vars[iVars],answer->vars[iVars])) 
	    {
	      printf("\nPROBLEMAS VI\n");		  
	      return FALSE;
	    }
	}
      Arg->next_insn = aux->consumer->next_insn;
      Arg->frame = aux->consumer->frame;

      return TRUE;
    } 

//  printf("\nTermina CONSUMER isid_stack %d--\n",isid_stack);

  isid_stack--;
  if (aux->sig != NULL)
    {
      //WARNING --> for to traverse aux is neccesary??
      for (aux = aux->sig; aux != NULL; aux = aux->sig)
	{
	  Answers = aux->consumer->last_answer;
	  
	  if ((sf->last_answer != NULL) && (Answers != sf->last_answer))
	    {      
	      if (Answers == NULL) Answers = sf->first_answer;
	      else Answers = Answers->child;
	      aux->consumer->last_answer = Answers;
	      
	      //	      printf("\nConsumer %p de %p-%p lee respuesta %p\n", 
	      //		     aux->consumer,sf,aux->consumer->parent_id,Answers);
	      
	      //	  printf("\nConsumer %p lee respuesta %p\n",
	      //		 aux->consumer,Answers);
	      
	      //	  printf("\nRelanzo CONSUMER isid_stack %d++ valor %p\n",
	      //		 isid_stack,aux->consumer->parent_id);
	      
	      sid_stack[isid_stack++] = aux->consumer->parent_id; 
	      
	      Arg->node->term[1] = (ENGINE_Term)aux;
	      //Reinstalling the trail
	      int iTrail;
	      for (iTrail = 0; iTrail < aux->consumer->ntrail; iTrail = iTrail + 2)
		{
		  if (!ENGINE_Unify(aux->consumer->trail[iTrail],
				    aux->consumer->trail[iTrail + 1]))		
		    {
		      ENGINE_Term tt;
		      DEREF(tt,CTagToPointer(aux->consumer->trail[iTrail]));
		      printf("\nPROBLEMAS VII %p - %p - DEREF(1) %p\n",
			     CTagToPointer(aux->consumer->trail[iTrail]),
			     aux->consumer->trail[iTrail + 1],
			     tt);		  
		    }
		}
	      
	      //Reinstalling subtitution factor from answer
	      
	      struct subs_factor *answer = get_trie_answer(Arg,Answers);
	      int iVars;
	      for (iVars = 0; iVars < answer->size; iVars++)
		if (!ENGINE_Unify(aux->consumer->sub_fact->vars[iVars],answer->vars[iVars])) 
		  {
		    printf("\nPROBLEMAS VIII\n");		  
		    return FALSE;
		  }
	      
	      Arg->next_insn = aux->consumer->next_insn;
	      Arg->frame = aux->consumer->frame;
	      return TRUE;
	    } 
	}
    }
  
//  printf("\nCompletando %p\n",sid_stack[isid_stack-1]);
  while(sf != sid_stack[isid_stack-1])
    {
      sf = sf->previous;    
      if (sf->cons == NULL) continue;

      for (aux = sf->cons; aux != NULL; aux = aux->sig)
	{
	  Answers = aux->consumer->last_answer;
	  if ((sf->last_answer != NULL) && (Answers != sf->last_answer))
	    {//if first_ans of first_consumer is NULL, then it is NULL for the others.
	      if (Answers == NULL) Answers = sf->first_answer;
	      else Answers = Answers->child;
	      aux->consumer->last_answer = Answers;
	      
//	      printf("\nConsumer %p de %p-%p lee respuesta %p\n", 
//		     aux->consumer,sf,aux->consumer->parent_id,Answers);
	      
	      //Updating completion stack
	      
	      //	  printf("\nRelanzo CONSUMER isid_stack %d++ valor %p\n",
	      //		 isid_stack,aux->consumer->parent_id);
	      
	      sid_stack[isid_stack++] = aux->consumer->parent_id; 
	      
	      //Variables for the next execution of nd_resume_cons_c
	      Arg->node->term[0] = (ENGINE_Term)sf;
	      Arg->node->term[1] = (ENGINE_Term)aux;
	      
	      //Reinstalling the trail
	      int iTrail;
	      for (iTrail = 0; iTrail < aux->consumer->ntrail; iTrail = iTrail + 2)
		{
		  if (!ENGINE_Unify(aux->consumer->trail[iTrail],
				    aux->consumer->trail[iTrail + 1]))
		    {
		      printf("\nPROBLEMAS IX\n");		  
		    }
		}
	      
	      //Reinstalling subtitution factor from answer
	      
	      struct subs_factor *answer = get_trie_answer(Arg,Answers);
	      int iVars;
	      for (iVars = 0; iVars < answer->size; iVars++)
		if (!ENGINE_Unify(aux->consumer->sub_fact->vars[iVars],answer->vars[iVars])) 
		  {
		    printf("\nPROBLEMAS X\n");		  
		    return FALSE;
		  }
	      
	      Arg->next_insn = aux->consumer->next_insn;
	      Arg->frame = aux->consumer->frame;
	      return TRUE;
	    }
	}
    }

  sf = NULL;
  do
    {
      if (sf == NULL) sf = TOP_SF;
      else sf = sf->previous;
      
      for (aux = sf->cons; aux != NULL; aux = aux->sig)
	{
	  if (aux == NULL) continue;
	  
	  trie_node_t *Answers = aux->consumer->last_answer;
	  if ((sf->last_answer != NULL) && (Answers != sf->last_answer))
	    {//if first_ans of first_consumer is NULL, then it is NULL for the others.
	      if (Answers == NULL) Answers = sf->first_answer;
	      else Answers = Answers->child;
	      aux->consumer->last_answer = Answers;
	      
//	      printf("\nConsumer %p de %p-%p lee respuesta %p\n", 
//		     aux->consumer,sf,aux->consumer->parent_id,Answers);

	      //Updating completion stack
//	      printf("\nRelanzo CONSUMER isid_stack %d++ valor %p\n",
//		 isid_stack,aux->consumer->parent_id);

	      sid_stack[isid_stack++] = aux->consumer->parent_id; 
	      
	      //Variables for the next execution of nd_resume_cons_c
	      Arg->node->term[0] = (ENGINE_Term)sf;
	      Arg->node->term[1] = (ENGINE_Term)aux;
	      
	      //Reinstalling the trail
	      int iTrail;
	      for (iTrail = 0; iTrail < aux->consumer->ntrail; iTrail = iTrail + 2)
		{
		  if (!ENGINE_Unify(aux->consumer->trail[iTrail],
				    aux->consumer->trail[iTrail + 1]))
		    {
		      printf("\nPROBLEMAS XI\n");		  
		    }
		}
	      
	      //Reinstalling subtitution factor from answer
	      
	      struct subs_factor *answer = get_trie_answer(Arg,Answers);
	      int iVars;
	      for (iVars = 0; iVars < answer->size; iVars++)
		if (!ENGINE_Unify(aux->consumer->sub_fact->vars[iVars],answer->vars[iVars])) 
		  {
		    printf("\nPROBLEMAS XII\n");		  
		    return FALSE;
		  }
	      
	      Arg->next_insn = aux->consumer->next_insn;
	      Arg->frame = aux->consumer->frame;
	      return TRUE;
	    }
	}
    }
  while(sf != sid_stack[isid_stack-1]);

//  printf("\nVe si completo %p\n",sid_stack[isid_stack-1]);
  complete(sid_stack[isid_stack-1],sid_stack[isid_stack-1]);
  pop_choicept(Arg);

//  printf("\nTermina GENERADOR isid_stack %d--\n",isid_stack);

  isid_stack--;
  //Updating dependences between generators
  if (isid_stack && sid_stack[isid_stack]->comp != COMPLETE) 
    {
//      printf("\n Entra2 y con %d\n",sid_stack[isid_stack]->comp);
      SETMIN(sid_stack[isid_stack - 1]->plink, sid_stack[isid_stack]->plink);  
//  printf("\nPLINK de GENERATOR %p es %p\n", sid_stack[isid_stack - 1],
//	 sid_stack[isid_stack - 1]->plink); fflush(stdout);
    }

  struct EACH_CALL *callid = sid_stack[isid_stack];
//      printf("\nCojo callid %p\n",callid);

  //Reading actual answers
  if (callid->comp == COMPLETE)
    {
      if (callid->first_answer != NULL)
	{
	  TrNode node = callid->first_answer;
	  
	  push_choicept(Arg,address_nd_consume_answer_c);
	  Arg->node->term[0] = (ENGINE_Term)callid->sub_fact;
	  Arg->node->term[1] = (ENGINE_Term)node;
	  Arg->node->term[2] = (ENGINE_Term)callid;
	  
	  struct subs_factor *answer = get_trie_answer(Arg,node);

	  int iVars;
	  for (iVars = 0; iVars < answer->size; iVars++)
	    if (!ENGINE_Unify(callid->sub_fact->vars[iVars],answer->vars[iVars])) 
	      {
		printf("\nPROBLEMAS XIII\n");		  
		return FALSE;
	      }
	  
	  return TRUE;        
	}
      return FALSE;
    }


//  printf("\nCREAMOS CONSUMER III\n");
//  printf("\n%p NO esta completo, padre consumidor %p\n", callid,sid_stack[isid_stack - 1]); 

  struct consumer* consumer = get_consumer(Arg, callid->sub_fact, sid_stack[isid_stack - 1]);
  consumer->last_answer = NULL;

  if (callid->last_cons == NULL)
    {
      callid->cons = (struct cons_list*) malloc (sizeof(struct cons_list));
      callid->cons->consumer = consumer;
      callid->cons->sig = NULL;
      callid->last_cons = callid->cons;
    }
  else
    {
      callid->last_cons->sig = (struct cons_list*) malloc (sizeof(struct cons_list));
      callid->last_cons = callid->last_cons->sig;
      callid->last_cons->consumer = consumer;
      callid->last_cons->sig = NULL;
    }

  return FALSE;
}

bool_t set_diff_answer_trie(void)
{
//  printf("\nset_diff\n"); fflush(stdout);
  struct EACH_CALL *call = sid_stack[isid_stack - 1];
  TrNode node;

  node = put_trie_answer(call->trie_answers, call->sub_fact);

  if (node->child == NULL && node != call->last_answer)
    {
      if (call->first_answer == NULL) call->first_answer = node;

      if(call->last_answer != NULL) call->last_answer->child = node;
      
      call->last_answer = node;
//      printf("\nNew answer %p en %p\n",node,call);
      return TRUE;
    }
  
  return FALSE;
}

bool_t new_answer_c(Arg)
     Argdecl;
{
//  printf("\nnew_answer\n"); fflush(stdout);

//  if (set_diff_answer_trie(callid, ENGINE_ARG1)) 
//    {
//      insert_answer(Arg, callid, ENGINE_ARG1); freezing on the heap
//    }

  set_diff_answer_trie();
  return FALSE;  
}

bool_t check_c(Arg)
     Argdecl;
{
  printf("\nCHECKING\n");
  printf("\nLOCAL       %p\n", Arg->local_top);
  printf("\nNode->frame %p\n", Arg->frame);
  printf("\nNode y Node->local_top %p - %p y stack %p\n", 
	 Arg->node, Arg->node->local_top, stack_freeze);
  printf("\nNode->frame[1] %p\n", Arg->frame->term[0]);
  printf("\nNode->frame[2] %p\n", Arg->frame->term[1]);
  printf("\nNode->frame[3] %p\n", Arg->frame->term[2]);
  printf("\nX(0) %p\n", X(0));
  printf("\nX(1) %p\n", X(1));
  printf("\nX(2) %p\n", X(2));
  printf("\nTOP_SF %p\n", TOP_SF);

//  ENGINE_Term ttt;
//  DEREF(ttt,Arg->frame->term[0]);
//  printf("\nDEREF-Node->frame[1] %p\n", ttt);
//  ENGINE_Term ttt2;
//  DEREF(ttt2,Arg->frame->term[1]);
//  printf("\nDEREF-Node->frame[2] %p\n", ttt2);
//  ENGINE_Term ttt3;
//  DEREF(ttt3,Arg->frame->term[2]);
//  printf("\nDEREF-Node->frame[3] %p\n", ttt3);
//  ENGINE_Term ttt4;
//  DEREF(ttt4,X(0));
//  printf("\nDEREF-X(0) %p\n", ttt4);
//  ENGINE_Term ttt5;
//  DEREF(ttt5,X(1));
//  printf("\nDEREF-X(1) %p\n", ttt5);
//  ENGINE_Term ttt6;
//  DEREF(ttt6,X(2));
//  printf("\nDEREF-X(2) %p\n", ttt6);

  return TRUE;  
}

bool_t initial_c(Arg)
     Argdecl;
{
  ini = NULL;
  isid_stack = 0;
  lider_trail = NULL;
  memory_free = &memory[0];
  node_top = NULL;
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,3);
  address_nd_resume_cons_c = def_retry_c(nd_resume_cons_c,2);
  address_nd_prueba_c = def_retry_c(nd_prueba_c,0);
  init_tries_module();
  return TRUE;
}

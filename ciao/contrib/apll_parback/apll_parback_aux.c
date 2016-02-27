#if defined(PARBACK)

tagged_t my_make_var(Arg)
     Argdecl;
{
  ciao_ensure_heap(Arg->misc->goal_desc_ptr, 1);
  tagged_t resul = TagHVA(Arg->global_top);
  HeapPush(Arg->global_top, resul);
  return resul;
}			      

tagged_t my_make_list(Arg, head, tail) 
     Argdecl;
     tagged_t head;
     tagged_t tail;
{
  tagged_t list;
  ciao_ensure_heap(Arg->misc->goal_desc_ptr, 3);
  MakeLST(list, head, tail);
  return list;
}

/* Ensure memory in hte private handler memory */
int ensure_memory(par_goal_t *goal, int size)
{
  tagged_t *prev = goal->memory;
  if ((goal->mem_size - goal->free) < size)
    {
      goal->memory = (tagged_t*) checkrealloc (goal->memory, goal->mem_size*sizeof(tagged_t),
					      goal->mem_size + MEMSIZE*sizeof(tagged_t));
      if (prev != goal->memory)
	{
	  printf("\nESTAMOS AGRANDANDO MEMORIA\n");
	  int i;
	  for (i = 0; i < goal->free; i++)
	    if (VarBelongsToMemoryGoal(goal->memory + i,*goal) || TagIsLST(goal->memory[i]) ||
		(TagIsSTR(goal->memory[i]) && !TagIsSmall(goal->memory[i])))
	      goal->memory[i] += (tagged_t)(goal->memory - prev);
	}	
      goal->mem_size += MEMSIZE;
    }
  return goal->memory - prev;
}

/* Makes a ground term using the SF of the answer in the private handler memory */
void make_ground(par_goal_t *goal, AL_t *ans, tagged_t *heap_end)
{
  tagged_t *index, *top;
  top = index = goal->memory + goal->free;
  tagged_t t;
  int i;

  //Pushing the answer unified variables
  for (i = 0; i < ans->numVars; i++)
    {
      DEREF(t,ans->vars[i]);
      HeapPush(top,t);
    }

  tagged_t *i_term;
  int arity;
  int iarray = 0;

  while(HeapYounger(top,index))
    {
      t = *index;
      if (t == INTEGER_MARK) index = index + 3;
      else if (t == FLOAT_MARK) index = index + 4;
      else if (IsVar(t) || IsTrieVar(t))
	{
	  DEREF(t,t);
	  if (IsTrieVar(t))  
	    {
	      HeapPush(index, goal->array[TrieVarIndex(t)]);
	    }
	  else if (IsVar(t)) 
	    {
	      if (HeapYounger(goal->ch_init->global_top,TagToPointer(t)) || HeapYounger(heap_end,TagToPointer(t))) index++;
	      else
		{
		  if (iarray + 2 >= TERMSIZE)
		    fprintf(stderr, "\nmemo module: TERM_STACK full");
		  
		  goal->array[iarray++] = (unsigned int)index - MallocBase;
		  HeapPush(index, (unsigned int)index - MallocBase);
		  goal->array[iarray] = t;
		  CTagToPointer(t) = VarTrie | ((iarray-1) << 1);
		  iarray++;
		}
	    }
	  else *index = t;	  
	}
      else if (TagIsLST(t))
	{
	  HeapPush(index, 0xc0000000 + ((unsigned int)top - MallocBase));
	  tagged_t *list = TagToPointer(t);
	  int offset = ensure_memory(goal, 2);
	  index +=offset; top += offset;
	  HeapPush(top, *list);
	  list++;
	  HeapPush(top, *list);
	}
      else if (TagIsSTR(t)) 
	{
	  if (IsNumber(t))
	    {
	     if (TagIsSmall(t)) index++; 
	     else
	       {
		 if (!LargeIsFloat(t)) //large integer
		   {
		     HeapPush(index, 0xe0000000 + ((unsigned int)top - MallocBase));
		     int offset = ensure_memory(goal, 3);
		     index +=offset; top += offset;
		     HeapPush(top, INTEGER_MARK);
		     HeapPush(top, *(TagToPointer(t) + 1));
		     HeapPush(top, INTEGER_MARK);
		   }
		 else //float
		   {
		     HeapPush(index, 0xe0000000 + ((unsigned int)top - MallocBase));
		     int offset = ensure_memory(goal, 4);
		     index +=offset; top += offset;
		     HeapPush(top, FLOAT_MARK);
		     HeapPush(top, *(TagToPointer(t) + 1));
		     HeapPush(top, *(TagToPointer(t) + 2));
		     HeapPush(top, FLOAT_MARK);
		   }
	       }
	    }
	  else
	    {
	      HeapPush(index, 0xe0000000 + ((unsigned int)top - MallocBase));
	      i_term = TagToPointer(t);
	      arity = ArityOfFunctor(t);
	      int offset = ensure_memory(goal, arity + 1);
	      index +=offset; top += offset;
	      for (i = 0; i <= arity; i++)
		{
		  HeapPush(top, *i_term);
		  i_term++;
		}
	    }
	}
      else index++;
    }

  goal->free = (unsigned int)(index - goal->memory);

  //Reinstall original values of variables.
  int j;
  for(j = 1; j < iarray; j = j + 2) CTagToPointer(goal->array[j]) = goal->array[j];
}

/* Adds handler to the goal list */
void add_handler_to_goal_list(Arg, h)
     Argdecl;
     par_goal_t *h;
{
  if (h->list == GOAL) printf("\nPROBLEMAS ADD HANDLER GOAL LIST\n");  
  if (Goal_List_Top != NULL) 
    {
      h->gle->prev = Goal_List_Top;
      Goal_List_Top->next = h->gle;
      Goal_List_Start->prev = h->gle;
      Goal_List_Top = h->gle;
    }
  else 
    {
      Goal_List_Start = h->gle;
      Goal_List_Top = h->gle;
      h->gle->prev = Goal_List_Top;
    }
  h->gle->next = Goal_List_Start;
  h->list = GOAL;
}

/* Removes handler from the goal list */
void remove_handler_from_goal_list(Arg, h)
     Argdecl;
     par_goal_t *h;
{
//  if (h->list != GOAL) printf("\nPROBLEMAS REMOVE HANDLER GOAL LIST\n");
  if (Goal_List_Start == Goal_List_Top) 
    {
      Goal_List_Start = NULL;
      Goal_List_Top = NULL;
    }
  else 
    {
      h->gle->next->prev = h->gle->prev;
      h->gle->prev->next = h->gle->next;
      if (h->gle == Goal_List_Top) Goal_List_Top = h->gle->prev;
      if (h->gle == Goal_List_Start) Goal_List_Start = h->gle->next;
    }
  h->list = NONE;
}

/* Adds handler to the cancel goal list */
void add_handler_to_cancel_goal_list(Arg, h)
     Argdecl;
     par_goal_t *h;
{
  if (h->list == CANCEL) printf("\nPROBLEMAS ADD HANDLER CANCEL LIST\n");

  if (Cancel_Goal_List_Top != NULL) 
    {
      h->gle->prev = Cancel_Goal_List_Top;
      Cancel_Goal_List_Top->next = h->gle;
      Cancel_Goal_List_Start->prev = h->gle;
      Cancel_Goal_List_Top = h->gle;
    }
  else 
    {
      Cancel_Goal_List_Start = h->gle;
      Cancel_Goal_List_Top = h->gle;
      h->gle->prev = Cancel_Goal_List_Top;
    }
  h->gle->next = Cancel_Goal_List_Start;
  h->list = CANCEL;
}


/* Removes handler from the cancel goal list */
void remove_handler_from_cancel_goal_list(Arg, h)
     Argdecl;
     par_goal_t *h;
{
//  if (h->list != CANCEL) printf("\nPROBLEMAS REMOVE HANDLER CANCEL LIST\n");
  if (Cancel_Goal_List_Start == Cancel_Goal_List_Top) 
    {
      Cancel_Goal_List_Start = NULL;
      Cancel_Goal_List_Top = NULL;
    }
  else 
    {
      h->gle->next->prev = h->gle->prev;
      h->gle->prev->next = h->gle->next;
      if (h->gle == Cancel_Goal_List_Top) Cancel_Goal_List_Top = h->gle->prev;
      if (h->gle == Cancel_Goal_List_Start) Cancel_Goal_List_Start = h->gle->next;
    }
  h->list = NONE;
}

/* Adds handler to the back goal list */
void add_handler_to_back_goal_list(Arg, h)
     Argdecl;
     par_goal_t *h;
{
  if (h->list == BACK) printf("\nPROBLEMAS ADD HANDLER BACK LIST\n");  
  if (Back_Goal_List_Top != NULL) 
    {
      h->gle->prev = Back_Goal_List_Top;
      Back_Goal_List_Top->next = h->gle;
      Back_Goal_List_Start->prev = h->gle;
      Back_Goal_List_Top = h->gle;
    }
  else 
    {
      Back_Goal_List_Start = h->gle;
      Back_Goal_List_Top = h->gle;
      h->gle->prev = Back_Goal_List_Top;
    }
  h->gle->next = Back_Goal_List_Start;
  h->list = BACK;
}

/* Removes handler from the back goal list */
void remove_handler_from_back_goal_list(Arg, h)
     Argdecl;
     par_goal_t *h;
{
//  if (h->list != BACK) printf("\nPROBLEMAS REMOVE HANDLER BACK LIST\n");  
  if (Back_Goal_List_Start == Back_Goal_List_Top) 
    {
      Back_Goal_List_Start = NULL;
      Back_Goal_List_Top = NULL;
    }
  else 
    {
      h->gle->next->prev = h->gle->prev;
      h->gle->prev->next = h->gle->next;
      if (h->gle == Back_Goal_List_Top) Back_Goal_List_Top = h->gle->prev;
      if (h->gle == Back_Goal_List_Start) Back_Goal_List_Start = h->gle->next;
    }
  h->list = NONE;
}

#endif

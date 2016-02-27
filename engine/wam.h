/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#if defined(DEBUG)
   /* stop_on_pred_calls = predtrace | profiling */
#if defined(PROFILE)
#define PredTrace(X,Y)	\
if (stop_on_pred_calls) {  \
     if (predtrace) wr_functor(X,Y); \
     else add_to_profiling(Y); }
#else /* DEBUG & !PROFILE */
#define PredTrace(X,Y)	if (predtrace) wr_call(Arg,X,Y)
#endif
#else
#define PredTrace(X,Y)	
#endif

/* can use H when live */
#if MallocBase
#define TagIsHVAw(X)	TagIsHVA(X)
#else
#define TagIsHVAw(X)	(!OffHeaptop(X,TagHVA(H))) 
#endif

#define P		p
#define B		((struct node *)pt1)
#define SetB(X)		((struct node *)(pt1 = (TAGGED *)(X)))
#define E		((struct frame *)pt1)
#define SetE(X)		((struct frame *)(pt1 = (TAGGED *)(X)))
#define H		pt2
#define LoadH		(H = w->global_top)
#define StoreH		(w->global_top = H)
#define S		pt2
#define Func		((struct definition *)P)
#define Setfunc(X)	(P = (INSN *)(&(X)->enter_instr))
#define Htab		((struct sw_on_key *)pt1)
#define SetHtab(X)	(pt1 = (TAGGED *)(X))
#define HtabNode	((struct sw_on_key_node *)P)
#define SetHtabNode(X)	(P = (INSN *)(X))
#define Alts		((struct try_node *)pt1)
#define SetAlts(X)	(pt1 = (TAGGED *)(X))

#define Pdeep		((INSN *)(*P + (char *)P))
#define Pshallow	(P+1)

/* shorthands for bytecode operands */
#define Pplus0		(P-1)
#define Pplus1		P
#define Pplus2		(P+1)
#define Pplus3		(P+2)
#define Pplus4		(P+3)
#define Pplus5		(P+4)
#define Pplus6		(P+5)
#define Pplus7		(P+6)
#define Pplus8		(P+7)
#define Pplus9		(P+8)

/* get around bug in cc on IBM PC-RT and on Gould NP1 */
#if aiws || ibm032 || gould
#define Pplus2align	(ptemp=P+1)
#define Pplus4align	(ptemp=P+3)
#define Pplus6align	(ptemp=P+5)
#else
#define Pplus2align	(P+1)
#define Pplus4align	(P+3)
#define Pplus6align	(P+5)
#endif

#define OPCODE		(*P++)
#define SPnext		(*(short int *)P++)
#define SP0		(*(short int *)Pplus0)
#define SP1		(*(short int *)Pplus1)
#define SP2		(*(short int *)Pplus2)
#define SP3		(*(short int *)Pplus3)
#define SP4		(*(short int *)Pplus4)
#define SP5		(*(short int *)Pplus5)
#define SP6		(*(short int *)Pplus6)
#define SP7		(*(short int *)Pplus7)
#define SP8		(*(short int *)Pplus8)
#define SP9		(*(short int *)Pplus9)
#define Pnext		(*P++)
#define P0		(*Pplus0)
#define P1		(*Pplus1)
#define P2		(*Pplus2)
#define P3		(*Pplus3)
#define P4		(*Pplus4)
#define P5		(*Pplus5)
#define P6		(*Pplus6)
#define P7		(*Pplus7)
#define P8		(*Pplus8)
#define P9		(*Pplus9)
#define T1		(*(TAGGED *)Pplus1)
#define T2		(*(TAGGED *)Pplus2align)
#define T3		(*(TAGGED *)Pplus3)
#define C1		(**(CInfo *)Pplus1)
#define C2		(**(CInfo *)Pplus2align)
#define C3		(**(CInfo *)Pplus3)
#define C4		(**(CInfo *)Pplus4align)
#define C5		(**(CInfo *)Pplus5)
#define BP1		(*(INSN **)Pplus1)
#define BP2		(*(INSN **)Pplus2align)
#define LP1		(*(long *)Pplus1)
#define LP2		(*(long *)Pplus2align)
#define LP3		(*(long *)Pplus3)
#define LP4		(*(long *)Pplus4align)
#define LP5		(*(long *)Pplus5)
#define LP6		(*(long *)Pplus6align)
#define LP7		(*(long *)Pplus7)

#define DISPATCH_R(Incr)	{ P += Incr;  goto ReadMode; }
#define DISPATCH_W(Incr)	{ P += Incr;  goto WriteMode; }

/* segfault patch -- jf */
/*  'U' is a 'Yb(I)' expression. */
#define GetFirstValue(U,V) \
{ \
  if (CondStackvar(U)) \
    { \
      TrailPushCheck(w->trail_top,TagSVA(&U)); \
      BindingOfStackvar(U) = V; \
    } \
  else \
    U = V; \
}

#define Unify_local_value \
{ \
  if (TagIsSVA(t1)) \
    do \
      { \
	RefSVA(t0,t1); \
	if (t0 == t1) \
	  { \
	    BindSVA(t1,TagHVA(H)); \
	    PreLoadHVA(t1,H); \
	    break; \
	  } \
      } \
    while (TagIsSVA(t1=t0)); \
  HeapPush(H,t1); \
}

#define Unify_heap_atom(U,V) \
{ \
  t1=V; \
  SwitchOnHeapVar(t1,t0,{BindHVA(t1,U);}, \
                        {BindCVA(t1,U);Wake;}, \
		        {if (t1!=U) goto fail;}) \
}

#define Unify_atom(U,V) \
{ \
  t1=V; \
  SwitchOnVar(t1,t0,{BindHVA(t1,U);}, \
	            {BindCVA(t1,U);Wake;}, \
	            {BindSVA(t1,U);}, \
		    {if (t1!=U) goto fail;}) \
}

#define Unify_atom_internal(Atom,Var) \
{ \
  t1=Var; \
  if (t1 & TagBitSVA) BindSVA(t1,Atom) \
  else BindHVA(t1,Atom) \
}


#define Unify_heap_structure(U,V,RC,WC) \
{ \
  t1=V; \
  SwitchOnHeapVar(t1,t0,{LoadH; BindHVA(t1,Tag(STR,H)); HeapPush(H,U); WC}, \
		        {LoadH; BindCVA(t1,Tag(STR,H)); HeapPush(H,U); Wake; WC}, \
		        {if(!TagIsSTR(t1) || (TagToHeadfunctor(t1)!=U)) goto fail; \
			 S = TagToArg(t1,1); RC}) \
}

#define Unify_structure(U,V,RC,WC) \
{ \
  t1=V; \
  SwitchOnVar(t1,t0,{LoadH; BindHVA(t1,Tag(STR,H)); HeapPush(H,U); WC}, \
		    {LoadH; BindCVA(t1,Tag(STR,H)); HeapPush(H,U); Wake; WC}, \
		    {LoadH; BindSVA(t1,Tag(STR,H)); HeapPush(H,U); WC}, \
	            {if(!TagIsSTR(t1) || (TagToHeadfunctor(t1)!=U)) goto fail; \
		     S = TagToArg(t1,1); RC}) \
}

#define Unify_heap_large(ARG,P, T) \
{ \
  t1=T; \
  SwitchOnHeapVar(t1,t0,{BindHVA(t1,MakeLarge(ARG,P));}, \
		 {BindCVA(t1,MakeLarge(ARG,P)); Wake;}, \
	         {if (!TagIsSTR(t1)) goto fail; \
		  for (i=LargeArity(*(TAGGED *)(P)); i>0; i--) \
		    if (((TAGGED *)(P))[i-1] != CTagToArg(t1,i-1)) goto fail;}) \
}

#define Unify_large(ARG,P, T) \
{ \
  t1=T; \
  SwitchOnVar(t1,t0,{BindHVA(t1,MakeLarge(ARG,P));}, \
		 {BindCVA(t1,MakeLarge(ARG,P)); Wake;}, \
		 {BindSVA(t1,MakeLarge(ARG,P));}, \
	         {if (!TagIsSTR(t1)) goto fail; \
		  for (i=LargeArity(*(TAGGED *)(P)); i>0; i--) \
		    if (((TAGGED *)(P))[i-1] != CTagToArg(t1,i-1)) goto fail;}) \
}

#define Unify_heap_list(V,RC,WC) \
{ \
  t1=V; \
  SwitchOnHeapVar(t1,t0,{LoadH; BindHVA(t1,Tag(LST,H)); WC}, \
		        {LoadH; BindCVA(t1,Tag(LST,H)); Wake; WC}, \
		        {if(!TermIsLST(t1)) goto fail; \
			 S = TagToLST(t1); RC}) \
}

#define Unify_list(V,RC,WC) \
{ \
  t1=V; \
  SwitchOnVar(t1,t0,{LoadH; BindHVA(t1,Tag(LST,H)); WC}, \
		    {LoadH; BindCVA(t1,Tag(LST,H)); Wake; WC}, \
		    {LoadH; BindSVA(t1,Tag(LST,H)); WC}, \
	            {if(!TermIsLST(t1)) goto fail; \
		     S = TagToLST(t1); RC}); \
}


#define TRYEACH_R(List) \
{ SetAlts(List); goto tryeach_r; }

#define TRYEACH_W(List) \
{ SetAlts(List); goto tryeach_w; }


#if defined(DEBUG)
#define REPORT_CLEANUP(TopCChpt, TopNode) \
 if(debug_concchoicepoints) \
    fprintf(stderr, "cut: removing chains (%x to %x)\n", \
                    (int)TopCChpt, (int)TopNode);

#define REPORT_CUT(NewNode) \
 if (debug_choicepoints)  \
   fprintf(stderr, "Cutting: new chpt = %x\n", (int)NewNode);
#else
#define REPORT_CUT(NewNode)
#define REPORT_CLEANUP(TopCChpt, TopNode)
#endif

#if defined(THREADS)
#define   ConcChptCleanUp(TopCChpt, TopNode) \
    if (ChoiceYounger(TopCChpt, TopNode)) { \
       REPORT_CLEANUP(TopCChpt, TopNode); \
       remove_link_chains(&TopCChpt, TopNode); \
    } 
#else
#define    ConcChptCleanUp(TopCChpt, TopNode)
#endif

/* Concurrency: if we cut (therefore discarding intermediate
   choicepoints), make sure we also get rid of the linked chains which
   point to the pending calls to concurrent predicates. (MCL) */

#define DOCUT \
{ \
    w->node = SetB(w->next_node); \
    SetShadowregs(B); \
    REPORT_CUT(w->node); \
    ConcChptCleanUp(TopConcChpt, w->node); \
}

#define PUT_YVOID \
{ \
  t0 = P1, P++; \
  LoadSVA(Yb(t0)) \
}

/* Must return value in t0.  Second arg must not involve t0.  */
#define RefStackUnsafe(To,From) \
{ \
  RefStack(t0,From); \
  if(TagIsSVA(t0)) \
      do \
	{ \
	  RefSVA(t1,t0); \
	  if (t1 == t0) \
	    { \
	      if (UnsafeVar(t0)) \
		{ \
		  LoadHVA(t0,H); BindSVA(t1,t0); \
		} \
	      break; \
	    } \
	} \
      while (TagIsSVA(t0=t1)); \
  To = t0; \
}

#define CUNIFY(U,V) \
{ \
  t0 = U; t1 = V; \
  if (t0!=t1) \
    { \
      if (!cunify(Arg,t0,t1)) goto fail; \
    } \
}

#define EUNIFY(U,V,Incr) \
{ \
  t0 = U, t1 = V, P += Incr; \
  goto unify_t0_t1; \
}


/* This must not clobber  t2, X[*].  Build goal, store in t3.  */
#define EMUL_TO_GOAL \
{ \
  if (Func->arity==0) \
    t3 = Func->printname; \
  else \
    { \
/*       fprintf(stderr, "arity is %d\n", Func->arity); */ \
      t3 = Tag(STR,H); \
/*       fprintf(stderr, "2\n"); */ \
      HeapPush(H,SetArity(Func->printname,Func->arity)); \
/*       fprintf(stderr, "3\n"); */ \
      for(i=0; i<Func->arity; i++) \
	U1_XLVALC_W(i); \
/*       fprintf(stderr, "4\n"); */ \
    } \
}

#define DEALLOCATE \
{ \
  w->next_insn = E->next_insn; \
  w->frame = E->frame; \
}

#define PROCED \
{ \
  w->local_top = 0; \
  SetE(w->frame); \
  P = w->next_insn; \
}

#define UnsafeVar(X)		(!YoungerStackVar(TagSVA(Offset(E,EToY0)),X))

#define SetA(Frame,Ptr) \
{ \
  w->local_top = (struct frame *)(Ptr); \
}

/* Do not edit this defn - it's a special case of ComputeA. */
#define ComputeE \
{ \
  if (w->local_top) \
    SetE(w->local_top); \
  else if (!StackYounger(SetE(w->node->local_top),w->frame)) \
    SetE(StackCharOffset(w->frame,FrameSize(w->next_insn))); \
}

/* Do not edit this defn - it's the special case 
   ComputeA(w->local_top,B). */
#define ComputeLtop(B) \
{ \
  if (w->local_top) \
    ; \
  else if (!StackYounger(w->local_top = (B)->local_top,w->frame)) \
    w->local_top = StackCharOffset(w->frame,FrameSize(w->next_insn)); \
}


#define SETUP_PENDING_CALL(ADDR) \
{ \
	  ComputeE; \
	  Y(0) = PointerToTerm(Func); \
	  for(i=0; i<Func->arity; i++) Y(i+1) = X(i); \
	  E->next_insn = w->next_insn; \
	  E->frame = w->frame; \
	  w->frame = E; \
	  w->next_insn = &contcode[(1+LOffset)*(i+1)]; \
	  SetA(E,Offset(E,EToY0+i+1)); \
	  Setfunc(ADDR); \
}


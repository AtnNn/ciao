/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

 case BUMP_COUNTERQ:
  P++;
 case BUMP_COUNTER:

#if GAUGE
  INCR_COUNTER(*(ENG_INT **)P);
#endif

   DISPATCH_R(BPL);

 case COUNTED_NECKQ:
   P++;
 case COUNTED_NECK:
#if GAUGE
    if (w->next_alt)
     {
       SetB(w->node);
       if (B->next_alt)
	 {			/* retry */
	   INCR_COUNTER(*((ENG_INT **)P + 1));
	   B->next_alt = w->next_alt;
	 }
       else
	 {			/* try */
	   INCR_COUNTER(*(ENG_INT **)P);
	   B->next_alt = w->next_alt; /* 4 contiguous moves */
	   B->frame = w->frame;
	   B->next_insn = w->next_insn;
	   SaveLtop(B);
	   i=B->next_alt->node_offset;
	   if (i>ArityToOffset(0))
	     {
	       i = OffsetToArity(i);
	       SetB(w->next_node);
	       do
		 ChoicePush(pt1,(w->term-1)[i]);
	       while (--i);
	     }
	   if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))
	     choice_overflow(Arg,CHOICEPAD);
	 }
       w->next_alt = NULL;
       SetE(w->local_top);	/* OK even before allocate */
     }
   DISPATCH_R(4);
#else
   P += 4;
   goto neck_r;
#endif /* GAUGE */

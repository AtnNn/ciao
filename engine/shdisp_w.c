/* Copyright (C) 1996,1997,1998, UPM-CLIP */

 case INITTRUE:
 inittrue:
		ComputeE;
		for (t0 = P1-sizeof(TAGGED);
		     t0 >= EToY0*sizeof(TAGGED);
		     t0 -= sizeof(TAGGED))
		  {
		    LoadSVA(Yb(t0));
		  }
		goto firsttrue;

 case FIRSTTRUE_N:
 firsttrue_n:
		for (i = SP1, P++; i>0; --i)
		  PUT_YVOID;
 firsttrue:
		E->next_insn = w->next_insn;
		E->frame = w->frame;
		w->frame = E;
		w->next_insn = Pplus2;
		w->local_top = StackCharOffset(E,P1);
		if (OffStacktop(E,Stack_Warn))
		  SetEvent;
		DISPATCH_W(1);

 case INITCALLQ:
		P++;
 case INITCALL:
 initcall:
		ComputeE;
		for (t0 = *(P+BPTP)-sizeof(TAGGED);
		     t0 >= EToY0*sizeof(TAGGED);
		     t0 -= sizeof(TAGGED))
		  {
		    LoadSVA(Yb(t0));
		  }
		goto firstcall;

 case FIRSTCALL_NQ:
		P++;
		goto firstcall_n;

 case FIRSTCALL_8Q:
		P++;
		goto firstcall_8;

 case FIRSTCALL_7Q:
		P++;
		goto firstcall_7;

 case FIRSTCALL_6Q:
		P++;
		goto firstcall_6;

 case FIRSTCALL_5Q:
		P++;
		goto firstcall_5;

 case FIRSTCALL_4Q:
		P++;
		goto firstcall_4;

 case FIRSTCALL_3Q:
		P++;
		goto firstcall_3;

 case FIRSTCALL_2Q:
		P++;
		goto firstcall_2;

 case FIRSTCALL_1Q:
		P++;
		goto firstcall_1;

 case FIRSTCALLQ:
		P++;
		goto firstcall;

 case CALL_NQ:
		P++;
		goto call_n;

 case CALL_8Q:
		P++;
		goto call_8;

 case CALL_7Q:
		P++;
		goto call_7;

 case CALL_6Q:
		P++;
		goto call_6;

 case CALL_5Q:
		P++;
		goto call_5;

 case CALL_4Q:
		P++;
		goto call_4;

 case CALL_3Q:
		P++;
		goto call_3;

 case CALL_2Q:
		P++;
		goto call_2;

 case CALL_1Q:
		P++;
		goto call_1;

 case CALLQ:
		P++;
		goto call;

 case LASTCALL_NQ:
		P++;
		goto lastcall_n;

 case LASTCALL_8Q:
		P++;
		goto lastcall_8;

 case LASTCALL_7Q:
		P++;
		goto lastcall_7;

 case LASTCALL_6Q:
		P++;
		goto lastcall_6;

 case LASTCALL_5Q:
		P++;
		goto lastcall_5;

 case LASTCALL_4Q:
		P++;
		goto lastcall_4;

 case LASTCALL_3Q:
		P++;
		goto lastcall_3;

 case LASTCALL_2Q:
		P++;
		goto lastcall_2;

 case LASTCALL_1Q:
		P++;
		goto lastcall_1;

 case LASTCALLQ:
		P++;
		goto lastcall;


 case FIRSTCALL_N:
 firstcall_n:
		for (i = SP1, P++; i>8; --i)
		  PUT_YVOID;
 case FIRSTCALL_8:
 firstcall_8:
		PUT_YVOID;
 case FIRSTCALL_7:
 firstcall_7:
		PUT_YVOID;
 case FIRSTCALL_6:
 firstcall_6:
		PUT_YVOID;
 case FIRSTCALL_5:
 firstcall_5:
		PUT_YVOID;
 case FIRSTCALL_4:
 firstcall_4:
		PUT_YVOID;
 case FIRSTCALL_3:
 firstcall_3:
		PUT_YVOID;
 case FIRSTCALL_2:
 firstcall_2:
		PUT_YVOID;
 case FIRSTCALL_1:
 firstcall_1:
		PUT_YVOID;
 case FIRSTCALL:
 firstcall:
		E->next_insn = w->next_insn;
		E->frame = w->frame;
		w->frame = E;
		w->next_insn = P+BPTP+LOffset;
		w->local_top = StackCharOffset(E,*(P+BPTP));
		P = BP1;
		if (OffStacktop(E,Stack_Warn))
		  SetEvent;
		goto enter_predicate;

 case CALL_N:
 call_n:
		for (i = SP1, P++; i>8; --i)
		  {
		    t1 = P1, P++;
		    if (t1&1)
		      {
			RefStackUnsafe(X(i-1),&Yb(t1+1));
		      }
		    else
		      RefStack(X(i-1),&Yb(t1));
		  }
 case CALL_8:
 call_8:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(7),&Yb(t1+1));
		}
		else
		  RefStack(X(7),&Yb(t1));
 case CALL_7:
 call_7:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(6),&Yb(t1+1));
		}
		else
		  RefStack(X(6),&Yb(t1));
 case CALL_6:
 call_6:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(5),&Yb(t1+1));
		}
		else
		  RefStack(X(5),&Yb(t1));
 case CALL_5:
 call_5:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(4),&Yb(t1+1));
		}
		else
		  RefStack(X(4),&Yb(t1));
 case CALL_4:
 call_4:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(3),&Yb(t1+1));
		}
		else
		  RefStack(X(3),&Yb(t1));
 case CALL_3:
 call_3:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(2),&Yb(t1+1));
		}
		else
		  RefStack(X(2),&Yb(t1));
 case CALL_2:
 call_2:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(1),&Yb(t1+1));
		}
		else
		  RefStack(X(1),&Yb(t1));
 case CALL_1:
 call_1:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(0),&Yb(t1+1));
		}
		else
		  RefStack(X(0),&Yb(t1));
 case CALL:
 call:
		w->next_insn = P+BPTP+LOffset;
		P = BP1;
		goto enter_predicate;


 case LASTCALL_N:
 lastcall_n:
		for (i = SP1, P++; i>8; --i)
		  {
		    t1 = P1, P++;
		    if (t1&1)
		      {
			RefStackUnsafe(X(i-1),&Yb(t1+1));
		      }
		    else
		      RefStack(X(i-1),&Yb(t1));
		  }
 case LASTCALL_8:
 lastcall_8:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(7),&Yb(t1+1));
		}
		else
		  RefStack(X(7),&Yb(t1));
 case LASTCALL_7:
 lastcall_7:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(6),&Yb(t1+1));
		}
		else
		  RefStack(X(6),&Yb(t1));
 case LASTCALL_6:
 lastcall_6:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(5),&Yb(t1+1));
		}
		else
		  RefStack(X(5),&Yb(t1));
 case LASTCALL_5:
 lastcall_5:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(4),&Yb(t1+1));
		}
		else
		  RefStack(X(4),&Yb(t1));
 case LASTCALL_4:
 lastcall_4:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(3),&Yb(t1+1));
		}
		else
		  RefStack(X(3),&Yb(t1));
 case LASTCALL_3:
 lastcall_3:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(2),&Yb(t1+1));
		}
		else
		  RefStack(X(2),&Yb(t1));
 case LASTCALL_2:
 lastcall_2:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(1),&Yb(t1+1));
		}
		else
		  RefStack(X(1),&Yb(t1));
 case LASTCALL_1:
 lastcall_1:
		t1 = P1, P++;
		if (t1&1)
		{
		  RefStackUnsafe(X(0),&Yb(t1+1));
		}
		else
		  RefStack(X(0),&Yb(t1));
 case LASTCALL:
 lastcall:
		DEALLOCATE;
 case EXECUTE:
		P = BP1;
		goto enter_predicate;

 case EXECUTEQ:
		P = BP2;
		goto enter_predicate;

 case PUT_X_VOID:
 put_x_void:
		LoadHVA(Xb(P1),H);
		DISPATCH_W(1);

 case PUT_X_VARIABLE:
 put_x_variable:
		Load2HVA(Xb(P1),Xb(P2),H);
		DISPATCH_W(2);

 case PUT_XVAL_XVAL:
		Xb(P1) = Xb(P2);
		Xb(P3) = Xb(P4);
		DISPATCH_W(4);

 case PUT_X_VALUE:
		Xb(P1) = Xb(P2);
		DISPATCH_W(2);

 case PUT_X_UNSAFE_VALUE:
 put_x_unsafe_value:
		RefStackUnsafe(Xb(P1),&Xb(P2));
		Xb(P2) = t0;
		DISPATCH_W(2);

 case PUT_Y_FIRST_VARIABLE:
 put_y_first_variable:
		ComputeE;
 case PUT_Y_VARIABLE:
 put_y_variable:
		t0 = P2;
		Load2SVA(Xb(P1),Yb(t0));
		DISPATCH_W(2);

 case PUT_YFVAR_YVAR:
 put_yfvar_yvar:
		ComputeE;
 case PUT_YVAR_YVAR:
 put_yvar_yvar:
		t0 = P2;
		Load2SVA(Xb(P1),Yb(t0));
		t0 = P4;
		Load2SVA(Xb(P3),Yb(t0));
		DISPATCH_W(4);

 case PUT_CONSTANTQ:
		Xb(P2) = T3;
		DISPATCH_W(2+BPTP);

 case PUT_CONSTANT:
		Xb(P1) = T2;
		DISPATCH_W(1+BPTP);

 case PUT_NIL:
		Xb(P1) = atom_nil;
		DISPATCH_W(1);

 case PUT_LARGEQ:
 put_largeq:
		StoreH;
		Xb(P2) = MakeLarge(Arg,Pplus3);
		LoadH;
		DISPATCH_W(2+LargeInsns(T3));

 case PUT_LARGE:
 put_large:
		StoreH;
		Xb(P1) = MakeLarge(Arg,Pplus2);
		LoadH;
		DISPATCH_W(1+LargeInsns(T2));

 case PUT_STRUCTUREQ:
 put_structureq:
		Xb(P2) = Tag(STR,H);
		HeapPush(H,T3);
		DISPATCH_W(2+BPTP);

 case PUT_STRUCTURE:
 put_structure:
		Xb(P1) = Tag(STR,H);
		HeapPush(H,T2);
		DISPATCH_W(1+BPTP);

 case PUT_LIST:
 put_list:
		Xb(P1) = Tag(LST,H);
		DISPATCH_W(1);

 case PUT_Y_VALUE:
		RefStack(Xb(P1),&Yb(P2));
		DISPATCH_W(2);

 case PUT_Y_UNSAFE_VALUE:
 put_y_unsafe_value:
		RefStackUnsafe(Xb(P1),&Yb(P2));
		DISPATCH_W(2);

 case PUT_YVAL_YVAL:
		RefStack(Xb(P1),&Yb(P2));
		RefStack(Xb(P3),&Yb(P4));
		DISPATCH_W(4);

 case PUT_YVAL_YUVAL:
 put_yval_yuval:
		RefStack(Xb(P1),&Yb(P2));
		RefStackUnsafe(Xb(P3),&Yb(P4));
		DISPATCH_W(4);

 case PUT_YUVAL_YVAL:
 put_yuval_yval:
		RefStackUnsafe(Xb(P1),&Yb(P2));
		RefStack(Xb(P3),&Yb(P4));
		DISPATCH_W(4);

 case PUT_YUVAL_YUVAL:
 put_yuval_yuval:
		RefStackUnsafe(Xb(P1),&Yb(P2));
		RefStackUnsafe(Xb(P3),&Yb(P4));
		DISPATCH_W(4);

 case GET_X_VALUE:
		StoreH; goto get_x_value;

 case GET_Y_FIRST_VALUE:
		StoreH; goto get_y_first_value;

 case GET_Y_VALUE:
		StoreH; goto get_y_value;

 case GET_CONSTANTQ:
		P++;
 case GET_CONSTANT:
		StoreH; goto get_constant;

 case GET_LARGEQ:
		P++;
 case GET_LARGE:
		StoreH; goto get_large;

 case GET_STRUCTUREQ:
		P++;
 case GET_STRUCTURE:
		StoreH; goto get_structure;

 case GET_NIL:
		StoreH; goto get_nil;

 case GET_LIST:
		StoreH; goto get_list;

 case GET_CONSTANT_NECK_PROCEEDQ:
		P++;
 case GET_CONSTANT_NECK_PROCEED:
		StoreH; goto get_constant_neck_proceed;

 case GET_NIL_NECK_PROCEED:
		StoreH; goto get_nil_neck_proceed;

 case CUTB_X:
		StoreH; goto cutb_x;

 case CUTB_X_NECK:
		StoreH; goto cutb_x_neck;

 case CUTB_NECK:
		StoreH; goto cutb_neck;

 case CUTB_X_NECK_PROCEED:
		StoreH; goto cutb_x_neck_proceed;

 case CUTB_NECK_PROCEED:
		StoreH; goto cutb_neck_proceed;

 case CUTE_X:
		StoreH; goto cute_x;

 case CUTE_X_NECK:
		StoreH; goto cute_x_neck;

 case CUTE_NECK:
		StoreH; goto cute_neck;

 case CUTF:
		StoreH; goto cutf;

 case CUTF_X:
		StoreH; goto cutf_x;

 case CUT_Y:
		StoreH; goto cut_y;

 case CHOICE_X:
		Xb(P1) = ChoiceToInt(w->next_node);
		DISPATCH_W(1);

 case CHOICE_YF:
		ComputeE;
 case CHOICE_Y:
		Yb(P1) = ChoiceToInt(w->next_node);
		DISPATCH_W(1);

 case KONTINUE:
 kontinue: /* after wakeup, write mode! */
		Setfunc(TagToFunctor(Y(0)));
		for(i=0; i<Func->arity; i++) X(i) = Y(i+1);
		DEALLOCATE;
		goto enter_predicate;

 case LEAVE:
		StoreH; goto leave;

 case EXIT_TOPLEVEL:
		StoreH; goto exit_toplevel;

 case RETRY_CQ:
		StoreH; goto retry_cq;

 case RETRY_C:
		StoreH; goto retry_c;

 case GET_STRUCTURE_X0Q:
		P++;
 case GET_STRUCTURE_X0:
		t1 = Tag(STR,H);
		if (TagIsHVAw(t0))
		  BindHVA(t0,t1)
		else if (t0 & TagBitSVA)
		  BindSVA(t0,t1)
		else
		  { BindCVA(t0,t1); Wake; }
		HeapPush(H,T1);
		DISPATCH_W(0+BPTP);

 case GET_LARGE_X0Q:
		P++;
 case GET_LARGE_X0:
                StoreH;
		t1 = MakeLarge(Arg,Pplus1);
                LoadH;
		if (TagIsHVAw(t0))
		  BindHVA(t0,t1)
		else if (t0 & TagBitSVA)
		  BindSVA(t0,t1)
		else
		  { BindCVA(t0,t1); Wake; }
		DISPATCH_W(0+LargeInsns(T1));

 case GET_CONSTANT_X0Q:
		P++;
 case GET_CONSTANT_X0:
		if (TagIsHVAw(t0))
		  BindHVA(t0,T1)
		else if (t0 & TagBitSVA)
		  BindSVA(t0,T1)
		else
		  { BindCVA(t0,T1); Wake; }
		DISPATCH_W(0+BPTP);

 case GET_NIL_X0:
		if (TagIsHVAw(t0))
		  BindHVA(t0,atom_nil)
		else if (t0 & TagBitSVA)
		  BindSVA(t0,atom_nil)
		else
		  { BindCVA(t0,atom_nil); Wake; }
		DISPATCH_W(0);

 case GET_LIST_X0:
		t1 = Tag(LST,H);
		if (TagIsHVAw(t0))
		  BindHVA(t0,t1)
		else if (t0 & TagBitSVA)
		  BindSVA(t0,t1)
		else
		  { BindCVA(t0,t1); Wake; }
		DISPATCH_W(0);

 case GET_XVAR_XVAR:
		Xb(P2) = Xb(P1);
		Xb(P4) = Xb(P3);
		DISPATCH_W(4);

 case GET_X_VARIABLE:
		Xb(P2) = Xb(P1);
		DISPATCH_W(2);

 case GET_Y_FIRST_VARIABLE:
		ComputeE;
 case GET_Y_VARIABLE:
		Yb(P2) = Xb(P1);
		DISPATCH_W(2);

 case GET_YFVAR_YVAR:
		ComputeE;
 case GET_YVAR_YVAR:
		Yb(P2) = Xb(P1);
		Yb(P4) = Xb(P3);
		DISPATCH_W(4);

 case BRANCH:
  		P = Pdeep;
  		goto WriteMode;

 case FUNCTION_1Q:
		StoreH; goto function_1q;

 case FUNCTION_1:
		StoreH; goto function_1;

 case FUNCTION_2Q:
		StoreH; goto function_2q;

 case FUNCTION_2:
		StoreH; goto function_2;

 case BUILTIN_1Q:
		StoreH; goto builtin_1q;

 case BUILTIN_1:
		StoreH; goto builtin_1;

 case BUILTIN_2Q:
		StoreH; goto builtin_2q;

 case BUILTIN_2:
		StoreH; goto builtin_2;

 case BUILTIN_3Q:
		StoreH; goto builtin_3q;

 case BUILTIN_3:
		StoreH; goto builtin_3;
/* Not used now (DCG)
#if defined(FOREIGN_FILES)
 case CI_INARG:
		StoreH; goto ci_inarg;

 case CI_OUTARG:
		StoreH; goto ci_outarg;

 case CI_RETVAL:
		StoreH; goto ci_retval;

 case CI_CALL:
		StoreH; goto ci_call;
#endif
*/
 /* backtracking into clause/2 */
 case RETRY_INSTANCE:
		StoreH; goto retry_instance;

 case GET_CONSTRAINT:
 get_constraint:
                t1 = Xb(P1);
		LoadCVA(t2,H);
		SwitchOnVar(t1,t0,
		      {BindHVA(t1,t2); Xb(P1)=t2;},
		      {BindCVA(t2,t1); Wake;},
		      {BindSVA(t1,t2); Xb(P1)=t2;},
		      {BindCVA(t2,t1); Wake;});
		DISPATCH_W(1);

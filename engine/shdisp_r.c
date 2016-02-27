
/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

case INITTRUE:
LoadH;
goto inittrue;

case FIRSTTRUE_N:
LoadH;
goto firsttrue_n;

case INITCALLQ:
P++;
case INITCALL:
LoadH;
goto initcall;

case FIRSTCALL_NQ:
P++;
case FIRSTCALL_N:
LoadH;
goto firstcall_n;

case FIRSTCALL_8Q:
P++;
case FIRSTCALL_8:
LoadH;
goto firstcall_8;

case FIRSTCALL_7Q:
P++;
case FIRSTCALL_7:
LoadH;
goto firstcall_7;

case FIRSTCALL_6Q:
P++;
case FIRSTCALL_6:
LoadH;
goto firstcall_6;

case FIRSTCALL_5Q:
P++;
case FIRSTCALL_5:
LoadH;
goto firstcall_5;

case FIRSTCALL_4Q:
P++;
case FIRSTCALL_4:
LoadH;
goto firstcall_4;

case FIRSTCALL_3Q:
P++;
case FIRSTCALL_3:
LoadH;
goto firstcall_3;

case FIRSTCALL_2Q:
P++;
case FIRSTCALL_2:
LoadH;
goto firstcall_2;

case FIRSTCALL_1Q:
P++;
case FIRSTCALL_1:
LoadH;
goto firstcall_1;

case FIRSTCALLQ:
P++;
case FIRSTCALL:
LoadH;
goto firstcall;

case CALL_NQ:
P++;
case CALL_N:
LoadH;
goto call_n;

case CALL_8Q:
P++;
case CALL_8:
LoadH;
goto call_8;

case CALL_7Q:
P++;
case CALL_7:
LoadH;
goto call_7;

case CALL_6Q:
P++;
case CALL_6:
LoadH;
goto call_6;

case CALL_5Q:
P++;
case CALL_5:
LoadH;
goto call_5;

case CALL_4Q:
P++;
case CALL_4:
LoadH;
goto call_4;

case CALL_3Q:
P++;
case CALL_3:
LoadH;
goto call_3;

case CALL_2Q:
P++;
case CALL_2:
LoadH;
goto call_2;

case CALL_1Q:
P++;
case CALL_1:
LoadH;
goto call_1;

case CALLQ:
P++;
case CALL:
LoadH;
goto call;

case LASTCALL_NQ:
P++;
case LASTCALL_N:
LoadH;
goto lastcall_n;

case LASTCALL_8Q:
P++;
case LASTCALL_8:
LoadH;
goto lastcall_8;

case LASTCALL_7Q:
P++;
case LASTCALL_7:
LoadH;
goto lastcall_7;

case LASTCALL_6Q:
P++;
case LASTCALL_6:
LoadH;
goto lastcall_6;

case LASTCALL_5Q:
P++;
case LASTCALL_5:
LoadH;
goto lastcall_5;

case LASTCALL_4Q:
P++;
case LASTCALL_4:
LoadH;
goto lastcall_4;

case LASTCALL_3Q:
P++;
case LASTCALL_3:
LoadH;
goto lastcall_3;

case LASTCALL_2Q:
P++;
case LASTCALL_2:
LoadH;
goto lastcall_2;

case LASTCALL_1Q:
P++;
case LASTCALL_1:
LoadH;
goto lastcall_1;

case LASTCALLQ:
P++;
case LASTCALL:
LoadH;
goto lastcall;

case EXECUTEQ:
LoadH;
P = BP2;
goto enter_predicate;

case EXECUTE:
LoadH;
P = BP1;
goto enter_predicate;

case PUT_X_VOID:
LoadH;
goto put_x_void;

case PUT_X_VARIABLE:
LoadH;
goto put_x_variable;

case PUT_XVAL_XVAL:
Xb(P1) = Xb(P2);
Xb(P3) = Xb(P4);
DISPATCH_R(4);

case PUT_X_VALUE:
Xb(P1) = Xb(P2);
DISPATCH_R(2);

case PUT_X_UNSAFE_VALUE:
LoadH;
goto put_x_unsafe_value;

case PUT_Y_FIRST_VARIABLE: 
LoadH;
goto put_y_first_variable;

case PUT_Y_VARIABLE:
LoadH;
goto put_y_variable;

case PUT_YFVAR_YVAR:
LoadH;
goto put_yfvar_yvar;

case PUT_YVAR_YVAR:
LoadH;
goto put_yvar_yvar;

case PUT_YVAL_YVAL:
RefStack(Xb(P1),&Yb(P2));
RefStack(Xb(P3),&Yb(P4));
DISPATCH_R(4);

case PUT_Y_VALUE:
RefStack(Xb(P1),&Yb(P2));
DISPATCH_R(2);

case PUT_Y_UNSAFE_VALUE:
LoadH;
goto put_y_unsafe_value;

case PUT_CONSTANTQ:
Xb(P2) = T3;
DISPATCH_R(2+BPTP);

case PUT_CONSTANT:
Xb(P1) = T2;
DISPATCH_R(1+BPTP);

case PUT_NIL:
Xb(P1) = atom_nil;
DISPATCH_R(1);

case PUT_LARGEQ:
LoadH;
goto put_largeq;

case PUT_LARGE:
LoadH;
goto put_large;

case PUT_STRUCTUREQ:
LoadH;
goto put_structureq;

case PUT_STRUCTURE:
LoadH;
goto put_structure;

case PUT_LIST:
LoadH;
goto put_list;

case PUT_YVAL_YUVAL:
LoadH;
goto put_yval_yuval;

case PUT_YUVAL_YVAL:
LoadH;
goto put_yuval_yval;

case PUT_YUVAL_YUVAL:
LoadH;
goto put_yuval_yuval;

case GET_X_VALUE:
get_x_value:
EUNIFY(Xb(P2),Xb(P1),2);

case GET_Y_FIRST_VALUE:
get_y_first_value:
GetFirstValue(Yb(P2),Xb(P1));
DISPATCH_R(2);

case GET_Y_VALUE:
get_y_value:
RefStack(t1,&Yb(P2));
EUNIFY(Xb(P1),t1,2);

case GET_CONSTANTQ:
P++;
case GET_CONSTANT:
get_constant:		
Unify_atom(T2,Xb(P1));
DISPATCH_R(1+BPTP);

case GET_LARGEQ:
P++;
case GET_LARGE:
get_large:
Unify_large(Arg,Pplus2, Xb(P1));
DISPATCH_R(1+LargeInsns(T2));

case GET_STRUCTUREQ:
P++;
case GET_STRUCTURE:
get_structure:
Unify_structure(T2,Xb(P1),
                { DISPATCH_R(1+BPTP); },
                { DISPATCH_W(1+BPTP); })
     
     case GET_NIL:
     get_nil:
     Unify_atom(atom_nil,Xb(P1));
DISPATCH_R(1);

case GET_LIST:
get_list:
Unify_list(Xb(P1),
           { DISPATCH_R(1); },
           { DISPATCH_W(1); })
     
case GET_CONSTANT_NECK_PROCEEDQ:
P++;
case GET_CONSTANT_NECK_PROCEED:
get_constant_neck_proceed:
Unify_atom(T2,Xb(P1));
LoadH; goto neck_proceed_w;


case GET_NIL_NECK_PROCEED:
get_nil_neck_proceed:
Unify_atom(atom_nil,Xb(P1));
LoadH; goto neck_proceed_w;

case CUTB_X:
cutb_x:

w->local_top = 0; /* may get hole at top of local stack */
w->next_node = ChoiceFromInt(Xb(P1));
DOCUT;
DISPATCH_R(1);

case CUTB_X_NECK:
cutb_x_neck:
w->local_top = 0; /* may get hole at top of local stack */
w->next_node = ChoiceFromInt(Xb(Pnext));

case CUTB_NECK:
cutb_neck:
DOCUT;
if (w->next_alt){
  w->next_alt = NULL;
  if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
}
DISPATCH_R(0);

case CUTB_X_NECK_PROCEED:
cutb_x_neck_proceed:
w->next_node = ChoiceFromInt(Xb(P1)); /* P++ */
/* w->local_top = 0; done by PROCED */

case CUTB_NECK_PROCEED:
cutb_neck_proceed:
DOCUT;
if (w->next_alt) {
  w->next_alt = NULL;
  if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
}
goto proceed_r;

case CUTE_X:
cute_x:

w->next_node = ChoiceFromInt(Xb(P1));
w->local_top = E; /* w->local_top may be 0 here. */
DOCUT;
SetE(w->local_top);
DISPATCH_R(1);

case CUTE_X_NECK:
cute_x_neck:
w->next_node = ChoiceFromInt(Xb(Pnext));

case CUTE_NECK:
cute_neck:
w->local_top = E; /* w->local_top may be 0 here. */
DOCUT;
				/* w->next_alt can't be NULL here */
w->next_alt = NULL;
if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))
     choice_overflow(Arg,CHOICEPAD);
SetE(w->local_top);
DISPATCH_R(0);

case CUTF_X:
cutf_x:
w->next_node = ChoiceFromInt(Xb(Pnext));

case CUTF:
cutf:
DOCUT;
SetE(w->frame);
DISPATCH_R(0);

case CUT_Y:
cut_y:

RefStack(t1,&Yb(P1));
w->next_node = ChoiceFromInt(t1);
DOCUT;
SetE(w->frame);
DISPATCH_R(1);

case CHOICE_X:
Xb(P1) = ChoiceToInt(w->next_node);
DISPATCH_R(1);

case CHOICE_YF:
ComputeE;
case CHOICE_Y:
Yb(P1) = ChoiceToInt(w->next_node);
DISPATCH_R(1);

case KONTINUE:
LoadH;
goto kontinue;

case LEAVE:
leave:

case EXIT_TOPLEVEL:
exit_toplevel:
w->insn = P;
#if defined(UNDEFINED)          /* What should we save here? */ 
w->node = B;                                       /* MCL */
w->frame = E->frame;                               /* MCL */
#endif
if (worker && (worker->action & KEEP_STACKS)) {     /* We may backtrack */
  SAVE_WAM_STATE;
}

/* We may have been signaled and jumped here from enter_predicate: */

if (Stop_This_Goal(Arg)) 
     wam_exit_code = WAM_INTERRUPTED;

#if defined(DEBUG)
     /* printf("Goal %x returning!\n", worker); */
#endif

return wam_exit_code;

case RETRY_CQ:
retry_cq:
if (w->next_alt)
     B->next_alt = w->next_alt, w->next_alt = NULL;
if (!C2(Arg))
     goto fail;
goto proceed_r;

case RETRY_C:
retry_c:
if (w->next_alt)
     B->next_alt = w->next_alt, w->next_alt = NULL;
if (!C1(Arg))
     goto fail;
goto proceed_r;
     
case GET_STRUCTURE_X0Q:
S = TagToArg(t0,1);
DISPATCH_R(1+BPTP);

case GET_STRUCTURE_X0:
S = TagToArg(t0,1);
DISPATCH_R(0+BPTP);

case GET_LARGE_X0Q:
Unify_large(Arg,Pplus2, t0);
DISPATCH_R(1+LargeInsns(T2));

case GET_LARGE_X0:
Unify_large(Arg,Pplus1, t0);
DISPATCH_R(0+LargeInsns(T1));

case GET_CONSTANT_X0Q:
DISPATCH_R(1+BPTP);

case GET_CONSTANT_X0:
DISPATCH_R(0+BPTP);

case GET_NIL_X0:
DISPATCH_R(0);

case GET_LIST_X0:
S = TagToLST(t0);
DISPATCH_R(0);

case GET_XVAR_XVAR:
Xb(P2) = Xb(P1);
Xb(P4) = Xb(P3);
DISPATCH_R(4);

case GET_X_VARIABLE:
Xb(P2) = Xb(P1);
DISPATCH_R(2);

case GET_Y_FIRST_VARIABLE:
ComputeE;
case GET_Y_VARIABLE:
Yb(P2) = Xb(P1);
DISPATCH_R(2);

case GET_YFVAR_YVAR:
ComputeE;
case GET_YVAR_YVAR:
Yb(P2) = Xb(P1);
Yb(P4) = Xb(P3);
DISPATCH_R(4);

case BRANCH:
P = Pdeep;
goto ReadMode;

case FUNCTION_1Q:
function_1q:
Numstack_End = NULL;
if (ERRORTAG==(Xb(P2) = (TAGGED)C4(Arg,Xb(P3),Pplus6)))
     goto fail;
DISPATCH_R(4+2*BPTP);

case FUNCTION_1:
function_1:
Numstack_End = NULL;
if (ERRORTAG==(Xb(P1) = (TAGGED)C3(Arg,Xb(P2),Pplus5)))
     goto fail;
DISPATCH_R(3+2*BPTP);
     
     
case FUNCTION_2Q:
function_2q:
Numstack_End = NULL;
if (ERRORTAG==(Xb(P2) = (TAGGED)C5(Arg,Xb(P3),Xb(P4),Pplus7)))
     goto fail;
DISPATCH_R(5+2*BPTP);
     
case FUNCTION_2:
function_2:
Numstack_End = NULL;
if (ERRORTAG==(Xb(P1) = (TAGGED)C4(Arg,Xb(P2),Xb(P3),Pplus6)))
     goto fail;
DISPATCH_R(4+2*BPTP);

case BUILTIN_1Q:
builtin_1q:
if (!C3(Arg,Xb(P2))) goto fail;
DISPATCH_R(2+BPTP);

case BUILTIN_1:
builtin_1:
if (!C2(Arg,Xb(P1))) goto fail;
DISPATCH_R(1+BPTP);

case BUILTIN_2Q:
builtin_2q:
if (!C4(Arg,Xb(P2),Xb(P3))) goto fail;
DISPATCH_R(3+BPTP);

case BUILTIN_2:
builtin_2:
if (!C3(Arg,Xb(P1),Xb(P2))) goto fail;
DISPATCH_R(2+BPTP);

case BUILTIN_3Q:
builtin_3q:
if (!C5(Arg,Xb(P2),Xb(P3),Xb(P4))) goto fail;
DISPATCH_R(4+BPTP);

case BUILTIN_3:
builtin_3:
if (!C4(Arg,Xb(P1),Xb(P2),Xb(P3))) goto fail;
DISPATCH_R(3+BPTP);


/* We do not need these if we do not have foreign C files */
/* Not used now (DCG)
#if defined(FOREIGN_FILES)                     
case CI_INARG:
ci_inarg:
if (!foreign_ci_inarg(Arg,P1,P2))     goto fail;
DISPATCH_R(2);

case CI_OUTARG:
ci_outarg:
if (!foreign_ci_outarg(Arg,P1,P2))    goto fail;
DISPATCH_R(2);

case CI_RETVAL:
ci_retval:
if (!foreign_ci_retval(Arg,P1,P2))    goto fail;
DISPATCH_R(2);

case CI_CALL:
ci_call:
t1 = (TAGGED)(w->global_top+P1);
t1 += t1&0x4;
(*(void (*)())(ci_table[P2]))(t1,t1);
DISPATCH_R(2);
#endif
*/
case RETRY_INSTANCE:
retry_instance:          /* Take into account "open" predicates.  (MCL) */
/* If there is *definitely* no next instance, remove choicepoint */
if (
    (TagToRoot(X(RootArg))->behavior_on_failure != DYNAMIC &&
     !next_instance_conc(Arg, &ins)) /* Wait and removes handle if needed */
    ||
    (TagToRoot(X(RootArg))->behavior_on_failure == DYNAMIC &&
     !next_instance(Arg, &ins))
    ) {
  w->next_alt = NULL;
  w->node = SetB(w->next_node);
  SetShadowregs(B);
}

if (!ins) { /*  A conc. predicate has been closed, or a
                non-blocking call was made (MCL) */
#if defined(DEBUG)                                      /* Extended check */
  if (debug_concchoicepoints) {
    if ((TagToRoot(X(RootArg))->behavior_on_failure != CONC_CLOSED) &&
        (IS_BLOCKING(X(InvocationAttr))))
      fprintf(stderr,
"**wam(): failing on a concurrent closed pred, chpt=%x, failing chpt=%x .\n",
              (int)w->node,(int)TopConcChpt);
  }

  if (debug_conc) {
    if (TagToRoot(X(RootArg))->x2_pending_on_instance ||
        TagToRoot(X(RootArg))->x5_pending_on_instance)
      fprintf(stderr, 
      "**wam(): failing with invokations pending from root, type = %d.\n",
              (TagToRoot(X(RootArg))->behavior_on_failure));
  }
#endif
  TopConcChpt = (struct node *)TermToPointerOrNull(X(PrevDynChpt));
#if defined(DEBUG)
  if (debug_concchoicepoints)
    fprintf(stderr, "New topmost concurrent chpt = %x\n", (int)TopConcChpt);
#endif
  goto fail;                                           /* But fail anyway */
}

#if defined(DEBUG)
if(debug_conc && TagToRoot(X(RootArg))->behavior_on_failure != DYNAMIC) 
    fprintf(stderr, 
            "*** %d(%d)  backtracking on a concurrent predicate.\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
if(debug_concchoicepoints && 
   TagToRoot(X(RootArg))->behavior_on_failure != DYNAMIC) 
    fprintf(stderr, 
            "backtracking to chpt. = %x\n", (int)w->node);
#endif    

P = ins->emulcode;
goto ReadMode;

case GET_CONSTRAINT:
LoadH;
goto get_constraint;






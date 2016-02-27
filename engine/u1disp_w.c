/* Copyright (C) 1996,1997,1998, UPM-CLIP */

	case UNIFY_VOID:
		for (i=SP1, P++; i>4; --i) ConstrHVA(H);
	case UNIFY_VOID_4:
		ConstrHVA(H);
	case UNIFY_VOID_3:
		ConstrHVA(H);
	case UNIFY_VOID_2:
		ConstrHVA(H);
	case UNIFY_VOID_1:
		ConstrHVA(H);
		DISPATCH_W(0);

	case UNIFY_X_VARIABLE:
		U1_XVAR_W(P1);
		DISPATCH_W(1);

	case UNIFY_X_VALUE:
		U1_XVAL_W(P1);
		DISPATCH_W(1);

	case UNIFY_X_LOCAL_VALUE:
		U1_XLVAL_W(P1);
		DISPATCH_W(1);

	case UNIFY_Y_FIRST_VARIABLE:
		ComputeE;
	case UNIFY_Y_VARIABLE:
		U1_YVAR_W(P1);
		DISPATCH_W(1);
	
	case UNIFY_Y_FIRST_VALUE:
		U1_YFVAL_W(P1);
		DISPATCH_W(1);
	
	case UNIFY_Y_VALUE:
		U1_YVAL_W(P1);
		DISPATCH_W(1);

	case UNIFY_Y_LOCAL_VALUE:
		U1_YLVAL_W(P1);
		DISPATCH_W(1);

	case UNIFY_CONSTANTQ:
		HeapPush(H,T2);
		DISPATCH_W(1+BPTP);

	case UNIFY_CONSTANT:
		HeapPush(H,T1);
		DISPATCH_W(0+BPTP);

	case UNIFY_LARGEQ:
                P++;
       case UNIFY_LARGE:
		w->global_top = HeapOffset(H,1);
		*H = MakeLarge(Arg,Pplus1);
		DISPATCH_R(0+LargeInsns(T1));

	case UNIFY_STRUCTUREQ:
		HeapPush(H,Tag(STR,HeapOffset(H,1)));
		HeapPush(H,T2);
		DISPATCH_W(1+BPTP);

	case UNIFY_STRUCTURE:
		HeapPush(H,Tag(STR,HeapOffset(H,1)));
		HeapPush(H,T1);
		DISPATCH_W(0+BPTP);

	case UNIFY_NIL:
		HeapPush(H,atom_nil);
		DISPATCH_W(0);

	case UNIFY_LIST:
		HeapPush(H,Tag(LST,HeapOffset(H,1)));
		DISPATCH_W(0);

	case UNIFY_CONSTANT_NECK_PROCEEDQ:
		HeapPush(H,T2);
		goto neck_proceed_w;

	case UNIFY_CONSTANT_NECK_PROCEED:
		HeapPush(H,T1);
		goto neck_proceed_w;

	case UNIFY_NIL_NECK_PROCEED:
		HeapPush(H,atom_nil);
		goto neck_proceed_w;


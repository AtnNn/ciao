/* Copyright (C) 1996,1997,1998, UPM-CLIP */

	case UNIFY_VOID:
		U1_VOID_R(P1);
		DISPATCH_R(1);

	case UNIFY_VOID_1:
		U1_VOID_R(1);
		DISPATCH_R(0);

	case UNIFY_VOID_2:
		U1_VOID_R(2);
		DISPATCH_R(0);

	case UNIFY_VOID_3:
		U1_VOID_R(3);
		DISPATCH_R(0);

	case UNIFY_VOID_4:
		U1_VOID_R(4);
		DISPATCH_R(0);

	case UNIFY_X_VARIABLE:
		RefHeapNext(Xb(P1),S);
		DISPATCH_R(1);

	case UNIFY_X_VALUE:
	case UNIFY_X_LOCAL_VALUE:
		U1_XVAL_R2(P1);

	case UNIFY_Y_FIRST_VARIABLE:
		ComputeE;
	case UNIFY_Y_VARIABLE:
		RefHeapNext(Yb(P1),S);
		DISPATCH_R(1);

	case UNIFY_Y_FIRST_VALUE:
		U1_YFVAL_R(P1);
		DISPATCH_R(1);

	case UNIFY_Y_VALUE:
	case UNIFY_Y_LOCAL_VALUE:
		U1_YVAL_R2(P1);

	case UNIFY_CONSTANTQ:
		P++;
	case UNIFY_CONSTANT:
		RefHeapNext(t1,S);
		Unify_heap_atom(T1,t1);
                DISPATCH_R(0+BPTP);

	case UNIFY_LARGEQ:
		P++;
	case UNIFY_LARGE:
		RefHeapNext(t1,S);
		Unify_heap_large(Arg,Pplus1, t1);
                DISPATCH_R(0+LargeInsns(T1));

	case UNIFY_STRUCTUREQ:
		P++;
	case UNIFY_STRUCTURE:
		RefHeapNext(t1,S);
		Unify_heap_structure(T1,t1,
				{ DISPATCH_R(0+BPTP); },
				{ DISPATCH_W(0+BPTP); })

	case UNIFY_NIL:
		RefHeapNext(t1,S);
		Unify_heap_atom(atom_nil,t1);
                DISPATCH_R(0);

	case UNIFY_LIST:
		RefHeapNext(t1,S);
		Unify_heap_list(t1,
				{ DISPATCH_R(0); },
				{ DISPATCH_W(0); })

	case UNIFY_CONSTANT_NECK_PROCEEDQ:
		P++;
	case UNIFY_CONSTANT_NECK_PROCEED:
		RefHeapNext(t1,S);
		Unify_heap_atom(T1,t1);
                LoadH; goto neck_proceed_w;

	case UNIFY_NIL_NECK_PROCEED:
		RefHeapNext(t1,S);
		Unify_heap_atom(atom_nil,t1);
                LoadH; goto neck_proceed_w;


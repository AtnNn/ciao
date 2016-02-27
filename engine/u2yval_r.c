/* Copyright (C) 1996,1997,1998, UPM-CLIP */

				/* YVAL + ... */

	case U2_YVAL_VOID:
	case U2_YLVAL_VOID:
                RefHeapNext(t1,S);
		U1_VOID_R(P2);
                EUNIFY(Yb(P1),t1,2);

	case U2_YVAL_XVAR:
	case U2_YLVAL_XVAR:
                RefHeapNext(t1,S);
		U1_XVAR_R(P2);
                EUNIFY(Yb(P1),t1,2);

	case U2_YVAL_YVAR:
	case U2_YLVAL_YVAR:
		U1_YVAL_R(P1);
		U1_YVAR_R(P2);
		DISPATCH_R(2);

	case U2_YVAL_YFVAL:
	case U2_YLVAL_YFVAL:
		U1_YVAL_R(P1);
		U1_YFVAL_R(P2);
		DISPATCH_R(2);

	case U2_YVAL_XVAL:
	case U2_YVAL_XLVAL:
	case U2_YLVAL_XVAL:
	case U2_YLVAL_XLVAL:
		U1_YVAL_R(P1);
		U1_XVAL_R3(P2);

	case U2_YVAL_YVAL:
	case U2_YVAL_YLVAL:
	case U2_YLVAL_YVAL:
	case U2_YLVAL_YLVAL:
		U1_YVAL_R(P1);
		U1_YVAL_R3(P2);


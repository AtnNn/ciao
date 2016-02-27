/* Copyright (C) 1996,1997,1998, UPM-CLIP */

				/* XVAL + ... */

	case U2_XVAL_VOID:
	case U2_XLVAL_VOID:
                RefHeapNext(t1,S);
		U1_VOID_R(P2);
                EUNIFY(Xb(P1),t1,2);

	case U2_XVAL_XVAR:
	case U2_XLVAL_XVAR:
                t0 = Xb(P1);
                RefHeapNext(t1,S);
		U1_XVAR_R(P2);
                EUNIFY(t0,t1,2);

	case U2_XVAL_YFVAR:
	case U2_XLVAL_YFVAR:
		ComputeE;
	case U2_XVAL_YVAR:
	case U2_XLVAL_YVAR:
		U1_XVAL_R(P1);
		U1_YVAR_R(P2);
		DISPATCH_R(2);

	case U2_XVAL_XVAL:
	case U2_XVAL_XLVAL:
	case U2_XLVAL_XVAL:
	case U2_XLVAL_XLVAL:
		U1_XVAL_R(P1);
		U1_XVAL_R3(P2);

	case U2_XVAL_YFVAL:
	case U2_XLVAL_YFVAL:
		U1_XVAL_R(P1);
		U1_YFVAL_R(P2);
		DISPATCH_R(2);

	case U2_XVAL_YVAL:
	case U2_XVAL_YLVAL:
	case U2_XLVAL_YVAL:
	case U2_XLVAL_YLVAL:
		U1_XVAL_R(P1);
		U1_YVAL_R3(P2);


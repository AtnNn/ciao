/* Copyright (C) 1996,1997,1998, UPM-CLIP */

				/* VOID + ... */

	case U2_VOID_XVAR:
		U1_VOID_R(P1);
		U1_XVAR_R(P2);
		DISPATCH_R(2);

	case U2_VOID_YFVAR:
		ComputeE;
	case U2_VOID_YVAR:
		U1_VOID_R(P1);
		U1_YVAR_R(P2);
		DISPATCH_R(2);

	case U2_VOID_XVAL:
	case U2_VOID_XLVAL:
		U1_VOID_R(P1);
		U1_XVAL_R3(P2);

	case U2_VOID_YFVAL:
		U1_VOID_R(P1);
		U1_YFVAL_R(P2);
		DISPATCH_R(2);

	case U2_VOID_YVAL:
	case U2_VOID_YLVAL:
		U1_VOID_R(P1);
		U1_YVAL_R3(P2);

/* Copyright (C) 1996,1997,1998, UPM-CLIP */

				/* YVAR + ... */

	case U2_YFVAR_VOID:
		ComputeE;
	case U2_YVAR_VOID:
		U1_YVAR_R(P1);
		U1_VOID_R(P2);
		DISPATCH_R(2);

	case U2_YFVAR_XVAR:
		ComputeE;
	case U2_YVAR_XVAR:
		U1_YVAR_R(P1);
		U1_XVAR_R(P2);
		DISPATCH_R(2);

	case U2_YFVAR_YVAR:
		ComputeE;
	case U2_YVAR_YVAR:
		U1_YVAR_R(P1);
		U1_YVAR_R(P2);
		DISPATCH_R(2);

	case U2_YFVAR_XVAL:
	case U2_YFVAR_XLVAL:
		ComputeE;
	case U2_YVAR_XVAL:
	case U2_YVAR_XLVAL:
		U1_YVAR_R(P1);
		U1_XVAL_R3(P2);

	case U2_YFVAR_YVAL:
	case U2_YFVAR_YLVAL:
		ComputeE;
	case U2_YVAR_YVAL:
	case U2_YVAR_YLVAL:
		U1_YVAR_R(P1);
		U1_YVAL_R3(P2);


				/* YFVAL + ... */

	case U2_YFVAL_VOID:
		U1_YFVAL_R(P1);
		U1_VOID_R(P2);
		DISPATCH_R(2);

	case U2_YFVAL_XVAR:
		U1_YFVAL_R(P1);
		U1_XVAR_R(P2);
		DISPATCH_R(2);

	case U2_YFVAL_YFVAL:
		U1_YFVAL_R(P1);
		U1_YFVAL_R(P2);
		DISPATCH_R(2);

	case U2_YFVAL_XVAL:
	case U2_YFVAL_XLVAL:
		U1_YFVAL_R(P1);
		U1_XVAL_R3(P2);

	case U2_YFVAL_YVAL:
	case U2_YFVAL_YLVAL:
		U1_YFVAL_R(P1);
		U1_YVAL_R3(P2);


/* Copyright (C) 1996,1997,1998, UPM-CLIP */

				/* Defs for pairs of UNIFYs */

#define U1_VOID_R(I)	{ S = HeapOffset(S,I); }

#define U1_VOID_W(I)	{ i=(I); do ConstrHVA(H) while (--i); }

#define U1_XVAR_R(I)	{ RefHeapNext(Xb(I),S); }

#define U1_XVAR_W(I)	{ LoadHVA(Xb(I),H); }

#define U1_YVAR_R(I)	{ RefHeapNext(Yb(I),S); }

#define U1_YVAR_W(I)	{ LoadHVA(Yb(I),H); }

#define U1_XVAL_R(I)	{ RefHeapNext(t1,S); CUNIFY(Xb(I),t1) }
#define U1_XVAL_R2(I)	{ RefHeapNext(t1,S); EUNIFY(Xb(I),t1,1) }
#define U1_XVAL_R3(I)	{ RefHeapNext(t1,S); EUNIFY(Xb(I),t1,2) }

#define U1_XVAL_W(I)	{ HeapPush(H,Xb(I)); }

#define U1_YVAL_R(I)	{ RefHeapNext(t1,S); RefStack(t0,&Yb(I)); CUNIFY(t0,t1) }
#define U1_YVAL_R2(I)	{ RefHeapNext(t1,S); RefStack(t0,&Yb(I)); EUNIFY(t0,t1,1) }
#define U1_YVAL_R3(I)	{ RefHeapNext(t1,S); RefStack(t0,&Yb(I)); EUNIFY(t0,t1,2) }

#define U1_YVAL_W(I)	{ HeapPushRefStack(H,&Yb(I)); }

#define U1_YFVAL_R(I)	{ RefHeapNext(t0,S); GetFirstValue(Yb(I),t0); }

#define U1_YFVAL_W(I)	{ LoadHVA(t0,H); GetFirstValue(Yb(I),t0); }

#define	U1_XLVAL_W(I) { t1=Xb(I); Unify_local_value }

#define	U1_XLVALC_W(I) { t1=X(I); Unify_local_value }

#define	U1_YLVAL_W(I) { RefStack(t1,&Yb(I)); Unify_local_value }

#define	U1_YLVALC_W(I) { RefStack(t1,&Y(I)); Unify_local_value }



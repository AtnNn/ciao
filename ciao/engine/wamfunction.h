/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

extern tagged_t fu1_minus PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_plus PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_integer PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_float PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_not PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_add1 PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_sub1 PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu2_plus PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_minus PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_times PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_fdivide PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_idivide PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_rem PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_mod PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_xor PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_and PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_or PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_lsh PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_rsh PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_compare PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu2_gcd PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu1_abs PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_sign PROTO((Argdecl, tagged_t x0, bcp_t op));

extern tagged_t fu1_intpart PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_fractpart PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_floor PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_round PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_ceil PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu2_pow PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));
extern tagged_t fu1_exp PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_log PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_sqrt PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_sin PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_cos PROTO((Argdecl, tagged_t x0, bcp_t op));
extern tagged_t fu1_atan PROTO((Argdecl, tagged_t x0, bcp_t op));

extern tagged_t fu2_arg PROTO((Argdecl, tagged_t x0, tagged_t x1, bcp_t op));

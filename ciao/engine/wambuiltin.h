/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

extern bool_t bu1_atom PROTO((Argdecl,tagged_t x0));
extern bool_t bu1_atomic PROTO((Argdecl,tagged_t x0));
extern bool_t bu1_float PROTO((Argdecl,tagged_t x0));
extern bool_t bu1_integer PROTO((Argdecl,tagged_t x0));
extern bool_t bu1_number PROTO((Argdecl,tagged_t x0));
extern bool_t bu1_var PROTO((Argdecl,tagged_t x0));
extern bool_t bu1_nonvar PROTO((Argdecl,tagged_t x0));
extern bool_t bu2_lexeq PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_lexne PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_lexlt PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_lexle PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_lexgt PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_lexge PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_numeq PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_numne PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_numlt PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_numle PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_numgt PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu2_numge PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu1_if PROTO((Argdecl,tagged_t x0));
extern bool_t bu2_univ PROTO((Argdecl,tagged_t x0, tagged_t x1));
extern bool_t bu3_functor PROTO((Argdecl,tagged_t x0, tagged_t x1, tagged_t x2));

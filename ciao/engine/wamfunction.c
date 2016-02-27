/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "wamfunction.h"

/* declarations for global functions accessed here */

#include "support_defs.h"
#include "bignum_defs.h"
#include "wamfunction_defs.h"

/* local declarations */

static tagged_t lsh_internal(Argdecl, tagged_t t, int dist, bcp_t p);
static tagged_t rsh_internal(Argdecl, tagged_t t, int dist, bcp_t p);
/*extern tagged_t bn_call
  PROTO((Argdecl, int (*f)(), tagged_t x, tagged_t y, bcp_t op));*/
static tagged_t rsh_internal(Argdecl ,tagged_t t, int dist, bcp_t op);
static tagged_t lsh_internal(Argdecl, tagged_t t, int dist, bcp_t op);

/*
extern tagged_t bn_call
              PROTO((Argdecl, int (*f)(), tagged_t x, tagged_t y, bcp_t op));

static tagged_t rsh_internal PROTO((Argdecl, tagged_t t, int dist, bcp_t op));
static tagged_t lsh_internal PROTO((Argdecl, tagged_t t, int dist, bcp_t op));
*/

#if !defined(USE_OC_EXCEPTIONS)
static char *illexp = "illegal arithmetic expression";         /* Shared  */
#endif

tagged_t fu1_minus(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$-", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  /*extern bn_minus();*/
  
  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t))
    {
      if (t==TaggedLow)
	return make_integer_check(Arg, GetSmall(TaggedHigh),p);
      else
	return TaggedZero-(t-TaggedZero);
    }
  else if (IsFloat(t))
    return make_float_check(Arg, -GetFloat(t),p);
  else
    return bn_call(Arg,bn_minus,t,0,p);
}

tagged_t fu1_plus(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$+", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t) || p==NULL)
    return t;
  else if (IsFloat(t))
    return make_float_check(Arg, GetFloat(t),p);
  else
    return bn_call(Arg,bn_plus,t,0,p);
}

tagged_t fu1_integer(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$integer", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  /*extern bn_from_float(), bn_plus();*/
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t))
    {
      if (float_is_finite(t)) return bn_call(Arg,bn_from_float,t,0,p);
      else BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    } 
  else
    return bn_call(Arg,bn_plus,t,0,p);
}

tagged_t fu1_float(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$float", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (p==NULL && IsFloat(t))
    return t;
  else
    return make_float_check(Arg, GetFloat(t),p);
}

tagged_t fu1_add1(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$++", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  /*extern bn_incr();*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t))
    {
      if (t==TaggedHigh-(1<<SmallShift))
	return make_integer_check(Arg, GetSmall(TaggedHigh),p);
      else
	return t+(1<<SmallShift);
    }
  else if (IsFloat(t))
    return make_float_check(Arg, GetFloat(t) + 1.0,p);
  else
    return bn_call(Arg,bn_incr,t,0,p);
}

tagged_t fu1_sub1(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$--", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  /*extern bn_decr();*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t))
    {
      if (t==TaggedLow)
	return make_integer_check(Arg, GetSmall(TaggedLow)-1,p);
      else
	return t-(1<<SmallShift);
    }
  else if (IsFloat(t))
    return make_float_check(Arg, GetFloat(t) - 1.0,p);
  else
    return bn_call(Arg,bn_decr,t,0,p);
}


				/* binary functions */

tagged_t fu2_plus(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$+", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_add();*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  u=X1; 
  NDEREF_F(Arg, u, 1, t1);
  if (TagIsSmall(t) && TagIsSmall(u))
    {
      if (TagIsSmall(t1 = t+(u-TaggedZero)))
	return t1;
      else
	return make_integer_check(Arg, GetSmall(t1),p);
    }
  else if (IsFloat(t) || IsFloat(u))
    return make_float_check(Arg, GetFloat(t) + GetFloat(u),p);
  else
    return bn_call(Arg,bn_add,t,u,p);
}

tagged_t fu2_minus(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$-", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_subtract();*/
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  u=X1; 
  NDEREF_F(Arg, u, 1, t1);
  if (TagIsSmall(t) && TagIsSmall(u))
    {
      if (TagIsSmall(t1 = t-(u-TaggedZero)))
	return t1;
      else
	return make_integer_check(Arg, GetSmall(t1),p);
    }
  else if (IsFloat(t) || IsFloat(u))
    return make_float_check(Arg, GetFloat(t) - GetFloat(u),p);
  else
    return bn_call(Arg,bn_subtract,t,u,p);
}

tagged_t fu2_times(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$*", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_multiply();*/
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  u=X1; 
  NDEREF_F(Arg, u, 1, t1);
  if (TagIsSmall(t) && TagIsSmall(u))
    {
      int st = GetSmall(t);
      int su = (int)(u-TaggedZero);
      int stu = st*su;
      tagged_t tu = ((tagged_t)stu)+TaggedZero;
      
      if (su==0 || (stu/su==st && TagIsSmall(tu)))
	return tu;
    }
  if (IsFloat(t) || IsFloat(u))
    return make_float_check(Arg, GetFloat(t) * GetFloat(u),p);
  else
    return bn_call(Arg,bn_multiply,t,u,p);
}

tagged_t fu2_fdivide(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$/", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  u=X1; 
  NDEREF_F(Arg, u, 1, t1);
  return make_float_check(Arg, GetFloat(t)/GetFloat(u),p);
}

tagged_t fu2_idivide(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$//", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_quotient_remainder_quot_wanted();*/
  /*extern bool_t bn_quotient_wanted;*/
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  
  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);
  
  if (TagIsSmall(t) && TagIsSmall(u))
    return make_integer_check(Arg, (int)(t-TaggedZero)/(int)(u-TaggedZero),p);

  /*bn_quotient_wanted = TRUE;*/
  return bn_call(Arg,bn_quotient_remainder_quot_wanted,t,u,p);
}

tagged_t fu2_rem(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$rem", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_quotient_remainder_quot_not_wanted();*/
  /*extern bool_t bn_quotient_wanted;*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);


  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TagIsSmall(t) && TagIsSmall(u))
    return (int)(t-TaggedZero)%(int)(u-TaggedZero)+TaggedZero;

  /*bn_quotient_wanted = FALSE;*/
  return bn_call(Arg,bn_quotient_remainder_quot_not_wanted,t,u,p);
}

tagged_t fu2_mod(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$mod", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  int rem, denom;
  tagged_t T_rem;
  /*extern bn_quotient_remainder_quot_not_wanted();*/
  /*extern bn_add();*/
  /*extern bool_t bn_quotient_wanted;*/
  /*extern bool_t bn_positive PROTO((Bignum *x));*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TagIsSmall(t) && TagIsSmall(u)) {
    denom = (int)(u-TaggedZero);
    rem = (int)(t-TaggedZero)%denom;
    return ( (denom > 0 && rem < 0) || (denom < 0 && rem > 0) ?
              rem+denom : rem ) + TaggedZero;
  } else {
    /*bn_quotient_wanted = FALSE;*/
    T_rem = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,t,u,p);
    return ( T_rem != TaggedZero &&
             fu1_sign(Arg,u,p) != fu1_sign(Arg,T_rem,p)
             ? bn_call(Arg,bn_add,T_rem,u,p) : T_rem );
  }
}

tagged_t fu1_abs(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$abs", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  ENG_FLT f;
  /*extern bn_minus();*/
  /*extern bool_t bn_positive PROTO((Bignum *x));*/
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  //  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t)) {
      if (t==TaggedLow)
	return make_integer_check(Arg, GetSmall(TaggedHigh),p);
      else if (t < TaggedZero)
	return TaggedZero-(t-TaggedZero);
      else
        return t;
  } else if (IsFloat(t))
    return (((f = GetFloat(t)) < 0.0) ? make_float_check(Arg, -f, p) : t);
  else 
    return ((!bn_positive(TagToSTR(t))) ? bn_call(Arg,bn_minus,t,0,p) : t);
}

tagged_t fu1_sign(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$sign", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  ENG_FLT f;
  /*extern bool_t bn_positive PROTO((Bignum *x));*/
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t))
    return ((t==TaggedZero) ? TaggedZero :
            (t < TaggedZero) ? TaggedZero-(1<<SmallShift) :
            TaggedZero+(1<<SmallShift));
  else if (IsFloat(t)) {
    f = GetFloat(t);
    return ((f == 0.0) ? t :
            (f < 0.0) ? make_float_check(Arg, -1.0, p) :
            make_float_check(Arg, 1.0, p));
  } else 
    return ((!bn_positive(TagToSTR(t))) ? TaggedZero-(1<<SmallShift) :
            TaggedZero+(1<<SmallShift));
}

tagged_t fu1_not(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$\\", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  /*extern bn_not();*/
  
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t))
    return t^(QMask-(1<<SmallShift));
  else
    return bn_call(Arg,bn_not,t,0,p);
}

tagged_t fu2_xor(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$#", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_xor();*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);


  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  if (TagIsSmall(t) && TagIsSmall(u))
    return t^u^TaggedZero;
  else
    return bn_call(Arg,bn_xor,t,u,p);
}

tagged_t fu2_and(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$/\\", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_and();*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  if (TagIsSmall(t) && TagIsSmall(u))
    return ((t^ZMask)&(u^ZMask))^ZMask;
  else
    return bn_call(Arg,bn_and,t,u,p);
}

tagged_t fu2_or(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$\\/", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  /*extern bn_or();*/

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  if (TagIsSmall(t) && TagIsSmall(u))
    return ((t^ZMask)|(u^ZMask))^ZMask;
  else
    return bn_call(Arg,bn_or,t,u,p);
}

static tagged_t lsh_internal(Arg,t,dist,p)
     Argdecl;
     tagged_t t;
     int dist;
     bcp_t p;
{
  tagged_t u;
  /*extern bn_lshift();*/

  
  if (TagIsSmall(t))
    {
      switch (dist)
	{
	case 0:
	  return t;
	case 1:
	  u = (t<<1) + 0x78000000;
	  break;
	case 2:
	  u = (t<<2) + 0x68000000;
	  break;
	case 3:
	  u = (t<<3) + 0x48000000;
	  break;
	case 4:
	  u = (t<<4) + 0x08000000;
	  break;
	default:
	  u = 0;
	}
      if (TagIsSmall(u))
	return u;
      /*
      int value = GetSmall(t);

      if (dist<32 &&
          value>=0 && value < (unsigned long)(1<<31)>>dist ||
	  value<0 && value >= (long)(-1<<31)>>dist)
	return make_integer_check(value<<dist,p);
	*/
    }

  return bn_call(Arg, bn_lshift, t, MakeInteger(Arg,dist), p);
}


static tagged_t rsh_internal(Arg,t,dist,p)
     Argdecl;
     tagged_t t;
     int dist;
     bcp_t p;
{
  /*extern bn_rshift();*/

  
  if (TagIsSmall(t))
    {
      if (dist>=WORDSIZE)
	return MakeSmall((t>=TaggedZero)-1);
      else
	return ((int)((t>>dist)-(TaggedZero>>dist)) & -4) + TaggedZero;
    }



  return bn_call(Arg, bn_rshift, t, MakeInteger(Arg,dist), p);
}

tagged_t fu2_lsh(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$<<", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  int dist;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  dist = GetInteger(u);

  return (dist<0 ? rsh_internal(Arg,t,-dist,p) : lsh_internal(Arg,t,dist,p));
}

tagged_t fu2_rsh(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$>>", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  int dist;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  dist = GetInteger(u);

  return (dist<0 ? lsh_internal(Arg,t,-dist,p) : rsh_internal(Arg,t,dist,p));
}


/*  GCD for rat arithm., ch feb 92
    
    This works through the following mechanism:
		
    - The arithm. functions (via is/2,</2,...) apply NDEREF to their
      args which calls evaluate [wamsupport.c] which, when it sees
      terms, calls the corresponding function ( max arity = 2) Eval
      calls this functions with p = NULL which means that numstack_end
      must not be NULL (make_float_check, make_integer_check)
*/

tagged_t fu2_gcd(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$gcd", 3);
  //  CIAO_REGISTER tagged_t t1,u,v;
  CIAO_REG_1(tagged_t, u);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, v);

  CIAO_REGISTER int type = 3;			/* big x big */ 
  /*extern bn_quotient_remainder_quot_not_wanted(), bn_minus(); */
  /*extern bool_t bn_quotient_wanted;*/
  
  /*extern bool_t bn_positive PROTO((Bignum *x));*/
  
  u=X0; 
  NDEREF_I(Arg, u, 0, t1);
  if (TagIsSmall(u)) {
    type -= 2;
    if (u<=TaggedZero)
      u = (u==TaggedLow ? make_integer_check(Arg, GetSmall(TaggedHigh),p)
	                : TaggedZero-(u-TaggedZero));
  } 
  else if (!bn_positive(TagToSTR(u)))
    u = bn_call(Arg,bn_minus,u,0,p);

  v=X1; 
  NDEREF_I(Arg, v, 1, t1);
  if (TagIsSmall(v)) {
    type -= 1;
    if (v<=TaggedZero)
      v = (v==TaggedLow ? make_integer_check(Arg, GetSmall(TaggedHigh),p)
	                : TaggedZero-(v-TaggedZero));
  } 
  else if (!bn_positive(TagToSTR(v)))
    v = bn_call(Arg,bn_minus,v,0,p);
                                
  if ( u==TaggedZero ) return v;
  if ( v==TaggedZero ) return u;
  /*bn_quotient_wanted = FALSE;*/

  for (;;) {
    switch (type) {  			/*     u x v     */

      case 0:				/* small x small */
  small_x_small:
      	{ CIAO_REGISTER unsigned long int x = GetSmall(u), y = GetSmall(v);
      	  for (;;) {
	    x = x % y; if ( x==0 ) return MakeSmall(y);
	    y = y % x; if ( y==0 ) return MakeSmall(x);
      	  }
	}

      case 1:				/* small x big   */
	v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u,p);
	if ( v==TaggedZero ) return u;
	goto small_x_small;

      case 2:                           /*   big x small */
 	u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v,p);
	if ( u==TaggedZero ) return v;
	goto small_x_small;

      case 3:				/*   big x big   */
	u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v,p); 
	if ( u==TaggedZero ) return v;
	if ( TagIsSmall(u) ) type -= 2;
	v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u,p); 
	if ( v==TaggedZero ) return u;
	if ( TagIsSmall(v) ) type -= 1;
    }
  }
} 




#include <math.h>

#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(Win32) || defined(IRIX) || defined(BSD)
# define aint(f) (f>=0.0 ? floor(f) : ceil(f))
#endif

tagged_t fu1_intpart(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$float_integer_part", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  ENG_FLT f;

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  f = GetFloat(t);
  return make_float_check(Arg, aint(f),p);
}

tagged_t fu1_fractpart(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$float_fractional_part", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  ENG_FLT f;

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  f = GetFloat(t);
  return make_float_check(Arg, f-aint(f),p);
}

tagged_t fu1_floor(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$floor", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  tagged_t f;

  /*extern bn_from_float(), bn_plus();*/

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t)) {
    if (float_is_finite(t)) {
      f = make_float_check(Arg, floor(GetFloat(t)),p);
      return bn_call(Arg,bn_from_float,f,0,p);
    }
    else 
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
  } else
    return bn_call(Arg,bn_plus,t,0,p);
}

tagged_t fu1_round(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$round", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  tagged_t f;

  /*extern bn_from_float(), bn_plus();*/

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t)) {
    if (float_is_finite(t)) {
      f = make_float_check(Arg, floor(GetFloat(t)+0.5),p);
      return bn_call(Arg,bn_from_float,f,0,p);
    }
    else 
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
  } else
    return bn_call(Arg,bn_plus,t,0,p);
}

tagged_t fu1_ceil(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$ceiling", 2);
  CIAO_REGISTER tagged_t t,t1;

  tagged_t f;

  /*extern bn_from_float(), bn_plus();*/

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  if (TagIsSmall(t) || (p==NULL && !IsFloat(t)))
    return t;
  else if (IsFloat(t)) {
    if (float_is_finite(t)) {
      f = make_float_check(Arg, ceil(GetFloat(t)),p);
      return bn_call(Arg,bn_from_float,f,0,p);
    }
    else 
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
  } else
    return bn_call(Arg,bn_plus,t,0,p);
}

tagged_t fu2_pow(Arg,X0,X1,p)
     Argdecl;
     tagged_t X0,X1;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$**", 3);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);
  u=X1; 
  NDEREF_F(Arg, u, 1, t1);
  return make_float_check(Arg, pow(GetFloat(t),GetFloat(u)),p);
}

tagged_t fu1_exp(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$exp", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  return make_float_check(Arg, exp(GetFloat(t)),p);
}

tagged_t fu1_log(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$log", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  return make_float_check(Arg, log(GetFloat(t)),p);
}

tagged_t fu1_sqrt(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$sqrt", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  return make_float_check(Arg, sqrt(GetFloat(t)),p);
}

tagged_t fu1_sin(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$sin", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);


  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  return make_float_check(Arg, sin(GetFloat(t)),p);
}

tagged_t fu1_cos(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$cos", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  return make_float_check(Arg, cos(GetFloat(t)),p);
}

tagged_t fu1_atan(Arg,X0,p)
     Argdecl;
     tagged_t X0;
     bcp_t p;
{
  ERR__FUNCTOR("arithmetic:$atan", 2);
  //  CIAO_REGISTER tagged_t t,t1;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_F(Arg, t, 0, t1);

  return make_float_check(Arg, atan(GetFloat(t)),p);
}

/* ---------------------------------------------------------------- */

/*
tagged_t fu2_arg(Arg,number,complex, p)
     Argdecl;
     CIAO_REGISTER tagged_t number, complex;
     bcp_t p;
{  
  ERR__FUNCTOR("term_basic:arg", 3);
  CIAO_REG_1(tagged_t, t0);
  CIAO_REGISTER int i;

  DerefSwitch(number,t0, BUILTIN_ERROR(INSTANTIATION_ERROR, number, 1););
  DerefSwitch(complex,t0, BUILTIN_ERROR(INSTANTIATION_ERROR, complex, 2););

  if (TagIsSmall(number)) i = GetSmall(number);
  else if (TagIsLarge(number) && !LargeIsFloat(number)) return FALSE;
  else BUILTIN_ERROR(TYPE_ERROR(INTEGER), number, 1);

  if (i < 0)
    BUILTIN_ERROR(DOMAIN_ERROR(NOT_LESS_THAN_ZERO), number, 1);

  if (TagIsSTR(complex))
    {
      CIAO_REGISTER tagged_t f = TagToHeadfunctor(complex);

      if (i == 0 || i > Arity(f) || f&QMask) return FALSE;

      RefArg(t0,complex,i);
      return t0;
    }
  else if (IsComplex(complex))	/\* i.e. list *\/
    {
      if (i == 1)
	{
	  RefCar(t0,complex);
	  return t0;
	}
      else if (i == 2)
	{
	  RefCdr(t0,complex);
	  return t0;
	}
      else return FALSE;
    }
  // comment next line for full ISO compliance
  else if (IsAtom(complex)) return FALSE;      
  else  BUILTIN_ERROR(TYPE_ERROR(COMPOUND), complex, 2);
 

}
*/

// Old code without exception 
tagged_t fu2_arg(Arg,number,complex, p)
     Argdecl;
     CIAO_REGISTER tagged_t number, complex;
     bcp_t p;
{
  CIAO_REG_1(tagged_t, t0);
  
  DerefSwitch(number,t0,{goto barf1;});
  DerefSwitch(complex,t0,{goto barf2;});


  if (TagIsSTR(complex))
    {
      CIAO_REGISTER int i = GetSmall(number);
      CIAO_REGISTER tagged_t f = TagToHeadfunctor(complex);

      if (i<=0 || i>Arity(f) || f&QMask)
	goto barf1;
      
      RefArg(t0,complex,i);
      return t0;
    }
  else if (IsComplex(complex))	// i.e. list 
    {
      if (number==MakeSmall(1))
	{
	  RefCar(t0,complex);
	  return t0;
	}
      else if (number==MakeSmall(2))
	{
	  RefCdr(t0,complex);
	  return t0;
	}
      else
	goto barf1;
    }
  else
    goto barf2;

 barf1:
  MINOR_FAULT("arg/3: incorrect 1st argument");

 barf2:
  MINOR_FAULT("arg/3: incorrect 2nd argument");
}


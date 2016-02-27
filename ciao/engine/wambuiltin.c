/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "support_defs.h"
#include "wambuiltin.h"

/* declarations for global functions accessed here */

#include "wambuiltin_defs.h"
#include "misc_defs.h"
#include "bignum_defs.h"

/* local declarations */

#if !defined(USE_OC_EXCEPTIONS)
static char *illexp = "illegal arithmetic expression";          /* Shared */
#endif

/*
  extern int bn_compare PROTO((Bignum *x, Bignum *y));
  extern bool_t bn_positive PROTO((Bignum *x));
*/

bool_t bu2_numeq(Arg, x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  ERR__FUNCTOR("arithmetic:=:=", 2);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  
  Numstack_End = NULL;
  t=x0; NDEREF_B(Arg, t, 0, t1);
  u=x1; NDEREF_B(Arg, u, 1, t1);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t==u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)==GetFloat(u));
  else if (TagIsSmall(t) || TagIsSmall(u))
    return FALSE;
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))==0);
}

bool_t bu2_numne(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  ERR__FUNCTOR("arithmetic:=\\=", 2);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  
  Numstack_End = NULL;
  t=x0; NDEREF_B(Arg, t, 0, t1);
  u=x1; NDEREF_B(Arg, u, 1, t1);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t!=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)!=GetFloat(u));
  else if (TagIsSmall(t) || TagIsSmall(u))
    return TRUE;
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))!=0);
}

bool_t bu2_numlt(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  ERR__FUNCTOR("arithmetic:<", 2);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  
  Numstack_End = NULL;
  t=x0; NDEREF_B(Arg, t, 0, t1);
  u=x1; NDEREF_B(Arg, u, 1, t1);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t<u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<GetFloat(u));
  else if (TagIsSmall(t))
    return bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return !bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))<0);
}

bool_t bu2_numle(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  ERR__FUNCTOR("arithmetic:=<", 2);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  
  Numstack_End = NULL;
  t=x0; NDEREF_B(Arg, t, 0, t1);
  u=x1; NDEREF_B(Arg, u, 1, t1);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t<=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<=GetFloat(u));
  else if (TagIsSmall(t))
    return bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return !bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))<=0);
}

bool_t bu2_numgt(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  ERR__FUNCTOR("arithmetic:>", 2);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  
  Numstack_End = NULL;
  t=x0; NDEREF_B(Arg, t, 0, t1);
  u=x1; NDEREF_B(Arg, u, 1, t1);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t>u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>GetFloat(u));
  else if (TagIsSmall(t))
    return !bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))>0);
}

bool_t bu2_numge(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  ERR__FUNCTOR("arithmetic:>=", 2);
  //  CIAO_REGISTER tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  
  Numstack_End = NULL;
  t=x0; NDEREF_B(Arg, t, 0, t1);
  u=x1; NDEREF_B(Arg, u, 1, t1);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t>=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>=GetFloat(u));
  else if (TagIsSmall(t))
    return !bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))>=0);
}

bool_t bu1_atom(Arg,x0)
     Argdecl;
     CIAO_REG_1(tagged_t, x0);
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return (TermIsATM(x0));
}

bool_t bu1_atomic(Arg,x0)
     Argdecl;
     CIAO_REG_1(tagged_t, x0);
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return (!(x0 & TagBitComplex) || TagIsLarge(x0));
}

bool_t bu1_float(Arg,x0)
     Argdecl;
     CIAO_REG_1(tagged_t, x0);
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsFloat(x0);
}

bool_t bu1_integer(Arg,x0)
     Argdecl;
     CIAO_REG_1(tagged_t, x0);
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsInteger(x0);
}

bool_t bu1_number(Arg,x0)
     Argdecl;
     CIAO_REG_1(tagged_t, x0);
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsNumber(x0);
}

bool_t bu1_var(Arg,x0)
     Argdecl;
     CIAO_REG_1(tagged_t, x0);
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return TRUE;})
  return FALSE;
}

bool_t bu1_nonvar(Arg,x0)
     Argdecl;
     CIAO_REG_1(tagged_t, x0);
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return TRUE;
}

bool_t bu2_lexeq(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  extern int compare_help();

  return (x0==x1 || compare_help(Arg,x0,x1)==0);
}

bool_t bu2_lexne(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  return (x0!=x1 && compare_help(Arg,x0,x1)!=0);
}

bool_t bu2_lexlt(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  return (x0!=x1 && compare_help(Arg,x0,x1)<0);
}

bool_t bu2_lexle(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  return (x0==x1 || compare_help(Arg,x0,x1)<=0);
}

bool_t bu2_lexgt(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  return (x0!=x1 && compare_help(Arg,x0,x1)>0);
}

bool_t bu2_lexge(Arg,x0,x1)
     Argdecl;
     tagged_t x0,x1;
{
  return (x0==x1 || compare_help(Arg,x0,x1)>=0);
}

tagged_t fu2_compare(Arg,x1,x2)
     Argdecl;
     tagged_t x1,x2;
{
  int i;

  if (x1==x2)
    return atom_equal;
  else if ((i=compare_help(Arg,x1,x2)) < 0)
    return atom_lessthan;
  else if (i>0)
    return atom_greaterthan;
  else
    return atom_equal;
}


/*---------------------------------------------------------------*/
/*
bool_t bu3_functor(Arg,term,name,arity)
     Argdecl;
     CIAO_REG_1(tagged_t, term);
     CIAO_REGISTER tagged_t name,arity;
{
  ERR__FUNCTOR("term_basic:functor", 3);
  CIAO_REGISTER tagged_t t0;

  DerefSwitch(term,t0,{goto construct;});
    {
      tagged_t tagarity;
      
      if (TermIsAtomic(term))
	tagarity = TaggedZero;
      else if (!(term & TagBitFunctor))
	term = atom_list,
	tagarity = MakeSmall(2);
      else
	{
	  tagged_t f = TagToHeadfunctor(term);
	  
	  term = SetArity(f,0),
	  tagarity = MakeSmall(Arity(f));
	}

      Unify_constant(tagarity,arity);
      return cunify(Arg,term,name);
    }
 construct:
    {
      DerefSwitch(name, t0, 
		  BUILTIN_ERROR(INSTANTIATION_ERROR, name, 2););
      DerefSwitch(arity, t0,
		  BUILTIN_ERROR(INSTANTIATION_ERROR, arity, 3););

      if (TermIsAtomic(name)) 
	{
	  if (arity == TaggedZero) return cunify(Arg,name,term);
	  else if (arity > TaggedZero) 
	    {
	      if (TagIsATM(name)) 
		{
		  if (arity < MakeSmall(ARITYLIMIT))
		    return 
		      cunify(Arg, make_structure(Arg, SetArity(name,GetSmall(arity))), term);
		  else if (IsInteger(arity)) 
		    BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ARITY), arity, 3);
		  else
		    BUILTIN_ERROR(TYPE_ERROR(INTEGER),arity, 3);
		}
	      else if (IsInteger(arity))
		BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), name, 2);
	      else
		BUILTIN_ERROR(TYPE_ERROR(INTEGER), arity, 3);
	    }
	  else
	    BUILTIN_ERROR(DOMAIN_ERROR(NOT_LESS_THAN_ZERO), arity, 3);
	}
      else
	BUILTIN_ERROR(TYPE_ERROR(ATOMIC), name, 2);
    }
    
}
*/

// Old code without exception
bool_t bu3_functor(Arg,term,name,arity)
     Argdecl;
     CIAO_REG_1(tagged_t, term);
     CIAO_REGISTER tagged_t name,arity;
{
  CIAO_REGISTER tagged_t t0;
  
  DerefSwitch(term,t0,{goto construct;});
    {
      tagged_t tagarity;
      
      if (TermIsAtomic(term))
	tagarity = TaggedZero;
      else if (!(term & TagBitFunctor))
	term = atom_list,
	tagarity = MakeSmall(2);
      else
	{
	  tagged_t f = TagToHeadfunctor(term);
	  
	  term = SetArity(f,0),
	  tagarity = MakeSmall(Arity(f));
	}

      Unify_constant(tagarity,arity);
      return cunify(Arg,term,name);
    }
 construct:
    {
      DerefSwitch(name,t0,;);
      DerefSwitch(arity,t0,;);
      if (TermIsAtomic(name) && (arity==TaggedZero))
	return cunify(Arg,name,term);
      else if (TagIsATM(name) &&
	       (arity>TaggedZero) && (arity<MakeSmall(ARITYLIMIT)))
	return cunify(Arg,
                      make_structure(Arg, SetArity(name,GetSmall(arity))),
                      term);
      else
	return FALSE;
    }
}


/*---------------------------------------------------------------*/

bool_t bu2_univ(Arg,term,list)
     Argdecl;
     CIAO_REGISTER tagged_t term;
     tagged_t list;
{ 
  ERR__FUNCTOR("term_basic:=..", 2);
  CIAO_REG_1(tagged_t, car);
  CIAO_REG_2(tagged_t, cdr);
  CIAO_REG_Z(tagged_t, *argp);
  CIAO_REG_Z(tagged_t, *argq);
  int arity;
  tagged_t f;

  DerefSwitch(term,car,{goto construct;});
  cdr = atom_nil;
  if (TermIsAtomic(term))
    {
      MakeLST(cdr,term,cdr);
      return cunify(Arg,cdr,list);
    }
  
  if (term & TagBitFunctor)
    f = TagToHeadfunctor(term),
    argp = TagToArg(term,1),
    argq = HeapOffset(argp,Arity(f));
  else
    f = functor_list,
    argp = TagToCar(term),
    argq = HeapOffset(argp,2);

  while HeapYounger(argq,argp)
    {
      HeapDecr(argq);
      RefHeap(car,argq);
      MakeLST(cdr,car,cdr);
    }
  MakeLST(cdr,SetArity(f,0),cdr);
  return cunify(Arg,cdr,list);

 construct:
  cdr = list;
  DerefSwitch(cdr,car, BUILTIN_ERROR(INSTANTIATION_ERROR, list, 2););
  arity = 0;

  if (IsVar(cdr))
    goto bomb;
  if (!TagIsLST(cdr))
    {
      if (cdr == atom_nil)
	BUILTIN_ERROR(DOMAIN_ERROR(NON_EMPTY_LIST), list, 2); 
      else
	BUILTIN_ERROR(TYPE_ERROR(LIST), list, 2); 
    }

  DerefCar(f,cdr);
  DerefCdr(cdr,cdr);
  if (cdr==atom_nil)
    {
      if (TermIsAtomic(f))
	return cunify(Arg,f,term);
      else 
	BUILTIN_ERROR(TYPE_ERROR(ATOMIC), f, 2); 
    }
  else if (IsVar(f))
    goto bomb;
  else if (!TagIsATM(f))
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), f, 2); 


  argp = w->global_top;
  HeapPush(w->global_top,f);
  while (TagIsLST(cdr) && arity<ARITYLIMIT)
    {
      DerefCar(car,cdr);
      DerefCdr(cdr,cdr);
      HeapPush(w->global_top,car);
      arity++;
    }
  if (IsVar(cdr))
    goto bomb;
  if (arity==ARITYLIMIT)
    BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ARITY), list, 2);
  if (cdr!=atom_nil) 
    BUILTIN_ERROR(TYPE_ERROR(LIST), list, 2);
  
  f = SetArity(f,arity);
  if (f==functor_list)
    {
      w->global_top = argp;
      argq = HeapOffset(w->global_top,1);
      RefHeapNext(car,argq);
      RefHeapNext(cdr,argq);
      HeapPush(w->global_top,car);
      HeapPush(w->global_top,cdr);
      return cunify(Arg,Tag(LST,argp),term);
    }
  else
    {
      *argp = f;
      return cunify(Arg,Tag(STR,argp),term);
    }

 bomb:
  BUILTIN_ERROR(INSTANTIATION_ERROR,list, 2);
}

/*
// Old code without exception
bool_t bu2_univ(Arg,term,list)
     Argdecl;
     CIAO_REGISTER tagged_t term;
     tagged_t list;
{
  CIAO_REG_1(tagged_t, car);
  CIAO_REG_2(tagged_t, cdr);
  CIAO_REG_Z(tagged_t *, argp);
  CIAO_REG_Z(tagged_t *, argq);
  int arity;
  tagged_t f;

  DerefSwitch(term,car,{goto construct;});
  cdr = atom_nil;
  if (TermIsAtomic(term))
    {
      MakeLST(cdr,term,cdr);
      return cunify(Arg,cdr,list);
    }
  
  if (term & TagBitFunctor)
    f = TagToHeadfunctor(term),
    argp = TagToArg(term,1),
    argq = HeapOffset(argp,Arity(f));
  else
    f = functor_list,
    argp = TagToCar(term),
    argq = HeapOffset(argp,2);
  
  while HeapYounger(argq,argp)
    {
      HeapDecr(argq);
      RefHeap(car,argq);
      MakeLST(cdr,car,cdr);
    }
  MakeLST(cdr,SetArity(f,0),cdr);
  return cunify(Arg,cdr,list);

 construct:
  cdr = list;
  DerefSwitch(cdr,car,;);
  arity = 0;

  if (IsVar(cdr))
    goto bomb;
  if (!TagIsLST(cdr))
    MINOR_FAULT("=../2: incorrect 2nd argument");
  DerefCar(f,cdr);
  DerefCdr(cdr,cdr);
  if (TermIsAtomic(f) && (cdr==atom_nil))
    return cunify(Arg,f,term);
  else if (IsVar(f))
    goto bomb;
  else if (!TagIsATM(f))
    MINOR_FAULT("=../2: incorrect 2nd argument");
  
  argp = w->global_top;
  HeapPush(w->global_top,f);
  while (TagIsLST(cdr) && arity<ARITYLIMIT)
    {
      DerefCar(car,cdr);
      DerefCdr(cdr,cdr);
      HeapPush(w->global_top,car);
      arity++;
    }
  if (IsVar(cdr))
    goto bomb;
  if (cdr!=atom_nil || arity==ARITYLIMIT)
    MINOR_FAULT("=../2: incorrect 2nd argument");
  
  f = SetArity(f,arity);
  if (f==functor_list)
    {
      w->global_top = argp;
      argq = HeapOffset(w->global_top,1);
      RefHeapNext(car,argq);
      RefHeapNext(cdr,argq);
      HeapPush(w->global_top,car);
      HeapPush(w->global_top,cdr);
      return cunify(Arg,Tag(LST,argp),term);
    }
  else
    {
      *argp = f;
      return cunify(Arg,Tag(STR,argp),term);
    }

 bomb:
    USAGE_FAULT("=../2: illegal arguments");
}
*/


/* Support for if/3 */
bool_t bu1_if(Arg,x0)
     Argdecl;
     CIAO_REGISTER tagged_t x0;
{
  DEREF(x0,x0);
  CTagToCar(x0) = atom_true;
  return TRUE;
}

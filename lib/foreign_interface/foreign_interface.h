/* Macros and auxiliar functions needed by compile_c.pl for the glue-code
   generation. */

#if defined(DARWIN)
#include <stdlib.h>
#else
#include <malloc.h>
#endif
#include <string.h>

/* Functors */

TAGGED functor_address;

#define FUNCTOR_DEFINITION_CODE \
functor_address = SetArity(MakeString("$address"),1);

/* Prolog to C conversion code */

#define GET_INTEGER(N,C) \
  DEREF(X(N),X(N)); \
  if (!IsInteger(X(N))) { \
    ERROR_IN_ARG(X(N),N+1,INTEGER); \
  } else C = GetInteger(X(N));

#define GET_NUMBER(N,C) \
  DEREF(X(N),X(N)); \
  if (!IsNumber(X(N))) { \
    ERROR_IN_ARG(X(N),N+1,NUMBER); \
  } else C = GetFloat(X(N));

#define GET_CSTRING_FROM_ATOM(N,C) \
  DEREF(X(N),X(N)); \
  if (!IsAtom(X(N))) { \
    ERROR_IN_ARG(X(N),N+1,STRICT_ATOM); \
  } else { \
    char *s; \
    int l; \
    s = GetString(X(N)); \
    l = strlen(s) + 1; \
    C = (char *)malloc(sizeof(char) * l); \
    strcpy(C,s); \
  }

#define BYTES_TEST(N,C,Len) \
  DEREF(X(N),X(N)); \
  if (TestStringAndLength(Arg,X(N)) != Len) { \
    USAGE_FAULT("foreign interface: list length or data inconsistency."); \
  }

#define GET_BYTES(N,C,Len) \
  if (Len) C = GetBytesFromList(Arg,X(N),Len,0); else C = NULL;

#define INTS_TEST(N,C,Len) \
  DEREF(X(N),X(N)); \
  if (TestIntsAndLength(Arg,X(N)) != Len) { \
    USAGE_FAULT("foreign interface: list length or data inconsistency."); \
  }

#define GET_INTS(N,C,Len) \
  if (Len) C = GetIntsFromList(Arg,X(N),Len,0); else C = NULL;

#define STRING_TEST(N,C) \
  DEREF(X(N),X(N)); \
  if (((int)(C = (char *)TestStringAndLength(Arg,X(N)))) == -1) { \
    ERROR_IN_ARG(X(N),N+1,CHARACTER_CODE_LIST); \
  }  

#define GET_CSTRING_FROM_LIST(N,C) \
  C = GetBytesFromList(Arg,X(N),(int)C,1);

int TestStringAndLength(Arg,cdr)
     Argdecl;
     TAGGED cdr;
{
  int len;
  TAGGED car;

  for (len=0; cdr!=atom_nil; len++) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!TagIsSmall(car) || (car<TaggedZero) || (car>=MakeSmall(256))) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? len : (-1);
}

char *GetBytesFromList(Arg,cdr,len,zero_ended)
     Argdecl;
     TAGGED cdr;
     int len;
     int zero_ended;
{
  int i;
  char *s;
  TAGGED car;

  s = (char *)malloc(sizeof(char) * (len + (zero_ended != 0)));
  for (i = 0; i < len; i++) {
    DerefCar(car,cdr);
    s[i] = GetSmall(car);
    DerefCdr(cdr,cdr);
  }
  if (zero_ended) s[i] = 0;
  return s;
}     

int TestIntsAndLength(Arg,cdr)
     Argdecl;
     TAGGED cdr;
{
  int len;
  TAGGED car;

  for (len=0; cdr!=atom_nil; len++) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsInteger(car)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? len : (-1);
}

int *GetIntsFromList(Arg,cdr,len,zero_ended)
     Argdecl;
     TAGGED cdr;
     int len;
     int zero_ended;
{
  int i;
  int *s;
  TAGGED car;

  s = (int *)malloc(sizeof(int) * (len + (zero_ended != 0)));
  for (i = 0; i < len; i++) {
    DerefCar(car,cdr);
    s[i] = GetInteger(car);
    DerefCdr(cdr,cdr);
  }
  if (zero_ended) s[i] = 0;
  return s;
}     

#define GET_ADDRESS(N,C) \
  DEREF(X(N),X(N)); \
  if (TagIsSTR(X(N)) && (TagToHeadfunctor(X(N))==functor_address)) { \
    REGISTER TAGGED px; \
    DerefArg(px,X(N),1); \
    if (IsInteger(px)) { \
      C = (void *)GetInteger(px); \
    } else { \
      ERROR_IN_ARG(X(N),N+1,INTEGER); \
    } \
  } else { \
    USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)") \
  } 

/* C to Prolog conversion code */

#define MAKE_ATOM(Arg,X) MakeString(X)

#define MAKE_STRING(Arg,X) MakeList(Arg,X,strlen(X))

TAGGED MakeList(Arg,s,len)
     Argdecl;
     char *s;
     int len;
{
  int i;
  TAGGED cdr;
  
  s += len;
  if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(len<<1))
    explicit_heap_overflow(Arg,CONTPAD+(len<<1),2);

  cdr = atom_nil;
  for (i = 0; i < len; i++) {
    MakeLST(cdr,MakeSmall(*(--s)),cdr);
  }

  return cdr;
}

TAGGED MakeIntList(Arg,s,len)
     Argdecl;
     int *s;
     int len;
{
  int i;
  TAGGED cdr;

  /* NOTE: This overflow check is not very accurate ... */
  s += len;
  if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(len<<4 /* Was 1...*/))
    explicit_heap_overflow(Arg,CONTPAD+(len<<4),2);

  cdr = atom_nil;
  for (i = 0; i < len; i++) {
    s--;

    MakeLST(cdr,MakeInteger(Arg, *s),cdr);
  }

  return cdr;
}

TAGGED MakeAddress(Arg,address)
     Argdecl;
     void *address;
{
  REGISTER TAGGED *pt1;
  REGISTER TAGGED address_as_integer;

  address_as_integer = MakeInteger(Arg, (int)address);
  pt1 = w->global_top;
  HeapPush(pt1, functor_address);
  HeapPush(pt1, address_as_integer);
  w->global_top = pt1;  
  return Tag(STR, HeapOffset(pt1, -2));
}

/* Free code */

#define FREE(X) if (X) free(X);









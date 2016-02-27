/* -------------------------- */
/*          Includes          */
/* -------------------------- */
//#include "ciao_prolog.h"


/* -------------- */
/*      Tags      */
/* -------------- */
/* WARNING: these macros need Ciao tag scheme */

#define ENGINE_PairInitTag  0xC0000000  //to mark the begining of a list in trie.c
#define ENGINE_PairEndTag   0xD0000000  //to mark the end of a list in trie.c

#define ENGINE_CommaInitTag 0x70000000  //to mark the begining of a comma functor in trie.c
#define ENGINE_CommaEndTag  0x60000000  //to mark the end of a comma functor in trie.c

#define ENGINE_FloatInitTag 0xB0000000  //to mark the begining of a comma functor in trie.c
#define ENGINE_FloatEndTag  0xF0000000  //to mark the end of a comma functor in trie.c


/* ------------------------- */
/*      Term Definition      */
/* ------------------------- */
#define ENGINE_Term tagged_t


/* ------------------------- */
/*      Construct Terms      */
/* ------------------------- */
#define ENGINE_MkVarTerm() (my_make_var(Arg->misc->goal_desc_ptr))
#define ENGINE_MkIntTerm(INT) (my_make_integer(Arg->misc->goal_desc_ptr,(INT)))
#define ENGINE_MkFloatTerm(FLOAT) (my_make_float(Arg->misc->goal_desc_ptr,(FLOAT)))
#define ENGINE_MkAtomTerm(ATOM) (MakeString(ATOM))
#define ENGINE_MkPairTerm(HEAD,TAIL) (my_make_list(Arg->misc->goal_desc_ptr,(HEAD),(TAIL)))
#define ENGINE_MkApplTerm(FUNCTOR,ARITY,ARGS) (my_make_functor(Arg->misc->goal_desc_ptr,(FUNCTOR),(ARITY),(ARGS))) 
          

/* ------------------------ */
/*      Destruct Terms      */
/* ------------------------ */
#define ENGINE_IntOfTerm(TERM) (GetInteger(TERM)) 
#define ENGINE_FloatOfTerm(TERM) (GetFloat(TERM)) 
#define ENGINE_AtomName(ATOM) (((atom_t *)TagToAtom(ATOM))->name)
#define ENGINE_HeadOfTerm(TERM) (CTagToPointer(TERM))
#define ENGINE_TailOfTerm(TERM) (*(TagToPointer(TERM) + 1))
#define ENGINE_ArgOfTerm(A,TERM) (CTagToArg(TERM,A))
#define ENGINE_NameOfFunctor(FUNCTOR) (((atom_t *)TagToAtom(SetArity(TagToHeadfunctor(FUNCTOR),0)))->name)
#define ENGINE_ArityOfFunctor(FUNCTOR)(Arity(TagToHeadfunctor(FUNCTOR)))


/* -------------------- */
/*      Test Terms      */
/* -------------------- */
#define ENGINE_IsIntTerm(TERM) (IsInteger(TERM))
#define ENGINE_IsFloatTerm(TERM) (IsFloat(TERM))
#define ENGINE_IsAtomTerm(TERM) (IsAtom(TERM)) 
#define ENGINE_IsPairTerm(TERM) (TagIsLST(TERM)) 
#define ENGINE_IsApplTerm(TERM) (TagIsSTR(TERM) && !IsNumber(TERM))
#define ENGINE_IsVarTerm(TERM) (IsVar(TERM))
#define ENGINE_IsNonVarTerm(TERM) (!IsVar(TERM))
#define IsFreeVar(X) (IsVar(X) && ((X) == CTagToPointer(X)))


/* -------------------- */
/*      Unification     */
/* -------------------- */
#define ENGINE_Unify(TERM1,TERM2) (cunify(Arg,(TERM1),(TERM2)))


/* ---------------------------- */
/*      Predicate Arguments     */
/* ---------------------------- */
#define ENGINE_ARG1 X(0)
#define ENGINE_ARG2 X(1)
#define ENGINE_ARG3 X(2)
#define ENGINE_ARG4 X(3)
#define ENGINE_ARG5 X(4)
#define ENGINE_ARG6 X(5)
#define ENGINE_ARG7 X(6)
#define ENGINE_ARG8 X(7)
#define ENGINE_ARG9 X(8)
#define ENGINE_ARG10 X(9)
#define ENGINE_ARG11 X(10)
#define ENGINE_ARG12 X(11)
#define ENGINE_ARG13 X(12)
#define ENGINE_ARG14 X(13)
#define ENGINE_ARG15 X(14)
#define ENGINE_ARG16 X(15)



/* --------------------- */
/*      Memory Space     */
/* --------------------- */
#define ENGINE_AllocSpaceFromYap(N) (mem_alloc(N))
#define ENGINE_FreeSpaceFromYap(N) (ciao_free(N))

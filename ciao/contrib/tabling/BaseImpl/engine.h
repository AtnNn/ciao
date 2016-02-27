/* -------------------------- */
/*          Includes          */
/* -------------------------- */
#include "ciao_prolog.h"


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
#define ENGINE_Term ciao_term


/* ------------------------- */
/*      Construct Terms      */
/* ------------------------- */
#define ENGINE_MkVarTerm() (ciao_var())
#define ENGINE_MkIntTerm(INT) (ciao_integer(INT))
#define ENGINE_MkFloatTerm(FLOAT) (ciao_float(FLOAT))
#define ENGINE_MkAtomTerm(ATOM) (ciao_atom(ATOM)) 
#define ENGINE_MkPairTerm(HEAD,TAIL) (ciao_list(HEAD,TAIL))
#define ENGINE_MkApplTerm(FUNCTOR,ARITY,ARGS_TERMS) (ciao_structure_a(FUNCTOR,ARITY,ARGS_TERMS)) 
          

/* ------------------------ */
/*      Destruct Terms      */
/* ------------------------ */
#define ENGINE_IntOfTerm(TERM) (ciao_to_integer(TERM)) 
#define ENGINE_FloatOfTerm(TERM) (ciao_to_float(TERM)) 
#define ENGINE_AtomName(ATOM) (ciao_atom_name(ATOM))
#define ENGINE_HeadOfTerm(TERM) (ciao_list_head(TERM))
#define ENGINE_TailOfTerm(TERM) (ciao_list_tail(TERM))
#define ENGINE_ArgOfTerm(A,TERM) (ciao_structure_arg(TERM,A))
#define ENGINE_NameOfFunctor(FUNCTOR) (ciao_structure_name(FUNCTOR))
#define ENGINE_ArityOfFunctor(FUNCTOR)(ciao_structure_arity(FUNCTOR))


/* -------------------- */
/*      Test Terms      */
/* -------------------- */
#define ENGINE_IsIntTerm(TERM) (ciao_is_integer(TERM))
#define ENGINE_IsFloatTerm(TERM) (ciao_is_float(TERM))
#define ENGINE_IsAtomTerm(TERM) (ciao_is_atom(TERM)) 
#define ENGINE_IsPairTerm(TERM) (ciao_is_list(TERM)) 
#define ENGINE_IsApplTerm(TERM) (ciao_is_structure(TERM))
#define ENGINE_IsVarTerm(TERM) (ciao_is_variable(TERM))
#define ENGINE_IsNonVarTerm(TERM) (!ciao_is_variable(TERM))


/* -------------------- */
/*      Unification     */
/* -------------------- */
#define ENGINE_Unify(TERM1,TERM2) (ciao_unify(TERM1,TERM2))


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



/* ------------------ */
/*      Execution     */
/* ------------------ */
#define ENGINE_CallProlog(TERM) (ciao_commit_call_term(TERM))


/* --------------------- */
/*      Memory Space     */
/* --------------------- */
#define ENGINE_AllocSpaceFromYap(N) (ciao_malloc(N))
#define ENGINE_FreeSpaceFromYap(N) (ciao_free(N))


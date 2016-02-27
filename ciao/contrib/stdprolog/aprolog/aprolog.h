#ifndef APROLOG_H
#define APROLOG_H

#include <stdio.h>
#include <stdlib.h>

#define APROLOG_VERSION "1.22" /**** pts ****/

extern int if_debug;
extern int if_profile;
extern FILE *debfp;

#ifdef DEBUG
#define DEB(X) if(if_debug) {X;}
#define PROF(X) if(if_profile) {X;}
#else
#define DEB(X)
#define PROF(X)
#endif


typedef union  _Tagged   Tagged;
typedef struct _Functor  Functor;
typedef struct _Adm      Adm;
typedef struct _Regs     Regs;
typedef struct _Array    Array;
typedef struct _Clause   Clause;
typedef struct _GoalHead GoalHead;

typedef enum {
    FALSE,
    TRUE
} Bool;

typedef enum {
    TAG_VAR = 0,
    TAG_REF = 0,
    TAG_STR = 1,
    TAG_ATM = 2,
    TAG_FLT = 3,
    TAG_TMP = 4
} Tag;

enum {
    FUNCT_NIL,
    FUNCT_TRUE,
    FUNCT_FAIL, 
    FUNCT_FALSE,
    FUNCT_DOT,
    FUNCT_COMMA,
    FUNCT_CALL_DYNAMIC,
    FUNCT_DEBUG,
    FUNCT_CALL_ERROR,
    FUNCT_INTERRUPT,
    FUNCT_ASSERTA,
    FUNCT_ASSERTZ,
    FUNCT_ASSERTS,
    FUNCT_MAIN,
    FUNCT_CL,
    FUNCT_LESS,
    FUNCT_LEQ,
    FUNCT_EQ,
    FUNCT_NEQ,
    FUNCT_INTEGER,
    FUNCT_FLOAT,
    FUNCT_NUMBER,
    FUNCT_ATOM,
    FUNCT_ATOMIC,
    FUNCT_COMPOUND,
    FUNCT_CALLABLE,
    FUNCT_NONVAR,
    FUNCT_VAR,
    FUNCT_READ,
    FUNCT_WRITE,
    FUNCT_APPEND,
    
    FUNCT_EEXIST,
    FUNCT_EPERM,
    FUNCT_ESYSTEM,

    FUNCT_A_NEG,
    FUNCT_A_NOT,
    FUNCT_A_ABS,
    FUNCT_A_SIGN,
    FUNCT_A_INT,
    FUNCT_A_FRAC,
    FUNCT_A_ROUND,
    FUNCT_A_TRUNCATE,
    FUNCT_A_FLOOR,
    FUNCT_A_CEILING,
    FUNCT_A_SIN,
    FUNCT_A_COS,
    FUNCT_A_ATAN,
    FUNCT_A_SQRT,
    FUNCT_A_LOG,
    FUNCT_A_EXP,

    FUNCT_A_ADD,
    FUNCT_A_SUB,
    FUNCT_A_MUL,
    FUNCT_A_IDIV,
    FUNCT_A_REM,
    FUNCT_A_MOD,
    FUNCT_A_AND,
    FUNCT_A_OR,
    FUNCT_A_SHL,
    FUNCT_A_SHR,
    FUNCT_A_DIV,
    FUNCT_A_POW,

    FUNCT_ACCESS_USER,
    FUNCT_ACCESS_PROLOG,
    FUNCT_ACCESS_HIDDEN,

    FUNCT_PRED_STATIC,
    FUNCT_PRED_DYNAMIC,
    FUNCT_PRED_BUILTIN,
    FUNCT_PRED_GLOBVAR,
    
    FUNCT_ERROR,
    FUNCT_CURR_INPUT,
    FUNCT_CURR_OUTPUT,
    FUNCT_CURR_ERROR,
    FUNCT_VERSION, /**** pts ****/
    FUNCT_VERSION_VALUE, /*** pts ****/

    FUNCT_TYPE_ERROR,
    FUNCT_ZERO_DIVISOR,
    FUNCT_FLOAT_OVERFLOW,
    FUNCT_INT_OVERFLOW,
    FUNCT_UNDEFINED,

    FUNCT_LASTDEF
};

#define FUNCT_A_FLOAT FUNCT_FLOAT

typedef signed int Index;

typedef Bool (*BuiltIn) (Regs *regs, Index functor, Index args);

union _Tagged {
    struct {
        unsigned int isint : 1;
        signed   int i     : 31;
    } i;
    struct {
        unsigned int isint : 1;
        Tag          tag   : 3;
        Index        index : 28;
    } o;
};

#define ISVAR(t) (!t.i.isint && t.o.tag == TAG_VAR)
#define ISREF(t) (!t.i.isint && t.o.tag == TAG_REF)
#define ISSTR(t) (!t.i.isint && t.o.tag == TAG_STR)
#define ISATM(t) (!t.i.isint && t.o.tag == TAG_ATM)
#define ISFLT(t) (!t.i.isint && t.o.tag == TAG_FLT)
#define ISTMP(t) (!t.i.isint && t.o.tag == TAG_TMP)
#define ISINT(t) (t.i.isint)

#define SETVAR(t) t.i.isint = 0, t.o.tag = TAG_VAR
#define SETREF(t) t.i.isint = 0, t.o.tag = TAG_REF
#define SETSTR(t) t.i.isint = 0, t.o.tag = TAG_STR
#define SETATM(t) t.i.isint = 0, t.o.tag = TAG_ATM
#define SETFLT(t) t.i.isint = 0, t.o.tag = TAG_FLT
#define SETTMP(t) t.i.isint = 0, t.o.tag = TAG_TMP
#define SETINT(t) t.i.isint = 1

#define INT(t)             (t.i.i)
#define INDEX(t)           (t.o.index)
#define ATMFUNCT(t)        INDEX(t)
#define AT(i,t)            (i + INDEX(t))
#define ATI(i)             (i + INDEX(TAGGED(i)))

#define NEXTGOAL(pos)          ((pos) + STACK((pos), Index))
#define SETNEXTGOAL(pos, next) STACK((pos), Index) = (next) - (pos)

typedef enum {
    PRED_UNDEF,
    PRED_STATIC,
    PRED_DYNAMIC,
    PRED_BUILTIN,
    PRED_GLOBVAR
} PredType;

typedef enum {
    ACCESS_USER,
    ACCESS_PROLOG,
    ACCESS_HIDDEN
} PredAccess;

struct _Functor {
    char *name;
    unsigned char arity;
    PredType      type   : 8;
    PredAccess    access : 8;
    Index namefunct;
    union {
        BuiltIn builtin;
        Clause *clauses;
        Index globvar;
    } pred;
    Tagged blackboard;
#ifdef DEBUG
    unsigned int callctr;
#endif
};

struct _Adm {
    Index previous;
    Index prevchoice;
    Index goals;
    Clause *clauses;
    Index assigs;
    Index numassig;
    Index clauselist;
};

struct _Regs {
    Index lastadm;
    Index goals;
    Bool again;
    int trace;
};

struct _Array {
    unsigned int *array;
    unsigned int size;
    unsigned int end;
    unsigned int elemsize;
};

struct _Clause {
    Clause *next;
    Index funct;
    int refctr;
    unsigned int headsize;
    unsigned int firstgoal;
    unsigned int lastgoal;
    Array clause;
};

#define ARRAY(arr, type)  ((type *) (arr).array)

extern Array functors;
#define FUNCTOR(pos)      ARRAY(functors, Functor)[pos]

extern Array stack;
#define TAGGED(pos)       ARRAY(stack, Tagged)[pos]
#define STACK(pos, type)  (*(type *)&TAGGED(pos))

extern Array trail;
#define TRAIL(pos)        ARRAY(trail, Tagged)[pos]

#define CLAUSE(cl, pos, type)  (*(type *)&ARRAY(*cl, Tagged)[pos])


void apl_extend_array(Array *arr, unsigned int num);
void apl_init_array(Array *arr, unsigned int elemsize);

void apl_add_functor(Index i, const char *name, unsigned int arity);
Index apl_search_functor(const char *name, unsigned int arity);

void apl_init_builtins(void);
void apl_read_program(FILE *fp);

void apl_trail(Regs *regs, Index var);
void apl_undo_assignments(Regs *regs);
Bool apl_unify(Regs *regs, Index i1, Index i2);
Index apl_copy_clause(Clause *cl);
void apl_clause_unref(Clause *cl);

Index apl_calc_expr(Index i, double *fp, int *ip, Bool *isintp);

static inline Index apl_deref(Index i)
{
    Tagged t;
    t = TAGGED(i);
    while(ISREF(t) && INDEX(t) != 0) {
	i = AT(i,t);
	t = TAGGED(i);
    }
    return i;
}

static inline Index apl_get_functor(Index i)
{
    Tagged t;
    i = apl_deref(i);
    t = TAGGED(i);

    if(ISSTR(t))
        return STACK(AT(i, t), Index);
    else if(ISATM(t))
        return ATMFUNCT(t);
    else
        return -1;
}

static inline unsigned int apl_alloc_array(Array *arr, unsigned int num)
{
    unsigned int prevend;
    
    prevend = arr->end;
    if(arr->end + num > arr->size)
        apl_extend_array(arr, num);
    arr->end += num;
    
    return prevend;
}

static inline unsigned int apl_alloc_array_even(Array *arr, unsigned int num)
{
    unsigned int end;

    /* NOTE: some architectures (Sparc) seem to require an 8-byte
       aligned address for double */

    if((arr->end & 1) == 0)
        end = apl_alloc_array(arr, num);
    else
        end = apl_alloc_array(arr, num+1) + 1;

    return end;
}

static inline void apl_del_array(Array *arr)
{
    free(arr->array);
}

static inline Index apl_functor_name(Index funct)
{
    Index i;

    i = FUNCTOR(funct).namefunct; 
    if(i == -1) {
        i = apl_search_functor(FUNCTOR(funct).name, 0);
        FUNCTOR(funct).namefunct = i;
    }
    
    return i;
}

static inline Tagged apl_make_struct(Index pos, unsigned int arity)
{
    Tagged t;
    Index strpos;

    strpos = apl_alloc_array(&stack, 1 + arity);
    SETSTR(t);
    INDEX(t) = strpos-pos;
    TAGGED(pos) = t;
    
    return t;
}

static inline Tagged apl_make_float(Index pos, double fval)
{
    Tagged t;
    Index dblpos;
    
    dblpos = apl_alloc_array_even(&stack, 2);
    SETFLT(t);
    INDEX(t) = dblpos-pos;
    TAGGED(pos) = t;
    STACK(dblpos, double) = fval;

    return t;
}

#endif

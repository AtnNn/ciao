#include "aprolog.h"

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    PRG_EOF = -1,
    PRG_VAR,
    PRG_INT,
    PRG_FLT,
    PRG_FUN,
    PRG_CLS
} linetype;

static void parse_error(void)
{
    fprintf(stderr, "Parse error in input file\n");
    exit(1);
}

static Index read_functor(FILE *fp)
{
    unsigned int arity;
    unsigned int len;
    char *name;
    Index funct;
    
    fscanf(fp, "%u", &arity);
    
    fscanf(fp, "%u", &len);
    name = malloc(len + 1);
    assert(name != 0);
    
    getc(fp);
    fread(name, len, 1, fp);
    name[len] = '\0';
    
    funct = apl_search_functor(name, arity);

    free(name);

    return funct;
}

static linetype read_type(FILE *fp)
{
    linetype type = 0;
    char typename[16];
    int res;

    res = fscanf(fp, "%s", typename);
    if(res != 1) {
        if(res == EOF)
            return PRG_EOF;
        parse_error();
    }
    
    if(strcmp(typename, "VAR") == 0)
        type = PRG_VAR;
    else if(strcmp(typename, "INT") == 0)
        type = PRG_INT;
    else if(strcmp(typename, "FLT") == 0)
        type = PRG_FLT;
    else if(strcmp(typename, "FUN") == 0)
        type = PRG_FUN;
    else if(strcmp(typename, "CLS") == 0)
        type = PRG_CLS;
    else
        parse_error();

    return type;
}

static Tagged read_tagged(FILE *fp, double *f)
{
    linetype type;
    Tagged t;
    Index funct;
    int i;
    
    type = read_type(fp);
    
    switch(type) {
    case PRG_VAR:
        fscanf(fp, "%u", &i);
        SETVAR(t);
        INDEX(t) = i;
        break;
        
    case PRG_INT:
        fscanf(fp, "%i", &i);
        SETINT(t);
        INT(t) = i;
        break;

    case PRG_FLT:
        if(f != 0)
            fscanf(fp, "%lf", f);
        SETFLT(t);
        break;
        
    case PRG_FUN:
        funct = read_functor(fp);
        if(FUNCTOR(funct).arity == 0)
            SETATM(t);
        else
            SETSTR(t);
        
        INDEX(t) = funct;
        break;

    default:
        parse_error();
    }

    return t;
}

static void read_term(FILE *fp, Array *cla, Index *vars, Index pos) 
{
    Tagged p;
    Tagged t;
    double f;

    p = read_tagged(fp, &f);
    if(ISVAR(p)) {
        if(vars[INDEX(p)] == 0)
            vars[INDEX(p)] = pos;
        
        SETREF(t);
        INDEX(t) = vars[INDEX(p)] - pos;
    }
    else if(ISFLT(p)) {
        Index fltpos;

        fltpos = apl_alloc_array_even(cla, 2);
        SETFLT(t);
        INDEX(t) = fltpos - pos;

        CLAUSE(cla, fltpos, double) = f;
    }
    else if(ISSTR(p)) {
        unsigned int arity;
        unsigned int i;
        Index strpos;

        arity = FUNCTOR(INDEX(p)).arity;
        strpos = apl_alloc_array(cla, 1 + arity);
        
        SETSTR(t);
        INDEX(t) = strpos - pos;

        CLAUSE(cla, strpos, Index) = INDEX(p);
        for(i = 1; i <= arity; i++)
            read_term(fp, cla, vars, strpos+i);
    }
    else 
        t = p;

    CLAUSE(cla, pos, Tagged) = t;
}

static void read_clause(FILE *fp, Clause *cl)
{
    Tagged t;
    unsigned int numvars;
    unsigned int i;
    Array *cla = &cl->clause;
    Index *vars;
    Index pos;

    t = read_tagged(fp, 0);
    assert(ISSTR(t) && INDEX(t) == FUNCT_CL);
    t = read_tagged(fp, 0);
    assert(ISINT(t));
    numvars = INT(t);

    vars = (Index *) malloc(sizeof(Index) * numvars + 1);
    assert(vars != 0);
    for(i = 0; i < numvars; i++)
	vars[i] = 0;

    pos = apl_alloc_array(cla, 1);
    read_term(fp, cla, vars, pos);

    cl->headsize = cla->end;
    cl->firstgoal = 0;
    cl->lastgoal = 0;
    
    t = read_tagged(fp, 0);
    while(!ISATM(t)) {
        assert(ISSTR(t) && INDEX(t) == FUNCT_DOT);

        pos = apl_alloc_array(cla, 2);
        if(cl->lastgoal == 0)
            cl->firstgoal = pos;
        else
            CLAUSE(cla, cl->lastgoal, Index) = pos - cl->lastgoal;
                
        cl->lastgoal = pos;
        
        read_term(fp, cla, vars, pos + 1); 
        t = read_tagged(fp, 0);
    }
    assert(INDEX(t) == FUNCT_NIL);
}

static Clause *read_clause_list(FILE *fp, Index funct)
{
    Tagged t;
    Clause *cl;
    Clause clauses;
    cl = &clauses;

    do {
        t = read_tagged(fp, 0);
        
        if(!ISATM(t)) {
            assert(ISSTR(t) && INDEX(t) == FUNCT_DOT);
            
            cl->next = malloc(sizeof(Clause));
            assert(cl->next != 0);

            cl = cl->next; 
            cl->funct = funct;
            cl->refctr = 0;
            
            apl_init_array(&cl->clause, sizeof(Tagged));
            read_clause(fp, cl); 
        }
    } while(!ISATM(t));
    assert(INDEX(t) == FUNCT_NIL); 

    cl->next = 0;

    return clauses.next; 
} 

void apl_read_program(FILE *fp) 
{ 
    linetype type; 
    Index funct; 
    Clause *cl; 

    do {
        type = read_type(fp); 
        if(type != PRG_EOF) {
            const char *name;

            if(type != PRG_CLS) 
                parse_error(); 
            
            funct = read_functor(fp); 
            
            if(FUNCTOR(funct).type != PRED_UNDEF) {
                fprintf(stderr, "Predicate %s/%i already defined\n", 
                        FUNCTOR(funct).name, FUNCTOR(funct).arity);
                exit(1);
            }
            
            cl = read_clause_list(fp, funct); 
            
            FUNCTOR(funct).type = PRED_STATIC; 
            FUNCTOR(funct).pred.clauses = cl; 

            /* FIXME: should be implemented properly with a directive */
            name = FUNCTOR(funct).name;
            if(strchr(name, '$') != 0 || 
               (isalpha((int) name[0]) && strchr(name, ':') != NULL)) 
                FUNCTOR(funct).access = ACCESS_HIDDEN;
            else
                FUNCTOR(funct).access = ACCESS_PROLOG;
        }
    } while(type != PRG_EOF);
} 

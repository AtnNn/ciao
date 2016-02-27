#include "aprolog.h"

#include <string.h>
#include <assert.h>
#include <signal.h>

/** Command-line flags. */
int if_debug, if_profile, if_remove;

FILE *debfp;
static volatile int interrupt_flag;


#ifdef DEBUG
void write_term(Index term)
{
    Index pos = apl_deref(term);
    Tagged t = TAGGED(pos);
    
    if(ISREF(t))
        fprintf(debfp, "_%i", pos + INDEX(t));
    else if(ISATM(t))
        fprintf(debfp, "%s", FUNCTOR(ATMFUNCT(t)).name);
    else if(ISINT(t))
        fprintf(debfp, "%i", INT(t));
    else if(ISFLT(t))
        fprintf(debfp, "%g", STACK(pos+INDEX(t), double));
    else if(ISSTR(t)) {
        Index funct = STACK(pos+INDEX(t), Index);
        unsigned int i;

        fprintf(debfp, "%s(", FUNCTOR(funct).name);
        for(i = 1; i <= FUNCTOR(funct).arity; i++) {
            if(i != 1) 
                fprintf(debfp, ",");
            write_term(pos+INDEX(t)+i);
        }
        fprintf(debfp, ")");
    }
}


static void print_current_goal(Regs *regs)
{
    Index goal;
    goal = apl_deref(regs->goals + 1);

    fprintf(debfp, "Current goal is: ");
    write_term(goal);
    fprintf(debfp, "\n");
    fflush(debfp);
}

static void inc_callctr(Index goal)
{
    Index funct;

    funct = apl_get_functor(goal);
    FUNCTOR(funct).callctr++;
}

static void write_profile()
{
    Index i;
    FILE *fp;

    fp = fopen("aprolog.prof", "w");
    if(fp == NULL) {
        perror("aprolog.prof");
        return;
    }
    
    for(i = 0; i < functors.end; i++) {
        if(FUNCTOR(i).callctr > 0)
            fprintf(fp, "%8i\t%s/%i\n", FUNCTOR(i).callctr,
                    FUNCTOR(i).name, FUNCTOR(i).arity);
    }

}
#endif

Array functors;
Array stack;
Array trail;

static void interrupt_handler(int sig)
{
    (void)sig;
    interrupt_flag = 1;
}


void apl_init_array(Array *arr, unsigned int elemsize)
{
    arr->array = 0;
    arr->size = 0;
    arr->end = 0;
    arr->elemsize = elemsize;
}


void apl_extend_array(Array *arr, unsigned int num)
{
    arr->size += (num + 256) & ~0xFF;
    arr->array = (unsigned int *)
        realloc(arr->array, arr->size * arr->elemsize);
    assert(arr->array != 0);
}

void apl_add_functor(Index i, const char *name, unsigned int arity)
{
    FUNCTOR(i).arity = arity;
    FUNCTOR(i).name = strdup(name);
    FUNCTOR(i).namefunct = (arity == 0) ? i : -1;
    FUNCTOR(i).type = PRED_UNDEF;
    SETREF(FUNCTOR(i).blackboard);
    PROF(FUNCTOR(i).callctr = 0);
}

static void init_predefs(void)
{
    apl_alloc_array(&functors, FUNCT_LASTDEF);
    
    apl_add_functor(FUNCT_NIL,          "[]",                     0);
    apl_add_functor(FUNCT_TRUE,         "true",                   0);
    apl_add_functor(FUNCT_FAIL,         "fail",                   0);
    apl_add_functor(FUNCT_FALSE,        "false",                  0);
    apl_add_functor(FUNCT_DOT,          ".",                      2);
    apl_add_functor(FUNCT_COMMA,        ",",                      2);
    apl_add_functor(FUNCT_CALL_DYNAMIC, "execution:call_dynamic", 1);
    apl_add_functor(FUNCT_DEBUG,        "debug:debug",            1);
    apl_add_functor(FUNCT_CALL_ERROR,   "execution:call_error",   1);
    apl_add_functor(FUNCT_INTERRUPT,    "toplevel:interrupt",     0);
    apl_add_functor(FUNCT_MAIN,         "toplevel:main",          0);
    apl_add_functor(FUNCT_CL,           "clause",                 3);
    apl_add_functor(FUNCT_READ,         "read",                   0);
    apl_add_functor(FUNCT_WRITE,        "write",                  0);
    apl_add_functor(FUNCT_APPEND,       "append",                 0);

    apl_add_functor(FUNCT_EEXIST,       "eexist",                 0);
    apl_add_functor(FUNCT_EPERM,        "eperm",                  0);
    apl_add_functor(FUNCT_ESYSTEM,      "esystem",                0);

    apl_add_functor(FUNCT_A_NEG,        "-",                      1);
    apl_add_functor(FUNCT_A_NOT,        "\\",                     1);
    apl_add_functor(FUNCT_A_ABS,        "abs",                    1);
    apl_add_functor(FUNCT_A_SIGN,       "sign",                   1);
    apl_add_functor(FUNCT_A_INT,        "float_integer_part",     1);
    apl_add_functor(FUNCT_A_FRAC,       "float_fractional_part",  1);
    apl_add_functor(FUNCT_A_ROUND,      "round",                  1);
    apl_add_functor(FUNCT_A_TRUNCATE,   "truncate",               1);
    apl_add_functor(FUNCT_A_FLOOR,      "floor",                  1);
    apl_add_functor(FUNCT_A_CEILING,    "ceiling",                1);
    apl_add_functor(FUNCT_A_SIN,        "sin",                    1); 
    apl_add_functor(FUNCT_A_COS,        "cos",                    1); 
    apl_add_functor(FUNCT_A_ATAN,       "atan",                   1); 
    apl_add_functor(FUNCT_A_SQRT,       "sqrt",                   1); 
    apl_add_functor(FUNCT_A_LOG,        "log",                    1); 
    apl_add_functor(FUNCT_A_EXP,        "exp",                    1); 
    apl_add_functor(FUNCT_A_ADD,        "+",                      2);
    apl_add_functor(FUNCT_A_SUB,        "-",                      2);
    apl_add_functor(FUNCT_A_MUL,        "*",                      2);
    apl_add_functor(FUNCT_A_IDIV,       "//",                     2);
    apl_add_functor(FUNCT_A_REM,        "rem",                    2);
    apl_add_functor(FUNCT_A_MOD,        "mod",                    2);
    apl_add_functor(FUNCT_A_AND,        "/\\",                    2);
    apl_add_functor(FUNCT_A_OR,         "\\/",                    2);
    apl_add_functor(FUNCT_A_SHL,        "<<",                     2);
    apl_add_functor(FUNCT_A_SHR,        ">>",                     2);
    apl_add_functor(FUNCT_A_DIV,        "/",                      2);
    apl_add_functor(FUNCT_A_POW,        "**",                     2);

    apl_add_functor(FUNCT_ACCESS_USER,   "access_user",            0);
    apl_add_functor(FUNCT_ACCESS_PROLOG, "access_prolog",          0);
    apl_add_functor(FUNCT_ACCESS_HIDDEN, "access_hidden",          0);

    apl_add_functor(FUNCT_PRED_STATIC,   "pred_static",            0);
    apl_add_functor(FUNCT_PRED_DYNAMIC,  "pred_dynamic",           0);
    apl_add_functor(FUNCT_PRED_BUILTIN,  "pred_builtin",           0);
    apl_add_functor(FUNCT_PRED_GLOBVAR,  "pred_globvar",           0);
    
    apl_add_functor(FUNCT_ERROR,         "$error",                 0);
    apl_add_functor(FUNCT_CURR_INPUT,    "$current_input",         0);
    apl_add_functor(FUNCT_CURR_OUTPUT,   "$current_output",        0);
    apl_add_functor(FUNCT_CURR_ERROR,    "$current_error",         0);

    /**** pts ****/
    apl_add_functor(FUNCT_VERSION,       "$version",               0);
    apl_add_functor(FUNCT_VERSION_VALUE, "aprolog " APROLOG_VERSION, 0);
    
    apl_add_functor(FUNCT_TYPE_ERROR,    "type_error",             0);
    apl_add_functor(FUNCT_ZERO_DIVISOR,  "zero_divisor",           0);
    apl_add_functor(FUNCT_FLOAT_OVERFLOW,"float_overflow",         0);
    apl_add_functor(FUNCT_INT_OVERFLOW,  "int_overflow",           0);
    apl_add_functor(FUNCT_UNDEFINED,     "undefined",              0);
}

Index apl_search_functor(const char *name, unsigned int arity)
{
    unsigned i;

    for(i = 0; i < functors.end; i++)
        if(FUNCTOR(i).arity == arity && strcmp(FUNCTOR(i).name, name) == 0)
            return i;
    
    i = apl_alloc_array(&functors, 1);
    apl_add_functor(i, name, arity);

    return i;
}

void apl_trail(Regs *regs, Index var)
{
    Index assigi;

    DEB(fprintf(debfp, "assinged _%i\n", var));

    assigi = apl_alloc_array(&trail, 1);
    SETREF(TRAIL(assigi));
    INDEX(TRAIL(assigi)) = var;

    STACK(regs->lastadm, Adm).numassig++;
}

static void assign(Regs *regs, Index var, Index value)
{
    Tagged t;

    apl_trail(regs, var);

    t = TAGGED(value);
    if(ISREF(t) || ISSTR(t) || ISFLT(t))
	INDEX(t) = AT(value,t) - var;

    TAGGED(var) = t;
}


static Bool unify_args(Regs *regs, Index i1, Index i2)
{
    Index funct;
    unsigned int arity;
    unsigned int a;
    
    i1 = ATI(i1);
    i2 = ATI(i2);
    funct = STACK(i1, Index);
    if(funct != STACK(i2, Index))
        return FALSE;
    
    arity = FUNCTOR(funct).arity;
    for(a = 1; a <= arity; a++) {
        if(!apl_unify(regs, i1+a, i2+a))
            return FALSE;
    }
    return TRUE;

}

Bool apl_unify(Regs *regs, Index i1, Index i2)
{
    Tagged t1, t2;
    
    i1 = apl_deref(i1);
    i2 = apl_deref(i2);
    t1 = TAGGED(i1);
    t2 = TAGGED(i2);

    if(ISREF(t1)) {
        assign(regs, i1, i2);
        return TRUE;
    }
    if(ISREF(t2)) {
        assign(regs, i2, i1);
        return TRUE;
    }
 
    if(ISFLT(t1) && ISFLT(t2)) {
        i1 = AT(i1, t1);
        i2 = AT(i2, t2);
        if(STACK(i1, double) == STACK(i2, double))
            return TRUE;
        else
            return FALSE;
    }

    if(ISSTR(t1) && ISSTR(t2))
        return unify_args(regs, i1, i2);

    if(STACK(i1, Index) == STACK(i2, Index))
        return TRUE;

    return FALSE;
}

static void create_frame(Regs *regs, Clause *clauses)
{
    Index adm;
 
    adm = apl_alloc_array(&stack, sizeof(Adm) / sizeof(Tagged));

    STACK(adm, Adm).previous = regs->lastadm;
    STACK(adm, Adm).prevchoice = regs->lastadm;
    STACK(adm, Adm).goals = regs->goals;
    STACK(adm, Adm).clauses = clauses;
    STACK(adm, Adm).assigs  = trail.end;
    STACK(adm, Adm).numassig = 0;
    STACK(adm, Adm).clauselist = 0;

    regs->lastadm = adm;
}

Index apl_copy_clause(Clause *cl)
{
    Index headpos;

    headpos = apl_alloc_array_even(&stack, cl->clause.end);
    memcpy(&TAGGED(headpos), cl->clause.array, 
           cl->clause.end * sizeof(Tagged));

    return headpos;
}

static void replace_goal(Regs *regs, Index goal, Index arg)
{
    Index pos;
    Tagged str;
    Index strpos;

    pos = apl_alloc_array(&stack, 2);
    SETNEXTGOAL(pos, NEXTGOAL(regs->goals));

    regs->goals = pos;

    str = apl_make_struct(pos+1, 1);
    strpos = AT(pos+1, str);
    STACK(strpos, Index) = goal;

    SETREF(TAGGED(strpos+1));
    INDEX(TAGGED(strpos+1)) = arg - (strpos+1);
}

static void insert_goal(Regs *regs, Index goal)
{
    Index pos;

    pos = apl_alloc_array(&stack, 2);
    SETNEXTGOAL(pos, regs->goals);
    regs->goals = pos;

    SETATM(TAGGED(pos+1));
    ATMFUNCT(TAGGED(pos+1)) = goal;
}

static Bool apl_reduce(Regs *regs)
{
    Index headpos;
    Index goal;
    Clause *cl;
    Bool success;

    goal = apl_deref(regs->goals + 1);
    PROF(inc_callctr(goal));
    cl = STACK(regs->lastadm, Adm).clauses;
    headpos = apl_copy_clause(cl);
    if(cl->lastgoal != 0) {
        SETNEXTGOAL(headpos+cl->lastgoal, NEXTGOAL(regs->goals));
        regs->goals = headpos+cl->firstgoal;
    }
    else
        regs->goals = NEXTGOAL(regs->goals);
    
    if(ISATM(TAGGED(goal)))
        success = TRUE;
    else
        success = unify_args(regs, headpos, goal);
        
    STACK(regs->lastadm, Adm).clauses = 
        STACK(regs->lastadm, Adm).clauses->next;
    
    return success;
}

void apl_clause_unref(Clause *cl)
{
    cl->refctr --;
    if(cl->refctr == 0 && cl->funct == -1) {
        apl_del_array(&cl->clause);
        free(cl);
    }
}

static void unref_clauses(Regs *regs)
{
    Index clpos = STACK(regs->lastadm, Adm).clauselist;
    while(STACK(clpos, Clause *) != 0) {
        apl_clause_unref(STACK(clpos, Clause *));
        clpos ++;
    }
}

void apl_undo_assignments(Regs *regs)
{
    unsigned int numassig;
    Index i;

    numassig = STACK(regs->lastadm, Adm).numassig;
    i = STACK(regs->lastadm, Adm).assigs;

    for(; numassig > 0; i++, numassig--) {
        if(ISREF(TRAIL(i))) {
            Index var;
            var = INDEX(TRAIL(i));
        
            SETREF(TAGGED(var));
            INDEX(TAGGED(var)) = 0;
            DEB(fprintf(debfp, "unassinged _%i\n", var));
        }
        else { /* if(ISATM(TAGGED(i))) */
            Index funct;
            Index prevval;
            
            funct = ATMFUNCT(TRAIL(i));
            i++;
            prevval = INDEX(TRAIL(i));
            if(prevval != 0) 
                FUNCTOR(funct).pred.globvar = prevval;
            else
                FUNCTOR(funct).type = PRED_UNDEF;
            DEB(fprintf(debfp, "unassigned %s\n", FUNCTOR(funct).name));
        }
    }

    STACK(regs->lastadm, Adm).numassig = 0;
}

static Bool backtrack(Regs *regs)
{
    Clause *clauses;

    do {
        DEB(fprintf(debfp, "backtrack\n"));

        apl_undo_assignments(regs);

        clauses = STACK(regs->lastadm, Adm).clauses;
        if(clauses == 0) {
            DEB(fprintf(debfp, "FAIL\n"));
            
            if(STACK(regs->lastadm, Adm).clauselist != 0)
                unref_clauses(regs);

            regs->lastadm = STACK(regs->lastadm, Adm).previous;
            if(regs->lastadm == 0)
                return FALSE;
        }
    } while(clauses == 0);

    trail.end = STACK(regs->lastadm, Adm).assigs;
    stack.end = regs->lastadm + sizeof(Adm) / sizeof(Tagged);
    regs->goals = STACK(regs->lastadm, Adm).goals;
        
    return TRUE;
}


static Bool apl_execute(Regs *regs)
{
    Index goali;
    Tagged goal;
    Bool success;
    Index funct;
    Index args;

    goali = apl_deref(regs->goals + 1);
    goal = TAGGED(goali);
    
    if(!ISATM(goal) && !ISSTR(goal)) {
        replace_goal(regs, FUNCT_CALL_ERROR, goali);
        return TRUE;
    }
    
    if(ISSTR(goal)) {
        funct = STACK(goali+INDEX(goal), Index);
        args = goali+INDEX(goal) + 1;
    }
    else {
        funct = ATMFUNCT(goal);
        args = 0;
    }
    
    if(FUNCTOR(funct).type == PRED_UNDEF) {
        replace_goal(regs, FUNCT_CALL_ERROR, goali);
        return TRUE;
    }

    if(regs->trace >= 0) {
        if(regs->trace == 0)
            regs->trace = 1;
        else {
            if(FUNCTOR(funct).access == ACCESS_USER ||
               FUNCTOR(funct).access == ACCESS_PROLOG) {
                replace_goal(regs, FUNCT_DEBUG, goali);
                return TRUE;
            }
        }
    }

    DEB(print_current_goal(regs));

    if(FUNCTOR(funct).pred.clauses == 0)
        return FALSE;
    
    if(FUNCTOR(funct).type == PRED_BUILTIN) {
        PROF(inc_callctr(goali));
        create_frame(regs, 0);
        success = (*FUNCTOR(funct).pred.builtin) (regs, funct, args);
        if(!regs->again)
            regs->goals = NEXTGOAL(regs->goals);
        else
            regs->again = FALSE;
    }
    else if(FUNCTOR(funct).type == PRED_DYNAMIC) {
        PROF(inc_callctr(goali));
        create_frame(regs, 0);
        replace_goal(regs, FUNCT_CALL_DYNAMIC, goali);
        success = TRUE;
    }
    else {
        create_frame(regs, FUNCTOR(funct).pred.clauses);
        success = apl_reduce(regs);
    }

    DEB(fprintf(debfp, "%s\n", success ? "TRUE" : "FALSE"));

    return success;
}

static void main_loop(Regs *regs)
{
    Bool success;
    
    while(regs->goals != 0) {
        if(interrupt_flag) {
            interrupt_flag = 0;
            insert_goal(regs, FUNCT_INTERRUPT);
        }
        success = apl_execute(regs);
        
        while(!success) {
            if(!backtrack(regs))
                return;

            DEB(print_current_goal(regs));
            success = apl_reduce(regs);
            DEB(fprintf(debfp, "%s\n", success ? "TRUE" : "FALSE"));
        }
    }
}


static void init_program(Regs *regs)
{
    Index goal;
    Index goallist;
    Tagged nilt;

    SETATM(nilt);
    INDEX(nilt) = FUNCT_NIL;

    /* allocate 0 index for stack */
    apl_alloc_array(&stack, 1);

    goallist = apl_alloc_array(&stack, 1);
    STACK(goallist, Index) = 0 - goallist;
    
    goal = apl_alloc_array(&stack, 1);
    SETATM(STACK(goal, Tagged));
    ATMFUNCT(STACK(goal, Tagged)) = FUNCT_MAIN;
    
    regs->goals = goallist;
    regs->lastadm = 0;
    regs->again = FALSE;
    regs->trace = -1;
}


static char *progname;

static void usage(void)
{
    fprintf(stderr, "This is aprolog, a mini Prolog implementation, v" APROLOG_VERSION "\n"
                    "The license is GNU GPL >=2.0. It comes without warranty. USE AT YOUR OWN RISK!\n"
                    "Usage: %s [<option> ...] [--] <filename.apl> [...]\n"
                    "Run aprolog-run for a more user friendly command line interface.\n"
                    "Options:\n"
                    "-d  turn debugging on\n"
                    "-p  turn profiling on\n"
                    "-r  remove .apl files (except for 1st) after reading them\n"
                    "--help  get this help\n"
                  , progname);
    exit(1);
}


int main(int argc, char *argv[])
{
    FILE *fp;
    Regs regs;
    char **ap, **apfirst; /**** pts ****/
    (void)argc;
    
    progname = argv[0];

    apl_init_array(&functors, sizeof(Functor));
    apl_init_array(&stack, sizeof(Tagged));
    apl_init_array(&trail, sizeof(Tagged));

    init_predefs();
    apl_init_builtins();
    init_program(&regs);

    /**** pts ****/
    ap=argv+1;
    while (*ap!=NULL) {
      if (ap[0][0]!='-') break;
      if (ap[0][1]=='-' && ap[0][2]=='\0') { ap++; break; }
      else if (ap[0][1]=='d' && ap[0][2]=='\0') if_debug++;
      else if (ap[0][1]=='p' && ap[0][2]=='\0') if_profile++;
      else if (ap[0][1]=='r' && ap[0][2]=='\0') if_remove++;
      else if (0==strcmp(*ap, "--help")) usage();
      else { fprintf(stderr, "%s: unknown option: %s\n", progname, *ap); usage(); }
      ap++;
    }
    if (*ap==NULL) { fprintf(stderr, "%s: missing <filename.apl>\n", progname); usage(); }

    apfirst=ap;
    while (*ap!=NULL) {
      if (NULL==(fp=fopen(*ap++, "r"))) {
        fprintf(stderr, "%s: error opening .apl file: ", progname);
        perror(ap[-1]);
        exit(1);
      }
      
      apl_read_program(fp);
      fclose(fp);
      if (if_remove && ap!=apfirst+1 && 0!=remove(ap[-1])) {
        fprintf(stderr, "%s: error removing .apl file: ", progname);
        perror(ap[-1]);
      }
    }
    
    PROF(atexit(write_profile));
    DEB(debfp = fopen("/tmp/aprolog.deb", "w"); if(fp == NULL) fp = stderr);

    interrupt_flag = 0;

    {
        struct sigaction act;

        act.sa_handler = interrupt_handler;
        sigemptyset(&act.sa_mask);
        act.sa_flags = 0;

        sigaction(SIGINT,  &act, NULL);
    }


    main_loop(&regs);
   
    return 0;
}

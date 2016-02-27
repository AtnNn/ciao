#include "aprolog.h"

#include <assert.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/resource.h>

static void bb_clear(Index key)
{
    SETREF(FUNCTOR(key).blackboard);
}

static void bb_set_atom(Index key, Index atom)
{
    SETATM(FUNCTOR(key).blackboard);
    INDEX(FUNCTOR(key).blackboard) = atom;
}

static void bb_set_int(Index key, int val)
{
    SETINT(FUNCTOR(key).blackboard);
    INT(FUNCTOR(key).blackboard) = val;
}

static int bb_get_int(Index key)
{
    if(ISINT(FUNCTOR(key).blackboard))
        return INT(FUNCTOR(key).blackboard);
    return 0;
}


static Bool check_int_arg(Index arg, int *valp)
{
    Tagged t;

    t = TAGGED(apl_deref(arg));
    if(!ISINT(t))
        return FALSE;

    *valp = INT(t);

    return TRUE;
}

static Bool check_atm_arg(Index arg, Index *functp)
{
    Tagged t;

    t = TAGGED(apl_deref(arg));
    if(!ISATM(t))
        return FALSE;

    *functp = ATMFUNCT(t);

    return TRUE;
}


static Bool get_num_args(Index args, double *f1p, double *f2p,
                         int *i1p, int *i2p, Bool *isintp)
{
    Tagged t1, t2;
    Index i1, i2;

    i1 = apl_deref(args);
    i2 = apl_deref(args+1);
    t1 = TAGGED(i1);
    t2 = TAGGED(i2);

    if(ISINT(t1) && ISINT(t2)) {
        *i1p = INT(t1);
        *i2p = INT(t2);
        *isintp = TRUE;
        return TRUE;
    }

    if(ISINT(t1))
        *f1p = (double) INT(t1);
    else if(ISFLT(t1))
        *f1p = STACK(AT(i1,t1), double);
    else
        return FALSE;
    
    if(ISINT(t2))
        *f2p = (double) INT(t2);
    else if(ISFLT(t2))
        *f2p = STACK(AT(i2,t2), double);
    else
        return FALSE;
    
    *isintp = FALSE;
    return TRUE;
}

static Bool set_atomic_arg(Regs *regs, Index arg, Tagged t)
{
    Index pos;

    pos = apl_alloc_array(&stack, 1);
    TAGGED(pos) = t;

    return apl_unify(regs, pos, arg);
}

static Bool set_int_arg(Regs *regs, Index arg, int ival)
{
    Tagged t;

    SETINT(t);
    INT(t) = ival;

    return set_atomic_arg(regs, arg, t);
}

static Bool set_atm_arg(Regs *regs, Index arg, Index funct)
{
    Tagged t;

    SETATM(t);
    INDEX(t) = funct;
    return set_atomic_arg(regs, arg, t);
}

static Bool set_float_arg(Regs *regs, Index arg, double fval)
{
    Tagged t;
    Index pos;

    pos = apl_alloc_array(&stack, 1);
    t = apl_make_float(pos, fval);

    return apl_unify(regs, pos, arg);
}

static Bool bip_true(Regs *regs, Index functor, Index args)
{
    (void)functor;
    (void)regs;
    (void)args;
    return TRUE;
}

static Bool bip_fail(Regs *regs, Index functor, Index args)
{
    (void)functor;
    (void)regs;
    (void)args;
    return FALSE;
}

static Bool bip_halt(Regs *regs, Index functor, Index args)
{
    int exitcode;
    (void)functor;
    (void)regs;

    if(!check_int_arg(args, &exitcode))
        return FALSE;
    
    exit(exitcode);

    return TRUE;
}

static Bool bip_expr(Regs *regs, Index functor, Index args)
{
    double fv;
    int iv;
    Bool isint;
    Index res;
    (void)functor;
    
    res = apl_calc_expr(args, &fv, &iv, &isint);
    if(res != FUNCT_NIL) {
        bb_set_atom(FUNCT_ERROR, res);
        return FALSE;
    }
    
    if(isint)
        return set_int_arg(regs, args+1, iv);
    else
        return set_float_arg(regs, args+1, fv);
}


static Bool bip_cmp(Regs *regs, Index functor, Index args)
{
    double a1, a2;
    int i1, i2;
    Bool isint;
    (void)regs;

    if(!get_num_args(args, &a1, &a2, &i1, &i2, &isint))
        return FALSE;

    if(isint) 
        switch(functor) {
        case FUNCT_LESS:   return (i1 < i2);
        case FUNCT_LEQ:    return (i1 <= i2);
        case FUNCT_EQ:     return (i1 == i2);
        case FUNCT_NEQ:    return (i1 != i2);
        }
    else
        switch(functor) {
        case FUNCT_LESS:   return (a1 < a2);
        case FUNCT_LEQ:    return (a1 <= a2);
        case FUNCT_EQ:     return (a1 == a2);
        case FUNCT_NEQ:    return (a1 != a2);
        }
    return FALSE;
}

static Bool bip_type(Regs *regs, Index functor, Index args)
{
    Tagged t;
    (void)regs;

    t = TAGGED(apl_deref(args));
    switch(functor) {
    case FUNCT_INTEGER:  return ISINT(t);
    case FUNCT_FLOAT:    return ISFLT(t);
    case FUNCT_NUMBER:   return (ISINT(t) || ISFLT(t));
    case FUNCT_ATOM:     return ISATM(t);
    case FUNCT_ATOMIC:   return (ISINT(t) || ISFLT(t) || ISATM(t));
    case FUNCT_COMPOUND: return ISSTR(t);
    case FUNCT_CALLABLE: return (ISATM(t) || ISSTR(t));
    case FUNCT_NONVAR:   return !ISREF(t);
    case FUNCT_VAR:      return ISREF(t);
    }
    return FALSE;
}

static Bool list_to_string(Index pos, Array *string)
{
    Tagged t;
    unsigned int at;
    int cval;
    
    pos = apl_deref(pos);
    t = TAGGED(pos);
    while(!ISATM(t)) {
        if(!ISSTR(t) || STACK(AT(pos,t), Index) != FUNCT_DOT || 
           !check_int_arg(AT(pos,t)+1, &cval) || cval <= 0 || cval > 255)
            return FALSE;
        
        pos = apl_deref(AT(pos,t)+2);
	t = TAGGED(pos);
        at = apl_alloc_array(string, 1);
        ARRAY(*string, char)[at] = cval;
    }
    if(ATMFUNCT(t) != FUNCT_NIL)
        return FALSE;
            
    at = apl_alloc_array(string, 1);    
    ARRAY(*string, char)[at] = '\0';
    
    return TRUE;
}

static Index string_to_list(Regs *regs, const char *s)
{
    Index listpos;
    Index pos;
    Tagged it, st;
    (void)regs;

    listpos = apl_alloc_array(&stack, 1);
    pos = listpos;
    for(; *s != 0; s++) {
        st =  apl_make_struct(pos, 2);
        STACK(AT(pos,st), Index) = FUNCT_DOT;
        SETINT(it);
        INT(it) =  *(unsigned char const*)s;
        TAGGED(AT(pos,st)+1) = it;
        pos = AT(pos,st)+2;
    }
    SETATM(st);
    ATMFUNCT(st) = FUNCT_NIL;
    TAGGED(pos) = st;

    return listpos;
}

static Bool bip_atom_codes(Regs *regs, Index functor, Index args)
{
    Tagged t;
    Bool success;
    (void)functor;

    t = TAGGED(apl_deref(args));
    if(ISREF(t)) {
        Array string;
	Index atmfunct;
        
        apl_init_array(&string, sizeof(char));
        success = list_to_string(args+1, &string);
        if(!success) {
            apl_del_array(&string);            
            return FALSE;
        }
        atmfunct = apl_search_functor(ARRAY(string, char), 0);
        apl_del_array(&string);
        success = set_atm_arg(regs, args, atmfunct);
    }
    else if(ISATM(t)) {
        Index list;

        list = string_to_list(regs, FUNCTOR(ATMFUNCT(t)).name);
        success = apl_unify(regs, args+1, list);
    }
    else
        return FALSE;

    return success;
}

static Bool name_to_num(char *name, Index pos)
{
    char *end;
    Tagged t;
    
    if(strchr(name, '.') == NULL) {
        SETINT(t);
        INT(t) = (int) strtol(name, &end, 10);
        if(*end != '\0')
            return FALSE;
    }
    else {
        double fval;

        fval = strtod(name, &end);
        if(*end != '\0')
            return FALSE;

	t = apl_make_float(pos, fval);
    }
    
    TAGGED(pos)  = t;
    return TRUE;
}

static Bool bip_number_codes(Regs *regs, Index functor, Index args)
{
    Tagged t;
    Index numpos;
    Bool success;
    (void)functor;

    numpos = apl_deref(args);
    t = TAGGED(numpos);
    if(ISREF(t)) {
        Array string;
	Index pos;

        apl_init_array(&string, sizeof(char));
        success = list_to_string(args+1, &string);
        if(!success) {
            apl_del_array(&string);
            return FALSE;
        }
	
	pos = apl_alloc_array(&stack, 1);
        success = name_to_num(ARRAY(string, char), pos);
        apl_del_array(&string);
        if(!success)
            return FALSE;

        success = apl_unify(regs, args, pos);
    }
    else if(ISINT(t) || ISFLT(t)) {
        Index list;
        char namebuf[64];
        
        if(ISINT(t)) 
            sprintf(namebuf, "%i", INT(t));
        else {
            double fval = STACK(AT(numpos,t), double);
            double absfval = fval < 0 ? -fval : fval;
            const char *format;
            int i;

            if((absfval > 1.0e6 || absfval < 1.0e-6) && absfval != 0.0)
                format = "%.*e";
            else
                format = "%.*f";

            for(i = 1; i < 20; i++) {
                sprintf(namebuf, format, i, fval);
                if(strtod(namebuf, NULL) == fval)
                    break;
            }
        }

        list = string_to_list(regs, namebuf);

        success = apl_unify(regs, args+1, list);
    }
    else
        return FALSE;

    return success;
}

static Index new_struct(Regs *regs, Index funct)
{
    Tagged str;
    Index pos;
    Tagged t;
    unsigned int i;
    unsigned int arity = FUNCTOR(funct).arity;
    (void)regs;

    pos = apl_alloc_array(&stack, 1);
    str = apl_make_struct(pos, arity);
    STACK(AT(pos,str), Index) = funct;

    SETREF(t);
    INDEX(t) = 0;
    for(i = 1; i <= arity; i++)
        TAGGED(AT(pos,str)+i) = t;
    
    return pos;
}

static Bool bip_set_functor(Regs *regs, Index functor, Index args)
{
    Index funct;
    int arity;
    Index pos;
    (void)functor;

    if(!check_atm_arg(args, &funct) || !check_int_arg(args+1, &arity) ||
       arity < 1 || arity > 255)
        return FALSE;

    pos = new_struct(regs, apl_search_functor(FUNCTOR(funct).name, arity));
    return apl_unify(regs, args+2, pos);
}

static Bool bip_get_functor(Regs *regs, Index functor, Index args)
{
    Index str;
    Tagged t;
    Index funct;
    Index atmfunct;
    (void)functor;

    str = apl_deref(args);
    t = TAGGED(str);
    if(!ISSTR(t))
        return FALSE;

    funct = STACK(AT(str, t), Index);
    atmfunct = apl_functor_name(funct);
    
    if(set_atm_arg(regs, args+1, atmfunct) &&
       set_int_arg(regs, args+2, FUNCTOR(funct).arity))
        return TRUE;

    return FALSE;
}

static Bool bip_arg(Regs *regs, Index functor, Index args)
{
    int argn;
    unsigned int arity;
    Tagged str;
    Index pos;
    Index funct;
    (void)functor;

    bb_clear(FUNCT_ERROR);   
    if(!check_int_arg(args, &argn) || argn < 0) {
        bb_set_atom(FUNCT_ERROR, FUNCT_TRUE);
        return FALSE;
    }

    pos = apl_deref(args+1);
    str = TAGGED(pos);
    if(!ISSTR(str)) {
        bb_set_atom(FUNCT_ERROR, FUNCT_TRUE);
        return FALSE;
    }

    funct = STACK(AT(pos,str), Index);
    arity = FUNCTOR(funct).arity;

    if(argn < 1 || argn+0U > arity) /**** pts ****/ /* Dat: avoid comparision of signed and unsigned */
        return FALSE;

    return apl_unify(regs, args+2, AT(pos,str)+argn);
}

#define PTR_TO_INT(ptr) (((unsigned long) ptr) / sizeof(void *))
#define INT_TO_PTR(idx) (((unsigned long) idx) * sizeof(void *))

static FILE *curr_input(void)
{
    return (FILE *) INT_TO_PTR(bb_get_int(FUNCT_CURR_INPUT));
}

static FILE *curr_output(void)
{
    return (FILE *) INT_TO_PTR(bb_get_int(FUNCT_CURR_OUTPUT));
}


static Bool check_interrupt(Regs *regs, FILE *fp)
{
    if(ferror(fp) && errno == EINTR) {
        clearerr(fp);
        regs->again = TRUE;
        return TRUE;
    }
    return FALSE;
}

static Bool bip_get_code(Regs *regs, Index functor, Index args)
{
    int c;
    FILE *fp = curr_input();
    (void)functor;

    c = getc(fp);
    if(check_interrupt(regs, fp))
        return TRUE;
    if(c == -1)
        return FALSE;

    return set_int_arg(regs, args, c);
}

static Bool bip_peek_code(Regs *regs, Index functor, Index args)
{
    int c;
    FILE *fp = curr_input();
    (void)functor;

    c = getc(fp);
    if(check_interrupt(regs, fp))
        return TRUE;
    if(c == -1)
        return FALSE;
    ungetc(c, fp);

    return set_int_arg(regs, args, c);    
}

static Bool bip_put_code(Regs *regs, Index functor, Index args)
{
    int c;
    FILE *fp = curr_output();
    (void)functor;

    if(!check_int_arg(args, &c))
        return FALSE;

    putc(c, fp);
    check_interrupt(regs, fp);

    return TRUE;
}

static Bool bip_open(Regs *regs, Index functor, Index args)
{
    FILE *fp;
    Index funct;
    Index mode;
    char modestr[16];
    char *m;
    (void)functor;

    if(!check_atm_arg(args, &funct) || !check_atm_arg(args+1, &mode))
        return FALSE;
    
    m = modestr;
    switch(mode) {
    case FUNCT_READ:   *m++ = 'r'; break;
    case FUNCT_WRITE:  *m++ = 'w'; break;
    case FUNCT_APPEND: *m++ = 'a'; break;
    default:
        return FALSE;
    }
    *m = '\0';

    fp = fopen(FUNCTOR(funct).name, modestr);
    if(fp == NULL) {
        Index err;

        switch(errno) {
        case ENOENT:
            err = FUNCT_EEXIST; 
            break;
        case EACCES:
        case EROFS:
        case EISDIR:
            err = FUNCT_EPERM;
            break;

        default:
            err = FUNCT_ESYSTEM;
        }
        return set_atm_arg(regs, args+2, err);
    }

    return set_int_arg(regs, args+2, PTR_TO_INT(fp));
}

static Bool bip_close(Regs *regs, Index functor, Index args)
{
    int stream_index;
    FILE *fp;
    int res;
    (void)functor;
    (void)regs;

    if(!check_int_arg(args, &stream_index))
        return FALSE;
    fp = (FILE *) INT_TO_PTR(stream_index);
    res = fclose(fp);
    if(res == 0)
        return TRUE;

    return FALSE;
}

static Bool bip_stream_type(Regs *regs, Index functor, Index args)
{
    int stream_index;
    FILE *fp;
    int res;
    Index can_seek = FUNCT_TRUE;
    Index is_tty = FUNCT_TRUE;
    int fd;
    (void)functor;

    if(!check_int_arg(args, &stream_index))
        return FALSE;
    fp = (FILE *) INT_TO_PTR(stream_index);
    fd = fileno(fp);

    res = lseek(fd, 0, SEEK_CUR);
    if(res == -1 && errno == ESPIPE)
        can_seek = FUNCT_FALSE;

    res = isatty(fd);
    if(res == 0) 
        is_tty = FUNCT_FALSE;
    else
        can_seek = FUNCT_FALSE;

    if(!set_atm_arg(regs, args+1, can_seek) ||
       !set_atm_arg(regs, args+2, is_tty))
        return FALSE;

    return TRUE;
}

static Bool bip_set_pos(Regs *regs, Index functor, Index args)
{
    int stream_index;
    FILE *fp;
    int pos;
    (void)functor;
    (void)regs;

    if(!check_int_arg(args, &stream_index) ||
       !check_int_arg(args+1, &pos))
        return FALSE;
    fp = (FILE *) INT_TO_PTR(stream_index);
    fseek(fp, pos, SEEK_SET);
    return TRUE;
}

static Bool bip_get_pos(Regs *regs, Index functor, Index args)
{
    int stream_index;
    FILE *fp;
    int pos;
    (void)functor;

    if(!check_int_arg(args, &stream_index))
        return FALSE;
    fp = (FILE *) INT_TO_PTR(stream_index);
    pos = ftell(fp);

    return set_int_arg(regs, args+1, pos);
}

static Bool bip_flush(Regs *regs, Index functor, Index args)
{
    int stream_index;
    FILE *fp;
    int res;
    (void)functor;
    (void)regs;

    if(!check_int_arg(args, &stream_index))
        return FALSE;
    fp = (FILE *) INT_TO_PTR(stream_index);
    res = fflush(fp);
    if(res == 0)
        return TRUE;

    return FALSE;
}

static void remove_choices(Regs *regs, Index choice)
{
    Index adm = regs->lastadm;

    while(1) {
        STACK(adm, Adm).clauses = 0;
        if(adm < choice) 
            break;
        adm = STACK(adm, Adm).prevchoice;
    } 

    STACK(regs->lastadm, Adm).prevchoice = adm;
}

static Bool bip_cut(Regs *regs, Index functor, Index args)
{
    Index choice = STACK(regs->lastadm, Adm).goals;
    (void)functor;
    (void)regs;
    (void)args;

    remove_choices(regs, choice);
    
    return TRUE;
}

static Bool bip_choice(Regs *regs, Index functor, Index args)
{
    Index choice;
    Index pos;
    Tagged t;
    (void)functor;

    choice = STACK(regs->lastadm, Adm).goals;
    pos = apl_alloc_array(&stack, 1);
    SETINT(t);
    INT(t) = choice;
    TAGGED(pos) = t;

    return apl_unify(regs, args, pos);
}

static Bool bip_cut_at(Regs *regs, Index functor, Index args)
{
    Index choice;
    (void)functor;

    if(!check_int_arg(args, &choice))
        return FALSE;

    if(choice == 0) {
        fprintf(stderr, "\nInternal error: uncaught exception\n");
        exit(1);
    }

    remove_choices(regs, choice);

    return TRUE;
}

static Bool bip_var_index(Regs *regs, Index functor, Index args)
{
    Index v;
    (void)functor;
    
    v = apl_deref(args);
    if(!ISREF(TAGGED(v)))
        return FALSE;
    
    return set_int_arg(regs, args+1, ATI(v));
}

static void term_to_clause(Regs *regs, Index src, Index dst,  Array *cla)
{
    Tagged s;
    Tagged d;

    src = apl_deref(src);
    s = TAGGED(src);
    
    if(ISREF(s)) {
        apl_trail(regs, src);
        SETTMP(s);
        INDEX(s) = dst;
        TAGGED(src) = s;
    }
    
    if(ISTMP(s)) {
        SETREF(d);
        INDEX(d) = INDEX(s) - dst;
    }
    else if(ISSTR(s)) {
        unsigned int arity;
        unsigned int i;
        Index strpos;
        Index funct;

        src = AT(src,s);
        funct = STACK(src, Index);
        arity = FUNCTOR(funct).arity;
        strpos = apl_alloc_array(cla, 1 + arity);
        SETSTR(d);
        INDEX(d) = strpos - dst;
        CLAUSE(cla, strpos, Index) = funct;

        for(i = 1; i <= arity; i++)
            term_to_clause(regs, src+i, strpos+i, cla);
    }
    else if(ISFLT(s)) {
        Index fltpos;

        fltpos = apl_alloc_array_even(cla, 2);
        SETFLT(d);
        INDEX(d) = fltpos - dst;

        CLAUSE(cla, fltpos, double) = STACK(AT(src,s), double);
    }
    else 
        d = s;

    CLAUSE(cla, dst, Tagged) = d;
}

static Clause *to_clause(Regs *regs, Index head, Index goallist)
{
    Index pos;
    Tagged t;
    Clause *cl;
    Array *cla;

    cl = malloc(sizeof(Clause));
    assert(cl != 0);
    cl->refctr = 0;
    cla = &cl->clause;
    apl_init_array(cla, sizeof(Tagged));

    pos = apl_alloc_array(cla, 1);
    term_to_clause(regs, head, pos, cla);

    cl->headsize = cla->end;
    cl->firstgoal = 0;
    cl->lastgoal = 0;

    goallist = apl_deref(goallist);
    t = TAGGED(goallist);
    while(!ISATM(t)) {
        goallist = AT(goallist, t);
        assert(ISSTR(t) && STACK(goallist, Index) == FUNCT_DOT);
        goallist ++;

        pos = apl_alloc_array(cla, 2);
        if(cl->lastgoal == 0)
            cl->firstgoal = pos;
        else 
            CLAUSE(cla, cl->lastgoal, Index) = pos - cl->lastgoal;
        
        cl->lastgoal = pos;
        term_to_clause(regs, goallist, pos+1, cla);
        
        goallist ++;
        goallist = apl_deref(goallist);
        t = TAGGED(goallist);
    }
    assert(ATMFUNCT(t) == FUNCT_NIL);

    return cl;
}

static Bool bip_assert(Regs *regs, Index functor, Index args)
{
    Clause *cl;
    Index funct;
    PredType predtype = PRED_DYNAMIC;
    Bool isfirst = FALSE;

    if(functor == FUNCT_ASSERTS)
        predtype = PRED_STATIC;
    if(functor == FUNCT_ASSERTA)
        isfirst = TRUE;

    funct = apl_get_functor(args);
    if(funct == -1)
        return FALSE;
    if(FUNCTOR(funct).type != PRED_UNDEF &&
       FUNCTOR(funct).type != predtype)
        return FALSE;

    if(FUNCTOR(funct).type == PRED_UNDEF)
        FUNCTOR(funct).pred.clauses = 0;

    FUNCTOR(funct).type = predtype;
    if(FUNCTOR(funct).name[0] == '$')
        FUNCTOR(funct).access = ACCESS_HIDDEN;
    else
        FUNCTOR(funct).access = ACCESS_USER;

    cl = to_clause(regs, args, args+1);
    cl->funct = funct;

    if(isfirst) {
        cl->next = FUNCTOR(funct).pred.clauses;
        FUNCTOR(funct).pred.clauses = cl;
    }
    else {
        Clause **clp = &FUNCTOR(funct).pred.clauses;
        for(; *clp != 0; clp = &(*clp)->next);
	*clp = cl;
        cl->next = 0;
    }

    apl_undo_assignments(regs);

    return set_int_arg(regs, args+2, PTR_TO_INT(cl));
}

static Bool bip_clause(Regs *regs, Index functor, Index args)
{
    Index head;
    Index body;
    Index clpos;
    Clause *cl;
    (void)functor;

    if(!check_int_arg(args, &clpos))
        return FALSE;

    cl = STACK(clpos, Clause *);
    if(cl == 0)
        return FALSE;
    
    head = apl_copy_clause(cl);
    body = head + cl->firstgoal + 1;

    if(!apl_unify(regs, head, args+1) || !apl_unify(regs, body, args+2))
        return FALSE;
    
    return TRUE;
}

static Bool bip_retract(Regs *regs, Index functor, Index args)
{
    Index clpos;
    Clause *cl;
    Clause **clp;
    (void)functor;
    (void)regs;

    if(!check_int_arg(args, &clpos))
        return FALSE;

    cl = STACK(clpos, Clause *);
    if(cl == 0)
        return FALSE;

    if(cl->funct != -1) {
	clp = &FUNCTOR(cl->funct).pred.clauses;
	for(; *clp != cl; clp = &(*clp)->next)
	    assert(*clp != 0);

	*clp = cl->next;
	cl->next = 0;
	cl->funct = -1;
    }

    return TRUE;
}

static Bool bip_clauselist(Regs *regs, Index functor, Index args)
{
    Index funct;
    Clause *clause;
    Index clpos;
    Index clarray;
    Tagged t;
    Index pos;
    (void)functor;
    
    funct = apl_get_functor(args);
    if(funct == -1)
        return FALSE;

    if(FUNCTOR(funct).type == PRED_DYNAMIC)
        clause = FUNCTOR(funct).pred.clauses;
    else if(FUNCTOR(funct).type == PRED_UNDEF)
        clause = 0;
    else
        return FALSE;

    clpos = clarray = apl_alloc_array(&stack, 1);
    for(; clause != 0; clause = clause->next) {
        clause->refctr++;
        STACK(clpos, Clause *) = clause;
        clpos = apl_alloc_array(&stack, 1);
    }
    STACK(clpos, Clause *) = 0;

    STACK(regs->lastadm, Adm).clauselist = clarray;

    SETINT(t);
    INT(t) = clarray;
    pos = apl_alloc_array(&stack, 1);
    TAGGED(pos) = t;

    apl_unify(regs, pos, args+1);

    return TRUE;
}

static Bool bip_abolish(Regs *regs, Index functor, Index args)
{
    Index funct;
    Clause *clause;
    Clause *nextclause;
    (void)functor;
    (void)regs;

    funct = apl_get_functor(args);
    if(funct == -1)
        return FALSE;
    if(FUNCTOR(funct).type == PRED_UNDEF)
        return TRUE;

    if((FUNCTOR(funct).type != PRED_DYNAMIC &&
        FUNCTOR(funct).type != PRED_STATIC))
        return FALSE;

    clause = FUNCTOR(funct).pred.clauses;
    while(clause != 0) {
        clause->refctr++;
        nextclause = clause->next;
        clause->funct = -1;
        clause->next = 0;
        apl_clause_unref(clause);
        
        clause = nextclause;
    }
    FUNCTOR(funct).type = PRED_UNDEF;

    return TRUE;
}

static Bool bip_delclause(Regs *regs, Index functor, Index args)
{
    Clause *cl;
    Clause **clp;
    int ci;
    (void)functor;
    (void)regs;

    if(!check_int_arg(args, &ci))
        return FALSE;

    cl = (Clause *) INT_TO_PTR(ci);
    if(cl->funct != -1) {
	clp = &FUNCTOR(cl->funct).pred.clauses;
	for(; *clp != cl; clp = &(*clp)->next)
	    assert(*clp != 0);

	*clp = cl->next;
	cl->next = 0;
	cl->funct = -1;
    }
    apl_clause_unref(cl);

    return TRUE;
}

static Bool bip_refclause(Regs *regs, Index functor, Index args)
{
    Clause *cl;
    int ci;
    (void)functor;
    (void)regs;

    if(!check_int_arg(args, &ci))
        return FALSE;

    cl = (Clause *) INT_TO_PTR(ci);
    cl->refctr++;
    
    return TRUE;
}

static Bool bip_set_global(Regs *regs, Index functor, Index args)
{
    Index funct;
    Index prevval;
    Index assigi;
    (void)functor;

    if(!check_atm_arg(args, &funct))
        return FALSE;

    if(FUNCTOR(funct).type == PRED_UNDEF) {
        FUNCTOR(funct).type = PRED_GLOBVAR;
        prevval = 0;
    }
    else if(FUNCTOR(funct).type == PRED_GLOBVAR) 
        prevval = FUNCTOR(funct).pred.globvar;
    else
        return FALSE;

    DEB(fprintf(debfp, "assinged %s\n", FUNCTOR(funct).name));
    
    assigi = apl_alloc_array(&trail, 2);
    SETATM(TRAIL(assigi));
    INDEX(TRAIL(assigi)) = funct;
    INDEX(TRAIL(assigi+1)) = prevval;

    STACK(regs->lastadm, Adm).numassig++;
    
    FUNCTOR(funct).pred.globvar = apl_deref(args+1);

    return TRUE;
}

static Bool bip_get_global(Regs *regs, Index functor, Index args)
{
    Index funct;
    (void)functor;

    if(!check_atm_arg(args, &funct) || FUNCTOR(funct).type != PRED_GLOBVAR)
        return FALSE;
    
    return apl_unify(regs, FUNCTOR(funct).pred.globvar, args+1);
}

static Bool bip_set_bb(Regs *regs, Index functor, Index args)
{
    Index funct;
    Tagged val;
    (void)functor;
    (void)regs;

    if(!check_atm_arg(args, &funct))
        return FALSE;
    
    val = TAGGED(apl_deref(args+1));
    if(!ISATM(val) && !ISINT(val) && !ISREF(val))
        return FALSE;

    FUNCTOR(funct).blackboard = val;

    return TRUE;
}

static Bool bip_get_bb(Regs *regs, Index functor, Index args)
{
    Index funct;
    Tagged val;
    (void)functor;

    if(!check_atm_arg(args, &funct))
        return FALSE;

    val = FUNCTOR(funct).blackboard;
    if(ISREF(val))
        return FALSE;

    return set_atomic_arg(regs, args+1, val);
}


static Bool bip_trace(Regs *regs, Index functor, Index args)
{
    int trace;
    (void)functor;

    if(!check_int_arg(args, &trace))
        return FALSE;
    regs->trace = trace;

    return TRUE;
}

static Bool bip_runtime(Regs *regs, Index functor, Index args)
{
    static int last = 0;
    struct rusage usage;
    int curr;
    (void)functor;

    getrusage(RUSAGE_SELF, &usage);
    curr = usage.ru_utime.tv_sec * 1000 + usage.ru_utime.tv_usec / 1000;

    if(!set_int_arg(regs, args, curr) ||
       !set_int_arg(regs, args+1, curr - last))
        return FALSE;
    last = curr;
    
    return TRUE;
}

static Bool bip_nth_functor(Regs *regs, Index functor, Index args)
{
    Index funct;
    (void)functor;
    
    if(!check_int_arg(args, &funct))
        return FALSE;
    
    if(funct<0 || funct+0U >= functors.end) /**** pts ****/ /* Dat: avoid comparision of signed and unsigned */
        return FALSE;

    if(FUNCTOR(funct).arity == 0)
	return set_atm_arg(regs, args+1, funct);
    else {
	Index pos;
	pos = new_struct(regs, funct);
	return apl_unify(regs, args+1, pos);
    }
}

static Bool bip_pred_info(Regs *regs, Index functor, Index args)
{
    Index funct;
    Index functaccess;
    Index functtype;
    Bool ispred;
    (void)functor;
    
    funct = apl_get_functor(args);
    if(funct == -1)
        return FALSE;

    switch(FUNCTOR(funct).type) {
    case PRED_STATIC:   functtype = FUNCT_PRED_STATIC;  ispred = TRUE;   break;
    case PRED_DYNAMIC:  functtype = FUNCT_PRED_DYNAMIC; ispred = TRUE;   break;
    case PRED_BUILTIN:  functtype = FUNCT_PRED_BUILTIN; ispred = TRUE;   break;
    case PRED_GLOBVAR:  functtype = FUNCT_PRED_GLOBVAR; ispred = FALSE;  break;
    default:            functtype = FUNCT_NIL;          ispred = FALSE;  break;

    }
    
    if(ispred) {
        switch(FUNCTOR(funct).access) {
        case ACCESS_USER:   functaccess = FUNCT_ACCESS_USER;   break;
        case ACCESS_PROLOG: functaccess = FUNCT_ACCESS_PROLOG; break;
        case ACCESS_HIDDEN: functaccess = FUNCT_ACCESS_HIDDEN; break;
        default:            functaccess = FUNCT_NIL;           break;
        }
    }
    else functaccess = FUNCT_NIL;

    if(!set_atm_arg(regs, args+1, functtype) ||
       !set_atm_arg(regs, args+2, functaccess))
        return FALSE;

    return TRUE;
}

static Bool bip_error(Regs *regs, Index functor, Index args)
{
    (void)regs; (void)args;
    fprintf(stderr, "\nInternal error: %s/%i called\n",
            FUNCTOR(functor).name, 
            FUNCTOR(functor).arity);
    
    exit(1);
}

static void set_builtin(Index funct, BuiltIn bip, Bool debug)
{
    FUNCTOR(funct).type = PRED_BUILTIN;
    FUNCTOR(funct).pred.builtin = bip;

    if(!debug)
        FUNCTOR(funct).access = ACCESS_HIDDEN;
    else
        FUNCTOR(funct).access = ACCESS_PROLOG;
}

static void add_builtin(const char *name, unsigned int arity, BuiltIn bip,
                        Bool debug)
{
    Index funct = apl_search_functor(name, arity);
    set_builtin(funct, bip, debug);
}

static void add_builtin_funct(Index funct, const char *name,
                              unsigned int arity, BuiltIn bip, Bool debug)
{
    apl_add_functor(funct, name, arity);
    set_builtin(funct, bip, debug);
}

void apl_init_builtins()
{
    add_builtin_funct(FUNCT_INTEGER,  "integer",  1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_FLOAT,    "float",    1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_NUMBER,   "number",   1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_ATOM,     "atom",     1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_ATOMIC,   "atomic",   1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_COMPOUND, "compound", 1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_CALLABLE, "callable", 1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_VAR,      "var",      1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_NONVAR,   "nonvar",   1, bip_type,   TRUE);
    add_builtin_funct(FUNCT_LESS,     "$less",    2, bip_cmp,    FALSE);
    add_builtin_funct(FUNCT_LEQ,      "$leq",     2, bip_cmp,    FALSE);
    add_builtin_funct(FUNCT_EQ,       "$eq",      2, bip_cmp,    FALSE);
    add_builtin_funct(FUNCT_NEQ,      "$neq",     2, bip_cmp,    FALSE);
    add_builtin_funct(FUNCT_ASSERTA,  "$asserta", 3, bip_assert, FALSE);
    add_builtin_funct(FUNCT_ASSERTZ,  "$assertz", 3, bip_assert, FALSE);
    add_builtin_funct(FUNCT_ASSERTS,  "$asserts", 3, bip_assert, FALSE);

    add_builtin("true",            0, bip_true,             FALSE);
    add_builtin("fail",            0, bip_fail,             FALSE);
    add_builtin("!",               0, bip_cut,              FALSE);
    add_builtin(",",               2, bip_error,            FALSE);
    add_builtin(";",               2, bip_error,            FALSE);
    add_builtin("->",              2, bip_error,            FALSE);
    add_builtin("$halt",           1, bip_halt,             FALSE);
    add_builtin("$get_code",       1, bip_get_code,         FALSE);
    add_builtin("$peek_code",      1, bip_peek_code,        FALSE);
    add_builtin("$put_code",       1, bip_put_code,         FALSE);
    add_builtin("$open",           3, bip_open,             FALSE);
    add_builtin("$close",          1, bip_close,            FALSE);
    add_builtin("$stream_type",    3, bip_stream_type,      FALSE);
    add_builtin("$set_pos",        2, bip_set_pos,          FALSE);
    add_builtin("$get_pos",        2, bip_get_pos,          FALSE);    
    add_builtin("$flush",          1, bip_flush,            FALSE);
    add_builtin("$atom_codes",     2, bip_atom_codes,       FALSE);
    add_builtin("$number_codes",   2, bip_number_codes,     FALSE);
    add_builtin("$arg",            3, bip_arg,              FALSE);
    add_builtin("$set_functor",    3, bip_set_functor,      FALSE);
    add_builtin("$get_functor",    3, bip_get_functor,      FALSE);
    add_builtin("$choice",         1, bip_choice,           FALSE);
    add_builtin("$cut",            1, bip_cut_at,           FALSE);
    add_builtin("$var_index",      2, bip_var_index,        FALSE);
    add_builtin("$clause",         3, bip_clause,           FALSE);
    add_builtin("$retract",        1, bip_retract,          FALSE);
    add_builtin("$delclause",      1, bip_delclause,        FALSE);
    add_builtin("$refclause",      1, bip_refclause,        FALSE);    
    add_builtin("$clauselist",     2, bip_clauselist,       FALSE);
    add_builtin("$abolish",        1, bip_abolish,          FALSE);
    add_builtin("$set_global",     2, bip_set_global,       FALSE);
    add_builtin("$get_global",     2, bip_get_global,       FALSE);
    add_builtin("$set_bb",         2, bip_set_bb,           FALSE);
    add_builtin("$get_bb",         2, bip_get_bb,           FALSE);
    add_builtin("$runtime",        2, bip_runtime,          FALSE);
    add_builtin("$expr",           2, bip_expr,             FALSE);
    add_builtin("$nth_functor",    2, bip_nth_functor,      FALSE);
    add_builtin("$pred_info",      3, bip_pred_info,        FALSE);
    add_builtin("$trace",          1, bip_trace,            FALSE);

    bb_set_int(FUNCT_CURR_INPUT, PTR_TO_INT(stdin));
    bb_set_int(FUNCT_CURR_OUTPUT, PTR_TO_INT(stdout));
    bb_set_int(FUNCT_CURR_ERROR, PTR_TO_INT(stderr));

    bb_set_atom(FUNCT_VERSION, FUNCT_VERSION_VALUE);
}

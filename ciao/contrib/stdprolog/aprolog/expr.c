#include "aprolog.h"

#include <math.h>

#define MAX_INT ( (1 << (sizeof(int)*8-2)) - 1) /**** pts ****/
#define MIN_INT (-(1 << (sizeof(int)*8-2)))     /**** pts ****/

/** Mask out the highest bit. */
#define MASKINT(i) (((int) (i) << 1) >> 1)

static Index check_float_overflow(double f)
{
    if(f == HUGE_VAL || f == -HUGE_VAL) /* Imp: possibly better test */
        return FUNCT_FLOAT_OVERFLOW;

    return FUNCT_NIL;
}

static Index check_int_overflow(double f, int *ip)
{
    if(f > MAX_INT || f < MIN_INT)
        return FUNCT_INT_OVERFLOW;
    
    *ip = (int) f;

    return FUNCT_NIL;
}

static Index calc_one_int(Index funct, int i0, double *fp, Bool *isintp)
{
    *isintp = TRUE;
    switch(funct) {
    case FUNCT_A_NEG:      *fp = -i0;                            break;
    case FUNCT_A_NOT:      *fp = MASKINT(~i0);                   break;
    case FUNCT_A_ABS:      *fp = i0 < 0 ? -i0 : i0;              break;
    case FUNCT_A_SIGN:     *fp = i0 < 0 ? -1 : (i0 > 0 ? 1 : 0); break;

    default:
        *isintp = FALSE;
        switch(funct) {
        case FUNCT_A_FLOAT:    *fp = i0;             break;
        case FUNCT_A_SIN:      *fp = sin(i0);        break;
        case FUNCT_A_COS:      *fp = cos(i0);        break;
        case FUNCT_A_ATAN:     *fp = atan(i0);       break;
        case FUNCT_A_SQRT:  if(i0 < 0) return FUNCT_UNDEFINED;
                               *fp = sqrt(i0);       break;
        case FUNCT_A_LOG:   if(i0 <= 0) return FUNCT_UNDEFINED;
                               *fp = log(i0);        break;
        case FUNCT_A_EXP:      *fp = exp(i0);        break;

        default:
            return FUNCT_TYPE_ERROR;
        }
    }
    return FUNCT_NIL;
}

static Index calc_one_float(Index funct, double f0, double *fp, Bool *isintp)
{
    *isintp = FALSE;
    switch(funct) {
    case FUNCT_A_NEG:      *fp = -f0;            break;
    case FUNCT_A_INT:      *fp = (int) f0;       break;
    case FUNCT_A_FRAC:     *fp = f0 - (int) f0;  break;
    case FUNCT_A_ABS:      *fp = fabs(f0);       break;
    case FUNCT_A_SIGN:    
        *fp = f0 < 0.0 ? -1.0 : (f0 > 0.0 ? 1.0 : 0.0);  break;
    case FUNCT_A_FLOAT:    *fp = f0;             break;
    case FUNCT_A_SIN:      *fp = sin(f0);        break;
    case FUNCT_A_COS:      *fp = cos(f0);        break;
    case FUNCT_A_ATAN:     *fp = atan(f0);       break;
    case FUNCT_A_SQRT:  if(f0 < 0) return FUNCT_UNDEFINED;
                           *fp = sqrt(f0);       break;
    case FUNCT_A_LOG:   if(f0 <= 0) return FUNCT_UNDEFINED;
                           *fp = log(f0);        break;
    case FUNCT_A_EXP:      *fp = exp(f0);        break;

    default:
        *isintp = TRUE;
        switch(funct) {
        case FUNCT_A_ROUND:    *fp = floor(f0+0.5);  break;
        case FUNCT_A_TRUNCATE: *fp = (int) f0;       break;
        case FUNCT_A_FLOOR:    *fp = floor(f0);      break;
        case FUNCT_A_CEILING:  *fp = ceil(f0);       break;

        default:
            return FUNCT_TYPE_ERROR;
        }
    }

    return FUNCT_NIL;
}

/***** pts ****/
/** @returns the maximum signed value for the expression */
#define maxsval
/**
 * @peram res The result is saved the *res (can be clobbered)
 * @return FUNCT_NIL on success (and sets *res), FUNCT_FAIL on
 *   FUNCT_INT_OVERFLOW on overflow, or other error type.
 */
static Index calc_two_int_no_overflow(Index funct, int i0, int i1, int *res)
{
    double d;
    switch(funct) {
    case FUNCT_A_ADD:   if (i1>=0 && i0>MAX_INT-i1) return FUNCT_INT_OVERFLOW;
                        if (i1< 0 && i0<MIN_INT-i1) return FUNCT_INT_OVERFLOW;
                        *res = i0 + i1; break;
    case FUNCT_A_SUB:   if (i1>=0 && i0<MIN_INT+i1) return FUNCT_INT_OVERFLOW;
                        if (i1< 0 && i0<MAX_INT+i1) return FUNCT_INT_OVERFLOW;
                        *res = i0 - i1; break;
    case FUNCT_A_MUL:   *res = i0 * i1;
                        if (*res/i1!=i0) return FUNCT_INT_OVERFLOW;
                        break;
    case FUNCT_A_IDIV:  if(i1 == 0) return FUNCT_ZERO_DIVISOR;
    			if (0!=(i0%i1)) return FUNCT_INT_OVERFLOW;
                        *res = i0 / i1;
                        break;
    case FUNCT_A_AND:   *res = (i0 & i1);                     break;
    case FUNCT_A_OR:    *res = (i0 | i1);                     break;
    case FUNCT_A_SHL:   *res = MASKINT(i0 << i1);             break;
    case FUNCT_A_SHR:   *res = MASKINT(i0 >> i1);             break;
    case FUNCT_A_REM:   if(i1 == 0) return FUNCT_ZERO_DIVISOR;
                        /* Dat: sign(*fp) := sign(i0) */
                        *res = i0 % i1; break; /**** pts ****/
                        /* *fp = i0 - (i0 / i1) * i1;            break; */
    case FUNCT_A_MOD:   if(i1 == 0) return FUNCT_ZERO_DIVISOR;
                        /* Dat: sign(*fp) := sign(i1) */
                        *res = (i1>0) ? ((i0>=0) ? i0%i1 : i1+i0%i1)
                                     : ((i0>=0) ? i1+i0%i1 : i0%i1) ;
                        /* *fp = i0 - (int) floor((double) i0 / i1) * i1; break; */
                        break;
    case FUNCT_A_POW:   d=pow(i0, i1); *res=(int)d;
                        if (d!=(double)*res) return FUNCT_INT_OVERFLOW; /**** pts ****/
                        break;

    default:
        return FUNCT_TYPE_ERROR;
    }

    return FUNCT_NIL;
}

static Index calc_two_int(Index funct, int i0, int i1, double *fp, 
                          Bool *isintp)
{
    *isintp = TRUE;
    switch(funct) {
    case FUNCT_A_ADD:   *fp = i0 + i1;                        break;
    case FUNCT_A_SUB:   *fp = i0 - i1;                        break;
    case FUNCT_A_MUL:   *fp = i0 * i1;                        break;
    case FUNCT_A_IDIV:  if(i1 == 0) return FUNCT_ZERO_DIVISOR;
                        *fp = i0 / i1;                        break;
    case FUNCT_A_AND:   *fp = (i0 & i1);                      break;
    case FUNCT_A_OR:    *fp = (i0 | i1);                      break;
    case FUNCT_A_SHL:   *fp = MASKINT(i0 << i1);              break;
    case FUNCT_A_SHR:   *fp = MASKINT(i0 >> i1);              break;
    case FUNCT_A_REM:   if(i1 == 0) return FUNCT_ZERO_DIVISOR;
                        /* Dat: sign(*fp) := sign(i0) */
                        *fp = i0 % i1; break; /**** pts ****/
                        /* *fp = i0 - (i0 / i1) * i1;            break; */
    case FUNCT_A_MOD:   if(i1 == 0) return FUNCT_ZERO_DIVISOR;
                        /* Dat: sign(*fp) := sign(i1) */
                        *fp = (i1>0) ? ((i0>=0) ? i0%i1 : i1+i0%i1)
                                     : ((i0>=0) ? i1+i0%i1 : i0%i1) ;
                        /* *fp = i0 - (int) floor((double) i0 / i1) * i1; break; */
                        break;
    case FUNCT_A_POW:   *isintp = FALSE; *fp = pow(i0, i1);   break;

    default:
        return FUNCT_TYPE_ERROR;
    }

    return FUNCT_NIL;
}

static Index calc_two_float(Index funct, double f0, double f1, double *fp)
{
    switch(funct) {
    case FUNCT_A_ADD:      *fp = f0 + f1;     break;
    case FUNCT_A_SUB:      *fp = f0 - f1;     break;
    case FUNCT_A_MUL:      *fp = f0 * f1;     break;
    case FUNCT_A_DIV:   if(f1 == 0.0) return FUNCT_ZERO_DIVISOR;
                           *fp = f0 / f1;     break;
    case FUNCT_A_POW:      *fp = pow(f0, f1); break;
    default:
        return FUNCT_TYPE_ERROR;
    }

    return check_float_overflow(*fp);
}

/** Must set *isintp=... iff returns FUNCT_NIL */
Index apl_calc_expr(Index i, double *fp, int *ip, Bool *isintp)
{
    Tagged t;
    double f0, f1;
    int i0, i1;
    Bool isint0, isint1;
    Index funct;
    Index res;

    i = apl_deref(i);
    t = TAGGED(i);
    
    if(ISINT(t)) {
        *ip = INT(t);
        *isintp = TRUE;
        return FUNCT_NIL;
    }
    if(ISFLT(t)) {
        *fp = STACK(AT(i,t), double);
        *isintp = FALSE;
        return FUNCT_NIL;
    }
    if(!ISSTR(t))
        return FUNCT_TYPE_ERROR;

    i = AT(i, t);
    funct = STACK(i, Index);
    if(FUNCTOR(funct).arity == 0 || FUNCTOR(funct).arity > 2)
        return FUNCT_TYPE_ERROR;
    
    res = apl_calc_expr(i+1, &f0, &i0, &isint0);
    if(res != FUNCT_NIL)
        return res;

    if(FUNCTOR(funct).arity == 1) {
        if(isint0)
            res =  calc_one_int(funct, i0, fp, isintp);
        else 
            res =  calc_one_float(funct, f0, fp, isintp);
    }
    else {
        res = apl_calc_expr(i+2, &f1, &i1, &isint1);
        if(res != FUNCT_NIL)
            return res;
        
        if(isint0 && isint1 && funct != FUNCT_A_DIV)
        {   /**** pts ****/
            res=calc_two_int_no_overflow(funct, i0, i1, ip);
            if (FUNCT_INT_OVERFLOW!=res) { *isintp=1; return res; }

            res = calc_two_int(funct, i0, i1, fp, isintp);
            /* Dat: the standard says that the product of two big integers should trigger an int_overflow */
        }
        else {
            if(isint0)
                f0 = i0;
            if(isint1)
                f1 = i1;

            *isintp = FALSE;
            return calc_two_float(funct, f0, f1, fp);
        }
    }

    if(res != FUNCT_NIL)
        return res;

    if(*isintp)
        return check_int_overflow(*fp, ip);
    else
        return check_float_overflow(*fp);
               
}

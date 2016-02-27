/***************************************************************************
                          float_consts.h  -  description
                             -------------------
    begin                : Wed Jun 4 2003
    copyright            : (C) 2003 by Edison Mera Menéndez
    email                : edison@clip.dia.fi.upm.es
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef FLOAT_CONSTS_H
#define FLOAT_CONSTS_H

#include "termdefs.h"

#define MAX_EXP   1074
#define MIN_EXP   1023
#define HIDD_BIT  0x0010000000000000LL
#define SHR_EXP   52

/*
#define SIGN_MASK 0x8000000000000000LL
#define EXP_MASK  0x7FF0000000000000LL
#define SIG_MASK0 0x000FFFFF00000000LL
#define SIG_MASK1 0x00000000FFFFFFFFLL
*/

extern double invlog2[];
extern ENG_LFLT *powtable[];
extern ENG_LFLT *invpowtable[];

/* #define EXP_CHAR  36 */
/* #define FRAC_SEP  36 */

/* floating point char */
#define FLOAT_POINT '.'

extern int char_digit[256];
extern char digits_upper[];
extern char digits_lower[];
extern char exponents_upper[];
extern char exponents_lower[];

/*
  the next functions must be called before use of any base,
  and only is necessary one time per program execution.
  These functions speed-up the work with other bases rather than
  10, but uses more memory.  Use them when you will work with
  too much numbers.
*/

void fillchardigit();
void fillpowtable(int base);
void freepowtable(int base);
void fillallpowtable();
void freeallpowtable();
ENG_LFLT powl_int(int base, int exp);

#endif /*FLOAT_CONSTS_H*/


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

#define SIGN_MASK 0x8000000000000000LL
#define SHR_EXP   52
#define MAX_EXP   1074
#define MIN_EXP   1023
#define EXP_MASK  0x7FF0000000000000LL
#define SIG_MASK  0x000FFFFFFFFFFFFFLL
#define HIDD_BIT  0x0010000000000000LL

extern double invlog2[];
extern long double *powtable[];
extern long double *invpowtable[];

#define EXP_CHAR  36
#define FRAC_SEP  37

extern char digits_upper[];
extern char digits_lower[];

/*
  the next functions must be called before use of any base,
  and only is necessary one time per program execution.
  These functions speed-up the work with other bases rather than
  10, but uses more memory.  Use them when you will work with
  too much numbers.
*/

void fillpowtable(int B);
void freepowtable(int B);
void fillallpowtable();
void freeallpowtable();
long double powl_int(long double x, int n);

#endif /*FLOAT_CONSTS_H*/

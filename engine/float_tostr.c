/***************************************************************************
                          float_tostr.c  -  description
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
#include "float_tostr.h"
#include <math.h>
#include <string.h>

/*

Functions that generates for any IEEE-754 64 bits floating point value,
the representation on other base, in the format: +-0.XXXXXXXe+-XXX,
where the Xs represent the digits in the given base.

Based on the paper:

"Printing Floating-Point Numbers Quickly and Accurately.  Robert
G. Burger, R. Kent Dybvig. Indiana University Computer Science
Department. Lindley Hall 215.  Boomington, Indiana 47405.
{burger,dyb}@cs.indiana.edu.  1996 by ACM."

Some ideas has been taked reading the softfloat library, from
John R. Hauser, available at
http://www.cs.berkeley.edu/~jhauser/arithmetic/SoftFloat.html.


*/

/*----------------------------------------------------------------------------
| Returns the number of leading 0 bits before the most-significant 1 bit of
| `a'.  If `a' is zero, 32 is returned.
*----------------------------------------------------------------------------*/

static char countLeadingZeros32( unsigned long a )
{
  static const char countLeadingZerosHigh[] = {
    8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  };
  char shiftCount;

  shiftCount = 0;
  if ( a < 0x10000 ) {
    shiftCount += 16;
    a <<= 16;
  }
  if ( a < 0x1000000 ) {
    shiftCount += 8;
    a <<= 8;
  }
  shiftCount += countLeadingZerosHigh[ a>>24 ];
  return shiftCount;
}

/*----------------------------------------------------------------------------
| Returns the number of leading 0 bits before the most-significant 1 bit of
| `a'.  If `a' is zero, 64 is returned.
*----------------------------------------------------------------------------*/

static char countLeadingZeros64( unsigned long long a )
{
  char shiftCount;

  shiftCount = 0;
  if ( a < ( (unsigned long long) 1 )<<32 ) {
    shiftCount += 32;
  }
  else {
    a >>= 32;
  }
  shiftCount += countLeadingZeros32( a );
  return shiftCount;
}

char * nextDigit(
        char buffer[],
        char *start,
        int precision,

        int B,
        char digits[],
        int d,
        int *exp,
        int carrytoexp
)
{
  if(d+1<B) {
    *buffer++ = digits[d+1];
  } else {
    while(buffer>start) {
      d = *--buffer - digits[0];
      if(d+1<B) {
        *buffer++ = digits[d+1];
        return buffer;
      }
    }
    if(carrytoexp) {
      *buffer++ = digits[1];
      (*exp)++;
    }
    else {
      *--buffer = digits[1];
      buffer++;
    }
  }
  return buffer;
}

// this function generates the digits of number in a exponential format of base B

char * generate(
        char buffer[],
        int precision,
        
        unsigned long long r,
        unsigned long long s,
        int sa,
        unsigned long long m1,
        unsigned long long m0,
        int *exp,
        int carrytoexp,
        int B,
        char digits[],
        int isLow,
        int isHigh)
{
  int tc1;
  int tc2;
  int d;
  int i;

  int ra = 0;
  
  // 15 --> 52 / log2(10)

  char *start = buffer;
  i = precision;
  while (1) {
    i--;
    d = (int) (r / (s + 1));
    r = r - d * s;
    ra = ra - d * sa;
    r += ra / 16;
    ra %= 16;
    if(ra < 0) {
      r--;
      ra += 16;
    } else if(ra >= 16) {
      r++;
      ra -= 16;
    }
    while(r > s || ( r == s && ra >= sa )) {
      d++;
      r -= s;
      ra -= sa;
      if(ra < 0) {
        r--;
        ra += 16;
      } else if(ra >= 16) {
        r++;
        ra -= 16;
      }
    }
    if(i<=0) {
      tc1 = 1;
      tc2 = 1;
    }
    else {
      if(isLow)
        tc1 = (r < m0)||(r == m0 && ra == 0);
      else
        tc1 = r < m0;
      if(isHigh)
        tc2 = (r + m1 > s)||((r + m1 == s)&& ra >= sa);
      else
        tc2 = (r + m1 > s)||((r + m1 == s)&& ra > sa);
    }
    if(!tc1) {
      if(!tc2) {
        *buffer++ = digits[d];
        r *= B;
        ra *= B;
        r += ra / 16;
        ra %= 16;
        m1 *= B;
        m0 *= B;
      } else {
        buffer = nextDigit(buffer, start, precision, B, digits, d, exp, carrytoexp);
        break;
      }
    } else {
      if(!tc2) {
        *buffer++ = digits[d];
        break;
      } else {
        if ((2*r<s)||(2*r == s && 2*ra < sa)) {
          if(d!=0)
            *buffer++ = digits[d];
          break;
        }
        else {
          buffer = nextDigit(buffer, start, precision, B, digits, d, exp, carrytoexp);
          break;
        }
      }
    }
  };
  char *p = buffer;
  p--;
  if(p[0]==digits[0]) {
    while(*p==digits[0] && p != buffer) {
      p--;
    }
    p++;
    buffer=p;
  }
  return buffer;
}

typedef struct {
  unsigned long long low;
  short high;
} long_double_bits;

static char * scale_zero(
        char *buffer,
        int precision,
        char format,
	char digits[])
{
  switch(format) {
  case 'E':case 'e':
    *buffer++ = digits[0];
    if(precision>0) {
      *buffer++ = digits[FRAC_SEP];
      while(precision--) {
        *buffer++ = digits[0];
      }
    }
    *buffer++ = digits[EXP_CHAR];
    *buffer++ = '+';
    *buffer++ = digits[0];
    *buffer++ = digits[0];
    break;
  case 'G':case 'g':
    *buffer++ = digits[0];
    break;
  case 'p':
    *buffer++ = digits[0];
    *buffer++ = digits[FRAC_SEP];
    *buffer++ = digits[0];
    break;
  case 'f':
    *buffer++ = digits[0];
    if(precision>0) {
      *buffer++ = digits[FRAC_SEP];
      while(precision--) {
        *buffer++ = digits[0];
      }
    }
    break;
  }
  return buffer;
}
 
static char * scale(
        char *buffer,
        int precision,
        char format,
        unsigned long long r,
        unsigned long long shift,
        unsigned long long m1,
        unsigned long long m0,
        int B,
        char digits[],
        int isLow,
        int isHigh,
        int e)
{
  int l = countLeadingZeros64(r);
  int exp = (int)ceil(invlog2[B] * (e + 63 - l) - 1e-10);
  int scalee;
  long double scale;
  long_double_bits *scaleb;
  unsigned long long s;
  int sa;
  int k = 4;
  char *start, *p_at;
  int accuracy;
  if(exp >= 0) {
    if(powtable[B]==NULL)
      scale = powl_int(B, exp);
    else
      scale =(powtable[B])[exp];
  } else {
    //scale = 1.0 / (powtable[B])[-exp];
    if(invpowtable[B]==NULL)
      scale = powl_int(B, exp);
    else
      scale =(invpowtable[B])[-exp];
  }

  scaleb = (long_double_bits *)&scale;
  scalee = scaleb->high & 0x7FFF;
  s = scaleb->low;
  scalee += -16383 - 63 + 5 - e;
  scalee = -scalee - 3;
  r <<= (k + scalee);
  sa = s % (1<<(8 - k));
  s >>= (8 - k);
  m1 <<= (k + scalee - shift);
  m0 <<= (k + scalee - shift);

  // fix exponent
  if ( ( isHigh && ((r + m1 > s)||((r + m1 == s)&& 0 >= sa)))
  || ( !isHigh && (r + m1 > s)) ) {
    //(*exp)++;
  } else {
    r  *= B;
    m1 *= B;
    m0 *= B;
    //s /= 10;
    exp--;
  }

  start = buffer;
  switch(format) {
    case 'E':case 'e':
      accuracy = precision + 1;
      buffer = generate(++buffer, accuracy,
        r,
        s,
        sa,
        m1,
        m0,
        &exp,
        1,
        B, digits, isLow, isHigh);
      while(buffer <= start + accuracy)
        *buffer++ = digits[0];
      start[0] = start[1];
      if ( start + 2 < buffer )
        start[1] = digits[FRAC_SEP];
      else
        buffer--;
      //start[1] = digits[FRAC_SEP];
      break;
    /*
      The format specifier p is for ciao prolog style.
    */
    case 'G':case 'g':case 'p':
      accuracy = precision;
      buffer = generate(++buffer, accuracy,
        r,
        s,
        sa,
        m1,
        m0,
        &exp,
        1,
        B, digits, isLow, isHigh);
      if(-5 < exp && exp < 0) {
        accuracy = (int)(buffer - start) - 1;
        buffer -= exp;
        memmove(start - exp + 1, start + 1, accuracy);
        *start++ = digits[0];
        *start++ = digits[FRAC_SEP];
        while(++exp) {
          *start++ = digits[0];
        }
        exp = 0;
      }
      else if(0 <= exp && exp < precision) {
        char * p_at = start + exp + 1;
        buffer--;
        if(buffer < p_at) {
          memmove(start, start + 1, (int)(buffer - start + 1));
          while(buffer < p_at)
            *buffer++ = digits[0];
          if(format=='p') {
            *buffer++ = digits[FRAC_SEP];
            *buffer++ = digits[0];
          }  
        } else if(buffer > p_at) {
          memmove(start, start + 1, (int)(p_at - start));
          *p_at = digits[FRAC_SEP];
          buffer++;
        } else {
          memmove(start, start + 1, (int)(buffer - start + 1));
          if(format=='p') {
            *buffer++ = digits[FRAC_SEP];
            *buffer++ = digits[0];
          }
        }  
        exp = 0;
      }
      else {
        start[0] = start[1];
        if ( start + 2 < buffer )
          start[1] = digits[FRAC_SEP];
        else {
          buffer--;
          if(format=='p') {
            *buffer++ = digits[FRAC_SEP];
            *buffer++ = digits[0];
          }
        }
      }
      break;
    case 'f':
      accuracy = precision + exp + 1;
      if(exp<0){
        *buffer++ = digits[0];
        p_at = buffer;
        *buffer++ = digits[FRAC_SEP];
        if(accuracy <= 0) {
          accuracy = precision;
          while(accuracy--)
            *buffer++ = digits[0];
        }
        else {
          while(++exp)
            *buffer++ = digits[0];
          buffer = generate(buffer, accuracy,
            r,
            s,
            sa,
            m1,
            m0,
            &exp,
            0,
            B, digits, isLow, isHigh);
          accuracy = precision + 1 - (buffer - p_at);
          while(accuracy--)
            *buffer++ = digits[0];
        }
      }
      else {
        buffer = generate(buffer, accuracy,
          r,
          s,
          sa,
          m1,
          m0,
          &exp,
          1,
          B, digits, isLow, isHigh);
        p_at = start + exp + 1;
        if(buffer <= p_at) {
          while(buffer < p_at)
            *buffer++ = digits[0];
          if(precision > 0) {
            *buffer++ = digits[FRAC_SEP];
            accuracy = precision;
            while(accuracy--)
              *buffer++ = digits[0];
          }    
        } else {
          accuracy = (int)(buffer - p_at);
          if(accuracy > 0) {
            memmove(p_at + 1, p_at, accuracy);
            *p_at = digits[FRAC_SEP];
            accuracy = precision - (buffer - p_at);
            buffer++;
            while(accuracy--)
              *buffer++ = digits[0];
          }
        }
      }
      exp = 0;
      break;
  }
  if ((format=='e')||(format=='E')||(exp != 0)) {
    *buffer++ = digits[EXP_CHAR];
    if(exp < 0) {
      exp = -exp;
      *buffer++ = '-';
    }
    else {
      if(format!='p')
        *buffer++ = '+';
    }
    // exponent has 2, or 3, digits
    /*
    if ( exp < B ) {
      *buffer++ = digits[exp];
    } else
    */
    if ( exp < B*B ){
      *buffer++ = digits[exp/B];
      *buffer++ = digits[exp%B];
    } else {
      *buffer++ = digits[exp/(B*B)];
      exp %= B*B;
      *buffer++ = digits[exp/B];
      *buffer++ = digits[exp%B];
    }
  }
  return buffer;
}

char * float_to_string(
        char* buffer,
        int precision,
        char format,

        double x,
        int base)
{
  unsigned long long *r = (unsigned long long *)&x;
  //double m0, m1, t;
  int be = ( ( *r & EXP_MASK ) >> SHR_EXP );
  unsigned long long f = ( *r & SIG_MASK );
  unsigned long long s = ( *r & SIGN_MASK );
  int e = be - 1075;
  int okLow;
  int okHigh;
  char *digits;

  /* to avoid a confussion between the 14th digit 'e' and the exponent 'e',
  whe use lower case digits when the exponent qualifier is upper case, and
  viceverse */

  if(format=='E'||format=='G') {
    digits = digits_lower;
  } else {
    digits = digits_upper;
  }

  if ( be == 2047 ) {
    if ( f == 0 ) {
      if( s )
        *buffer++ = '-';
    /*else
        buffer[(*pos)++] = '+';*/
    /*
      *buffer++ = digits_upper[0];
      *buffer++ = digits[FRAC_SEP];
      *buffer++ = 'I';
      *buffer++ = 'n';
      *buffer++ = 'f';
      */
      // 1.0e1000
      *buffer++ = digits[1];
      *buffer++ = digits[FRAC_SEP];
      *buffer++ = digits[0];
      *buffer++ = digits[EXP_CHAR];
      *buffer++ = digits[1];
      *buffer++ = digits[0];
      *buffer++ = digits[0];
      *buffer++ = digits[0];
    }
    else {
      *buffer++ = digits[0];
      *buffer++ = digits[FRAC_SEP];
      *buffer++ = 'N';
      *buffer++ = 'a';
      *buffer++ = 'n';
    }
  } else {
    if ( s )
      *buffer++ = '-';
    if ( be == 0 && f == 0 ) {
      buffer = scale_zero(buffer, precision, format, digits);
    }
    else
    {
      if ( 1 <= be && be <= 2046 ) {
        f |= 0x0010000000000000LL;
        e = be - 1075;
      }
      else if ( be == 0 ) {
        //int l = countLeadingZeros64(f) - 11;
        //e = -(l+1074);
        //f <<= l;
        e = -1074;
      }
      //int d = 1;
      okLow = !(f & 1);
      okHigh = !(f & 1);
      if ( e >= 0 ) {
        if ( f != HIDD_BIT ) {
          buffer = scale(buffer, precision, format, f, 1, 1, 1, base, digits, okLow, okHigh, e);
          //scale(buffer, pos, precision, f<<d, 1<<d, 1, 1, B, okLow, okHigh, f, e);
        }
        else {
          buffer = scale(buffer, precision, format, f, 2, 2, 1, base, digits, okLow, okHigh, e);
          //scale(buffer, pos, precision, f<<(d+1), 1<<(d+1), 2, 1, B, okLow, okHigh, f, e);
        }
      }
      else {
        if (be == 0 || f != HIDD_BIT) {
          buffer = scale(buffer, precision, format, f, 1, 1, 1, base, digits, okLow, okHigh, e);
          //scale(buffer, pos, precision, f<<d, 1<<d, 1, 1, B, okLow, okHigh, f, e);
        } else {
          buffer = scale(buffer, precision, format, f, 2, 2, 1, base, digits, okLow, okHigh, e);
          //scale(buffer, pos, precision, f<<(d+1), 1<<(d+1), 2, 1, B, okLow, okHigh, f, e);
        }
      }
    }
  }
  *buffer++ = '\0';
  return buffer;
}

/***************************************************************************
                          float_tostr.h  -  description
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

#ifndef FLOAT_TOSTR_H
#define FLOAT_TOSTR_H

#include "float_consts.h"

char * float_to_string(char* buffer, int precision, char format, double x, int B);

#endif /*FLOAT_TOSTR_H*/

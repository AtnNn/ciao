/*
** glue_map.h
**
** Made by Edison Mera
** Login   <edison@vaioedison>
**
** Started on  Sun Mar 26 12:19:00 2007 Edison Mera
** Last update Fri Mar 30 00:09:43 2007 Edison Mera
*/

#ifndef _glue_map_H
#define	_glue_map_H

# define USE_HASH_MAP

# if defined(USE_HASH_MAP)

#  include "glue_hash.h"
#  define glue_map hash_map
#  define glue_set hash_set

# else

#  include <map>
#  include <set>

#  define glue_map map
#  define glue_set set

# endif

#endif	/* _glue_map_H */


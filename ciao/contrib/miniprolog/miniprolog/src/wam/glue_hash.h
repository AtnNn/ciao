/*
** glue_hash.h
**
** Made by Edison Mera
** Login   <edison@vaioedison>
**
** Started on  Fri Oct 12 15:55:31 2007 Edison Mera
** Last update Fri Oct 12 15:55:31 2007 Edison Mera
*/

#ifndef 	_glue_hash_H_
# define	_glue_hash_H_

# include <string>

# if defined(__GNUC__)

#  include <ext/hash_map>
#  define __STL_EXTRA__ __gnu_cxx

# else

#  include <hash_map>
#  include <xhash>

#  if defined(_MSC_VER)
#   define __STL_EXTRA__ stdext
#  elif defined(__BORLANDC__)
#   define __STL_EXTRA__ stlport
#  endif

# endif

# if defined(__GNUC__)
#  define _STD_BEGIN namespace __STL_EXTRA__ {
#  define _STD_END   }
# endif

_STD_BEGIN

# if defined(__GNUC__)

inline size_t hash_value(const char *s) {
  return __stl_hash_string(s);
}

template <typename T>
struct hash<T *> {
  size_t
  operator()(const T *__x) const
  { return (size_t)__x; }
};

# endif

_STD_END

#  define hash_map __STL_EXTRA__::hash_map

using namespace __STL_EXTRA__;

#endif // _glue_hash_H_

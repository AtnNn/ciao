#ifndef _HashString_H_
#define _HashString_H_

#include "glue_map.h"

using namespace std;

#ifdef USE_HASH_MAP

using namespace std;

# if defined(__BORLANDC__)
_STD_BEGIN
# else
namespace __STL_EXTRA__ {
# endif

# if defined(__GNUC__)
  inline size_t hash_value(const string &s) {
    return __stl_hash_string(s.c_str());
  }
# endif

# if defined(__BORLANDC__) || defined(__GNUC__)

  template<>
    struct hash<string> {
    size_t
      operator()(const string &__s) const
    { return hash_value(__s); }
  };

# endif

# if defined(__BORLANDC__)
  _STD_END
# else
    };
# endif
  
#endif

#endif // _HashString_H_

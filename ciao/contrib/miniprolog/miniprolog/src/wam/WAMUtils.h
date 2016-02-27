#ifndef  _WAMUtils_H_
# define _WAMUtils_H_


# include <string>
# include <iostream>
# include <sstream>
/* # include <vector> */

# include "Settings.h"
# include "hrtime.h"
# include "types.h"
using namespace std;

namespace wam {

  
# ifdef DEBUG
  
#  define DO_DEBUG(X, Y) DO_DEBUG_##Y(X)
  
#  if 0 <= DEBUG_LEVEL
#   define DO_DEBUG_0(X) X
#  else
#   define DO_DEBUG_0(X)
#  endif
#  if 1 <= DEBUG_LEVEL
#   define DO_DEBUG_1(X) X
#  else
#   define DO_DEBUG_1(X)
#  endif
  
#  if 2 <= DEBUG_LEVEL
#   define DO_DEBUG_2(X) X
#  else
#   define DO_DEBUG_2(X)
#  endif
  
#  if 3 <= DEBUG_LEVEL
#   define DO_DEBUG_3(X) X
#  else
#   define DO_DEBUG_3(X)
#  endif
  
#  if 4 <= DEBUG_LEVEL
#   define DO_DEBUG_4(X) X
#  else
#   define DO_DEBUG_4(X)
#  endif
  
  //# define DO_DEBUG(X, V) {if((V)<(DEBUG_LEVEL)){X;}}
  
# else
#  define DO_DEBUG(X, V)
# endif
  
  string trim(const string &s);
  int parseInt(const string &s);

  extern CharPtr * vpaths;
  
  
  template<typename T>
    string toString(const T i)
    {
      ostringstream soutput;
      soutput << i;
      return soutput.str();
    };
  string readLn(FILE *finput);
  bool isInt(const string &s);

  template<typename T>
    void parse(const string &s, T &i) {
    istringstream sinput(s);
    sinput >> i;
  }

  inline int parseInt(const string &s) {
    int i;
    parse(s, i);
    return i;
  }

  // bool isNumber(const string &s);
  /*  
  template<typename Tp>
    ostream& operator << (ostream& os, const vector<Tp> &s) {
    size_t n = s.size();
    os << "[";
    if (n > 0) {
      os << s[0];
      for (size_t i = 1; i < n; i++) {
	os << ", " << s[i];
      }
    }
    return os << "]";
  };
  
  template<typename Tp>
    static void clearVectorElements(vector<Tp *> &values,
				    size_t ini,
				    size_t end) {
    for (size_t i = ini; i < end; i++)
      delete values[i];
  };
  
  template<typename Tp>
    static void clearVectorElements(vector<Tp *> &values, size_t ini = 0) {
    clearVectorElements(values, ini, values.size());
  };
  
  template<typename Tp>
    static void clearVector(vector<Tp *> &values) {
    clearVectorElements(values);
    values.clear();
  };
  
  template<typename Tp>
    static void resizeVector(vector<Tp *> &values, size_t newSize) {
    clearVectorElements(values, newSize);
    values.resize(newSize);
  };
  */
};

#endif // _WAMUtils_H_

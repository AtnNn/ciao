#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "WAMUtils.h"
#include "hrtime.h"

using namespace std;

namespace wam{

  CharPtr * vpaths;
  
  string trim(const string &s) {
    size_t start, end, size;
    size=s.size();
    if(size==0) return s;
    for(start = 0; (start<size) && (s.at(start)<=' '); start++);
    for(end = size - 1; end<size && end>=start && s.at(end)<=' '; end--);
    if(start==0 && end==s.size()-1) return s;
    return s.substr(start, end+1-start);
  }
  
  bool isInt(const string &s) {
    size_t i = 0, n = s.size();
    if (n == 0) {
      return false;
    }
    while (i < n) {
      if (!isdigit(s[i])) {
        break;
      }
      i++;
    }
    return i == n;
  };
  
}

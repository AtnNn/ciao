#ifndef _ByteCode_H_
#define _ByteCode_H_

#include <iostream>

#include "Configure.h"

using namespace std;

namespace wam {
  
  enum BuiltinCode {
    callCall=0
  };
  
  const char * byteCodeToFunction(ByteCode op);
  
  ByteCode functionToByteCode(const char *function);

  inline ostream &operator << (ostream &os, const ByteCode &bc) {
    return os << (int)bc;
  }
  inline istream &operator >> (istream &in, ByteCode &bc) {
    int tmp;
    in >> tmp;
    bc = (ByteCode)tmp;
    return in;
  }
  
}

#endif // _ByteCode_H_

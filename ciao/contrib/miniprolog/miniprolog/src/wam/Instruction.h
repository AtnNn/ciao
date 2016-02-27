/*
** Instruction.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Tue Oct 23 17:34:45 2007 Edison Mera
** Last update Sun Dec 27 21:03:45 2009 Edison Mera
*/

#ifndef   	_Instruction_H_
#define   	_Instruction_H_

#include "ByteCode.h"
#include <string.h>

using namespace std;

namespace wam {
  
# define __Execute_Args__ size_t &programCounter, Engine *w
  
  class Engine;
  
  size_t byteCodeToSize(ByteCode op);
  
  // Instruction class implements WAM code statements
  // a statement looks like this:
  // [label:] operator operand1 [operand2 [operand3]]
  // label, op2 and op3 may be omitted. label is needed for jumps (calls)
  
  class Instruction {
  private:
  public:
    ByteCode byteCode; // same as function, but as integer (performance!)
    
    // creates a new statement with one operand/argument
  Instruction(ByteCode byteCode) : byteCode(byteCode) {};
    virtual ~Instruction() {};
    // returns the operator string, e.g. "get_variable"
    const char * getFunction() const {
      return byteCodeToFunction(byteCode);
    };
    
    void setFunction(const char *newFunction) {
      byteCode = functionToByteCode(newFunction);
    };
    
    size_t getSize();
    
    // getParameter returns a parameter that could be required for the
    // profiler to improve the precision of the estimated execution
    // time, it represents possible different types, machine states,
    // etc. to be considered
    virtual size_t getParameter(Engine *w) const {
      return 0;
    }
    void incProgramPointer(size_t &p) const {
      p += byteCodeToSize(byteCode);
    }
    
    virtual ostream & outArgs(ostream &os) const = 0;
    
    // for code dumping: print the statement: "label: operator op1 op2"
    friend ostream& operator << (ostream& os, const Instruction &s) {
      os << s.getFunction();
      s.outArgs(os);
      return os;
    }

    virtual ostream& outInst(ostream& os, Engine *) {
      return os << *this;
    };

  };
  
/*   static inline string statementToResourceName(Instruction *s) { */
/*     string result = ""; */
/*     if (s==NULL) */
/*       return "\"null\""; */
/*     result = string("\"") + byteCodeToFunction(s->byteCode) + "\""; */
/*     return result; */
/*   } */
  
};

#endif 	    // _Instruction_H_

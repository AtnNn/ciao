/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * Program.java contains the WAM program management class Program. A Program
 * consists of an array of Instructions (cf. Instruction.java).
 ******************************************************************************/


#ifndef _Program_H_
#define _Program_H_

#include "Instruction.h"
#include "ByteCode.h"
#include "Functions.h"
#include "MemSegment.h"

#include <iostream>

#include "ClauseKey.h"

using namespace std;

//inline void* operator new(std::size_t s, void* &__p) throw() { void
//*q = __p; __p = (char *)__p + s; return q; }

using namespace std;

namespace wam {
  
  class WAM;
  
  // Program class manages WAM programs, consisting of list (vector)
  // of instructions
  
  // If ClauseKey is a function, then labels contains the pointer to
  // such function
  
  class Program : public MemSegment {
    
  public:
    typedef map<ClauseKey, size_t> labels_type;
    
    labels_type labels;
    labels_type functions;
    labels_type builtins;
    
    Program(void *memory);
    
    ~Program() {
    };
    
    void clear() {
      MemSegment::clear();
      labels.clear();
    }
    
    Program(Engine *anOwner);
    
    void incProgramPointer(size_t &p) const {
      ((Instruction *)getAddress(p))->incProgramPointer(p);
    }
    
    void addProgram(Program &p);
    
    ClauseKey getLabel(size_t s) {
      for (labels_type::iterator p = labels.begin(); p != labels.end(); p++)
	if (p->second==s)
	  return p->first;
      return ClauseKey("", 0, 1);
    }
    
    ostream& dumpWAMCode(ostream& os);
    ostream& dumpIntCode(ostream& os);
    
    labels_type::iterator locateLabel(size_t s) {
      for (labels_type::iterator p = labels.begin(); p != labels.end(); p++)
	if (p->second==s)
	  return p;
      return labels.end();
    }
    
    void setLabel(const size_t s, const ClauseKey &label) {
      labels[label] = s;
    }
    
    void reserveSpace(size_t position, size_t s) {
      memmove(getAddress(position + s), getAddress(position),
	      getTop() - position);
      incTop(s);
    }
    /*        
	      void addInstruction(Instruction *s) {
	      instructions.push_back(s);
	      };
	      void addInstruction(const string& l, Instruction *s) {
	      statementLabels[s] = l;
	      instructions.push_back(s);
	      }
	      void addInstructionAtPosition(Instruction *s, int position) {
	      instructions.insert(instructions.begin() + position, s);
	      };
    */
    
    Instruction *getInstruction(size_t offset) const {
      return (Instruction *)getAddress(offset);
    };
    
    void deleteFromLine(size_t lineNumber);
    
    void deleteFrom(const ClauseKey &label) {
      deleteFromLine(getLabelIndex(label));
    };
    
    // size_t getLastClauseOf(const string &procedureName);
    
    ostream& outputLabel(ostream &os, Instruction *p);
    ostream& outputInstruction(ostream &os, Instruction *p) {
      outputLabel(os, p);
      return os << *p;
    }
    
    // size_t getLastClauseButOneOf(const string &procedureName);
    
    //        void addClause(const string &label, Program &code);
    
    size_t getLabelIndex(const ClauseKey &label);
    
    // updateLabels converts String label names in call, try_me_else
    // and retry_me_else instructions
    // to integer values. internal predicates (e.g. write, consult)
    // are transformed to negative line numbers
    void updateLabels();
    
  };
  
}

#endif // _Program_H_

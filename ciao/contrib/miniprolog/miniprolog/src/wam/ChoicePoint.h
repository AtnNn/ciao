/*
** ChoicePoint.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Nov  1 22:57:37 2007 Edison Mera
** Last update Thu Nov  1 22:57:37 2007 Edison Mera
*/

#ifndef  _ChoicePoint_H_
# define _ChoicePoint_H_

# ifdef USE_GC
#  include "gc.h"
# endif

#include "Environment.h"

using namespace std;

namespace wam {
  class ChoicePoint;
}

/* template <> */
/* void GCPtr<wam::ChoicePoint, 0>::justFree(wam::ChoicePoint *t); */

namespace wam {

  // class ChoicePoint implements the choice point concept, as
  // presented by Ait-Kaci
  class ChoicePoint {
    size_t numArgs;
  public:
    EnvironmentPtr lastEnvironment;  // E  - current environment
    size_t returnAddress;            // CP - current continuation pointer (cp)
    ChoicePointPtr lastCP;           // B  - last ChoicePoint on stack
    int nextClause;                  // L  - current instruction pointer + 1
    size_t trailPointer;             // TR - current trail pointer
    size_t heapPointer;              // H  - current Heap pointer
    size_t lastSize;
    ChoicePointPtr cutPoint;         // B0 - copy of B0

    static const char *getClassName() { return "ChoicePoint"; };

    VariableWPtr getArguments() const {
      return const_cast<Variable *>((Variable *)(this + 1));
    };

    Variable & getArg(size_t index) const {
      return getArguments()[index];
    }

    size_t getNumArgs() const { return numArgs; };
    
    // constructor gets A (argument variables vector), trailPtr
    // (trail pointer) and anAddress (current return address /
    // continuation pointer)
  ChoicePoint(const Variable a[], size_t aNumArgs, size_t trailPtr,
	      size_t heapPtr, size_t anAddress)
    : numArgs(aNumArgs), lastEnvironment(NULL), returnAddress(anAddress),
      lastCP(NULL), trailPointer(trailPtr), heapPointer(heapPtr)
      {
	DO_DEBUG(cerr << "*** calling constructor of ChoicePoint "
		 << (void *)this << " ***" << endl, 3);
	for (size_t i = 0; i < numArgs; i++)
	  new (getArguments() + i) Variable(a[i]);
      }

    ~ChoicePoint() {
      lastEnvironment = NULL;
      lastCP = NULL;
      cutPoint = NULL;
#ifdef USE_GC
      for (size_t i = getNumArgs() - 1; i != (size_t)(-1); i--)
	getArg(i).~Variable();
#endif
      DO_DEBUG(cerr << "*** called destructor of ChoicePoint "
	       << (void *)this << " ***" << endl, 3);
    }
    
    friend ostream& operator << (ostream& os, const ChoicePoint &s) {
      os << "ChoicePoint( ";
      for (size_t i = 0; i < s.numArgs; i++)
	os << s.getArg(i) << " ";
      os << ")";
      return os;
    }
    
  }; // end of class ChoicePoint

}

# ifdef USE_GC

template <>
inline void GCPtr<wam::Environment, 0>::justFree(wam::Environment *t)
  {
    DO_DEBUG(cerr << "Using specialized version of justFree for Environment" << endl, 0);
    wam::EnvironmentPtr lastEn = t->lastEnvironment;
    wam::ChoicePointPtr aCP = t->afterCutPoint;
    t->~Environment();
    ::operator delete(t, GCInfo<wam::Environment>::stack);
    aCP = NULL;
    lastEn = NULL;
  };

template <>
inline void GCPtr<wam::ChoicePoint, 0>::justFree(wam::ChoicePoint *t)
  {
    DO_DEBUG(cerr << "Using special version of justFree for ChoicePoint" << endl, 0);
    wam::ChoicePointPtr lastCP = t->lastCP;
    wam::EnvironmentPtr lastEn = t->lastEnvironment;
    wam::ChoicePointPtr cutPoint = t->cutPoint;
    t->~ChoicePoint();
    ::operator delete(t, GCInfo<wam::ChoicePoint>::stack);
    if ((void *)lastCP < (void *)lastEn) {
      cerr << "*** (a) Destroying lastEn before to lastCP" << endl;
      lastEn = NULL;
      lastCP = NULL;
      cerr << "*** (a) Done" << endl;
    }
    else {
      cerr << "*** (b) Destroying lastCP before to lastEn" << endl;
      lastCP = NULL;
      lastEn = NULL;
      cerr << "*** (b) Done" << endl;
    }
    cutPoint = NULL;
  };

# endif

#endif // _ChoicePoint_H_

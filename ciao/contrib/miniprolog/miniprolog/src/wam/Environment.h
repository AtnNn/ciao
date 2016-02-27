/*
** Environment.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Nov  1 22:50:26 2007 Edison Mera
** Last update Thu Nov  1 22:50:26 2007 Edison Mera
*/

#ifndef   	_Environment_H_
# define   	_Environment_H_

# include "Settings.h"
# include "hrtime.h"
# include "WAMUtils.h"
# include "MemSegment.h"

# ifdef USE_GC
#  include "gc.h"
# endif

using namespace std;

namespace wam {

  class Environment;
  class ChoicePoint;
  
# ifdef USE_GC
  typedef GCPtr<Environment> EnvironmentPtr;
# else
  typedef Environment * EnvironmentPtr;
# endif

# ifdef USE_GC
  typedef GCPtr<ChoicePoint> ChoicePointPtr;
# else
  typedef ChoicePoint * ChoicePointPtr;
# endif
  
  // class Environment for storing local variables that must not
  // be overridden
  class Environment {
  private:
    EnvironmentPtr lastEnvironment;
    size_t returnAddress;
    size_t numMaxVariables;
  public:
/*     vector<VariablePtr> variables; */
/*     VariablePtr * variables; */
    size_t getNumMaxVariables() const { return numMaxVariables; };
    size_t getReturnAddress() const { return returnAddress; };
    VariablePtr getVariable(size_t index) const {
      return ((VariablePtr *)(const_cast<Environment *>(this) + 1))[index];
    };
    void setVariable(int index, VariablePtr value) {
      ((VariablePtr *)(this + 1))[index] = value;
    };

    static const char *getClassName() { return "Environment"; };

    EnvironmentPtr getLastEnvironment() const { return lastEnvironment; };
    
    // constructor gets the current return address (continuation
    // pointer) and a pointer to the previous environment on stack
  Environment(size_t anAddress, int numVars, Environment * const anEnv)
    : lastEnvironment(anEnv), returnAddress(anAddress),
      numMaxVariables(numVars)
      {
	DO_DEBUG(cerr << "*** calling constructor of Environment " << " ***"
		 << endl, 3);
      }
    
    ~Environment() {
      DO_DEBUG(cerr << "*** calling destructor of Environment "
	       << (void *)this << " ***" << endl, 3);
/*       delete [] variables; */
      /*
      size_t i = 0;
      while (i < getNumVariables() - 1) {
	if (variables[i]->isStructure()) {
	  size_t n = variables[i]->getArity();
	  delete [] variables[i];
	  i += n;
	} else
	  delete variables[i];
	i++;
      }
      */
    }
    
    friend ostream& operator << (ostream& os, const Environment &s) {
      os << "Environment( ";
      for (size_t i = 0; i < s.getNumMaxVariables(); i++)
	os << s.getVariable(i) << " ";
      os << ")";
      return os;
    }
  }; // end of class Environment
  
}

#endif 	    /* !_Environment_H_ */

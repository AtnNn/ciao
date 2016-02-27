/* ****************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * WAM.java contains the actual WAM and the additional structures ChoicePoint,
 * Environment and Trail
 *****************************************************************************
 */


#ifndef _Engine_H_
# define _Engine_H_

# include <map>
# include "WAMUtils.h"
# include "hrtime.h"
# include "Variable.h"
# include "ChoicePoint.h"
# include "ClauseKey.h"
# include "ByteCode.h"
# include "MemSegment.h"
# include "Settings.h"
# include "Trail.h"

# ifdef USE_GC
#  include "gc.h"
# endif

# include <map>
# include <iostream>

using namespace std;
using namespace wam;

namespace wam {
  class Program;
  class Instruction;
  
  extern bool bProfile;
  void profiler_init(const char *fileName);
  void profiler_end(void);
  
  // class WAM is the core and contains the essential functions of the WAM
  class Engine {
  private:
    void runLoop(size_t &programCounter);
    void runLoopProfiled(size_t &programCounter);
    void runLoopUnprofiled(size_t &programCounter);
    bool executeInstruction(size_t &programCounter, Instruction *s);
    
  public:
    
    istream *ein;
    ostream *eout;
    
# ifdef USE_GC
    typedef GCPtr<Trail> TrailPtr;
# else
    typedef Trail * TrailPtr;
# endif
    
    /****************************** BEGIN SUBCLASSES **********************/
    
    /****************************** END SUBCLASSES ************************/
    DO_DEBUG(int backtrackCount, 0);
    Program *program;         // the program(s) loaded into memory
    bool failed;    // set to true upon an unsuccessful binding operation
    // the WAM's register set
    // We use map because it is a sorted map
    NamedVariables queryVariables;     // query variables, to be
                                       // accessed by Q1, Q2, and so on
    
    // Code Area:
    size_t continuationPointer;        // CP - continuation pointer
    
    // Heap simulation:
    MemSegment heap;                   // H  - Heap
    void *_HB;                         // HB - Previous heap top
    void *stack;                       // Stack
    
    // Stack:
    ChoicePointPtr cutPoint;           // B0 - current choicepoint for cut
				       //      instruction
  private:
    ChoicePointPtr choicePoint;        // B  - last choicepoint on stack
    EnvironmentPtr environment;        // E  - last environment on stack
    
    size_t numArgs;
    
    void restore_cp();
    
  public:
    
    // Trail:
    Trail trail;                       // TR - undo-list (WAM trail)
    size_t maxNumArgs;
    Variable *arguments;               // An - argument registers
    
    Engine(Program *aProgram, void *heap, void *stack, void *trail);
    ~Engine();
    
    size_t getNumArgs() const;
    void setNumArgs(size_t aNumArgs);
    VariableWPtr getArg(size_t index) const;
    ChoicePointPtr getChoicePoint() const;
    EnvironmentPtr getEnvironment() const;
    void setChoicePoint(const ChoicePointPtr cp);
    void setEnvironment(const EnvironmentPtr en);
    VariableWPtr get_y_ref(size_t index);
    VariableWPtr get_a_ref(size_t index);
    
    void do_trail(Variable *v);
    
    const char *moreSolutions();
    void clearQueryVariables();
    void reset();
    
    /******************** BEGIN WAM CODE OPERATIONS ********************/
    
    // WAM code operations are described in Ait Kaci: Warren's
    // Abstract Machine -- A Tutorial Reconstruction
    
    // comparison manages "<", "<=", ">=", ">" and "!="
    void comparison(size_t & programCounter, int s1, int s2, int comparator);
    bool equal2(VariableWPtr v1, VariableWPtr v2);
    bool unify_variable2(VariableWPtr v1, VariableWPtr v2);
    bool unify_list2(VariableWPtr list, VariableWPtr head,
		     VariableWPtr tail);
    bool unify_struc2(VariableWPtr struc, VariableWPtr head,
		      VariableWPtr tail);
    void proceed(size_t &programCounter);
    void profile_end();
    bool profile(size_t &programCounter);
    bool time(size_t &programCounter);
    bool call(size_t &programCounter, size_t target, size_t arity);
    bool call_builtin(size_t &programCounter, BuiltinCode index);
    bool execute(size_t &programCounter, size_t target, size_t arity);
    void is_bound(VariableWPtr v);
    void allocate(int numVars);
    void deallocate();
    bool call_call(size_t & programCounter);
    void cut(size_t variable);
    void backtrack(size_t & programCounter);
    bool get_struct(const char *name, size_t arity,
		    size_t variable);
    bool get_list(size_t head, size_t variable);
    void try_me_else(size_t address);
    void retry_me_else(size_t address);
    void set_variable(size_t v);
    void trust_me();
    
    // not_call performs a negated call by invoking a new WAM process
    // if the new process' execution fails, not_call is successful
    // (backtrack, otherwise)
    //void not_call(int target);
    
    /******************** END WAM CODE OPERATIONS ********************/
    
    /******************** BEGIN INTERNAL PREDICATES ********************/
    
    void load(const char *fileName);
    
    // assert asserts a new clause to the current program
    void myAssert(const char *label, const char *clause);
    
    void removeProgramLines(int fromLine);
    
    // retract undoes an assert action
    // bool callRetract(const string &clauseName);
    
    // calls retract(String) until it returns false
    // void retractall(const string &clauseName);
    
    /******************** END INTERNAL PREDICATES ********************/
    
    
    // showHelp shows a list of the available commands
    void showHelp();
    
    // run starts the actual execution of the program in memory
    void run(size_t &programCounter);
    
    bool bContinue;
    bool bHalt;
    
    // runQuery compiles a query given by s into a WAM program, adds
    // it to the program in memory and jumps to the label "query$",
    // starting the execution
    bool runQuery(const char *s);
    
    /*void derefQueryVariables() {
      for (size_t i = 0; i < queryVariables.size(); i++)
      queryVariables[i] = queryVariables[i]->deref();
      }*/
    
    ostream &os_query_variable(ostream &os,
			       map<const Variable *, const char *> &
			       variableNames,
			       const Variable &s);
  };
}

#ifdef INLINE
#include "EngineImpl.h"
#endif

#endif // _Engine_H_

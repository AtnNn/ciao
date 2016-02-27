/* ****************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * WAM.java contains the actual WAM and the additional structures ChoicePoint,
 * Environment and Trail
 *****************************************************************************
 */

#include <iostream>

#ifdef __BORLANDC__
# pragma hdrstop
# pragma package(smart_init)
#endif

#include "Program.h"
#include "Engine.h"
#include "QueryCompiler.h"
#include "Profiler.h"
#include "Settings.h"
#include "InstructionSpec.h"

using namespace wam;
using namespace std;

#ifndef INLINE
#include "EngineImpl.h"
#endif

#include "EngineLoop.h"

namespace wam {
  
  // class WAM is the core and contains the essential functions of the WAM
  
  Engine::~Engine()
  {
    setNumArgs(0);
    queryVariables.clear();
    trail.clear();
    setEnvironment(NULL);
    setChoicePoint(NULL);
    cutPoint = NULL;
  }
  
  // reset sets all WAM parameters to their initial values
  void Engine::reset() {
    setNumArgs(0);
    _HB = heap.getTopAddress();//(void *)0x7FFFFFFF;
    trail.clear();
    queryVariables.clear();
    // empty environment
    continuationPointer = (size_t)(-1);  // no continuation point
    choicePoint = NULL;
    cutPoint = NULL;
    setEnvironment(EnvironmentPtr(new (stack)
				  Environment((size_t)(-1), 512, NULL)));
  }
  /*
  VariableWPtr Engine::get_ref(vector<VariablePtr> *array,
			       MemSegment &mem,
			       size_t index) {
    size_t cnt = array->size();
    while (cnt++ < index + 1) {
      DO_DEBUG(cerr << "*** allocating new variable " << cnt << " ***"
	       << endl, 3);
      array->push_back(VariablePtr(new (mem) Variable()));
    }
    return array->at(index);
  }
  */
  /******************** BEGIN WAM CODE OPERATIONS ********************/
  
  // WAM code operations are described in Ait Kaci: Warren's Abstract
  // Machine -- A Tutorial Reconstruction
  
  bool Engine::equal2(VariableWPtr v1, VariableWPtr v2) {
    if (v1->tag!=v2->tag)
      return false;
    switch(v1->tag) {
    case ATM:
      return v1->getStructure()==v2->getStructure();
    case INT:
      return v1->getInteger()==v2->getInteger();
    default:
      return false;
    }
  }
  
  bool Engine::unify_variable2(VariableWPtr v1, VariableWPtr v2) {
    if ((v1 == NULL) || (v2 == NULL)) return false;
    v1 = v1->deref();
    v2 = v2->deref();
    if (v1 == v2) return true;
    
    if (v1->isFree() && (!v2->isFree() || v2 < v1)) {
      do_trail(v1);
      v1->copyFrom(v2);
      return true;
    }
    
    if (v2->isFree()) {
      do_trail(v2);
      v2->copyFrom(v1);
      return true;
    }
    
    if (v1->tag!=v2->tag)
      return false;
    
    switch(v1->tag) {
    case INT:
      return v1->getInteger()==v2->getInteger();
    case LIS:
      return unify_variable2(v1->getHead(), v2->getHead())
	&& unify_variable2(v1->getTail(), v2->getTail());
    case STR: case ATM:
      if (v1->getStructure()==v2->getStructure()) {
	for (size_t i = 0; i < v1->getArity(); i++)
	  if (!unify_variable2(v1->getArg(i), v2->getArg(i)))
	    return false;
	return true;
      }
    default:
      return false;
    }
  }
  
  bool Engine::unify_list2(VariableWPtr list, VariableWPtr head,
			VariableWPtr tail)
  {
    list = list->deref();
    head = head->deref();
    tail = tail->deref();
    if (list->isFree()) {
      do_trail(list);
      list->tag = LIS;
      list->setHead(head);
      list->setTail(tail);
      return true;
    }
    return list->isList()
      && unify_variable2(head, list->getHead())
      && unify_variable2(tail, list->getTail());
  }
  
  // showHelp shows a list of the available commands
  void Engine::showHelp() {
    cout << "*** This Help is obsolete now ***" << endl;
    cout << "This is Stu's mighty WAM speaking. Need some help?" << endl;
    cout << "" << endl;
    cout << "Available commands:" << endl;
    for (Program::labels_type::iterator p = program->functions.begin();
	 p != program->functions.end(); p++)
      cout << p->first.name << '/' << p->first.arity << endl;
  };
  
  // run starts the actual execution of the program in memory
  void Engine::run(size_t &programCounter) {
    
    failed = true;
    runLoop(programCounter);
    
    if (failed) {
      while (choicePoint != NULL)
	backtrack(programCounter);
    }
    DO_DEBUG(cout << "# backtracks: " << backtrackCount << endl, 0);
  };
  
  void Engine::runLoop(size_t &programCounter) {
    DO_DEBUG(backtrackCount = 0, 0);
    bContinue = true;
    bHalt = false;
    
    while (programCounter != (size_t)(-1)) {
      if (bProfile) {
	runLoopProfiled(programCounter);
      }
      else {
	runLoopUnprofiled(programCounter);
      }
      if (!bContinue)
	return;
    }
    if (programCounter==(size_t)(-1)) {
      failed = true;
    }
    return;
  }
  
  void Engine::runLoopProfiled(size_t &programCounter) {
    while (programCounter != (size_t)(-1)) {
      Instruction *s = program->getInstruction(programCounter);
      putInst(s);
      failed = false;
      executeInstruction(programCounter, s);
    }
  }
  
  void Engine::runLoopUnprofiled(size_t &programCounter) {
    while (programCounter != (size_t)(-1)) {
      Instruction *s = program->getInstruction(programCounter);
      failed = false;
      executeInstruction(programCounter, s);
    }
  }
  
  // runQuery compiles a query given by s into a WAM program, adds it
  // to the program in memory and jumps to the label "query$",
  // starting the execution
  bool Engine::runQuery(const char *ss) {
    QueryCompiler qc(this);
    reset();
    program->deleteFrom(ClauseKey("query$",0,1));
    string s = trim(ss);
    size_t programCounter;
    if (s.size()==0)
      return true;
    
    /*************** BEGIN SPECIAL COMMANDS ***************/
    
    //    if (s=="clear") {
    //        wamOutput->clear();
    //        return true;
    //    }
    if (s=="new." || s=="clear.") {  // clear memory
      program->clear();
      cout << "Memory cleared." << endl;
      return true;
    }
    /*************** END SPECIAL COMMANDS ***************/
    
    Program query(program->getTopAddress());
    
    if (!qc.compile(s, query)) {  // query could not be compiled
      cout << "Illegal query." << endl;
      return true;
    } else {
      /*
      DO_DEBUG({  // if in debug mode, display query WAM code
	  cout << "----- BEGIN QUERYCODE -----" << endl;
	  cout << query << endl;
	  cout << "------ END QUERYCODE ------" << endl;
	}, 1);
      */
      program->addProgram(query);  // add query to program in memory and
      program->updateLabels();  // update the labels for jumping hin und her
    }
    // reset the WAM's registers and jump to label "query$" (the
    // current query, of course)
    programCounter = program->getLabelIndex(ClauseKey("query$", 0, 1));
    string answer = "";
    do {
      DO_DEBUG(uint64 ms = hrtime(), 3);
      run(programCounter);
      if (bHalt)
	return false;
      DO_DEBUG(cout << "Total time elapsed: " <<
	       (hrtime() - ms) << " ms." << endl << endl, 3);
      if (failed) {  // if execution failed, just tell that
        cout << "no" << endl;
        break;
      }
      
      // if there are any query variables (e.g. in "start(X, Y)", X
      // and Y would be such variables), display their current values
      // and ask the user if he/she wants to see more possible
      // solutions
      VariableNames variableNames;
      for (NamedVariables::iterator p = queryVariables.begin();
	   p != queryVariables.end(); p++) {
	if (!p->second->isFree()) {
	  string name = p->first;
	  if (name[0] != '_') {
	    cout << name << " = ";
	    os_query_variable(cout, variableNames, *p->second);
	    cout << endl;
	  }
	}
      }
      cleanVariableNames(variableNames);
      // if there are any more choicepoints left, ask the user if they
      // shall be tried
      if (choicePoint != NULL) {
	answer = trim(moreSolutions());
      } else {
	cout << "yes" << endl;
	break;
      }
      // if the users decided to see more, show him/her. otherwise:
      // terminate
      if ((answer==";"))
        backtrack(programCounter);
    } while ((answer==";"));
    reset();
    return true;
  };
  
  const char *Engine::moreSolutions() {
    static char buf[32];
    cout << "? " << flush;
    fgets(buf, 32, stdin);
    cout << endl;
    return buf;
  };

  inline bool Engine::call_call(size_t & programCounter) {
    Variable *v = getArg(0)->deref();
    size_t target = (size_t)(-1);
    ClauseKey key(v->getString(), v->getArity(), 1);
    bool foundKeyInLabels
      = program->labels.find(key)!=program->labels.end();
    bool foundKeyInBuiltins
      = program->builtins.find(key)!=program->builtins.end();
    bool foundKeyInFunctions
      = program->functions.find(key)!=program->functions.end();
    if (foundKeyInLabels)
      target = program->labels[key];
    else if (foundKeyInBuiltins)
      target = program->builtins[key];
    else if (foundKeyInFunctions)
      target = program->functions[key];
    if (foundKeyInLabels || foundKeyInBuiltins || foundKeyInFunctions) {
      if (v->isStructure()) {
	size_t cnt;
	for (cnt = 0; cnt < v->getArity(); cnt++) {
	  get_a_ref(cnt)->setReference(v->getArg(cnt));
	}
      }
    }
    if (foundKeyInLabels) {
      return call(programCounter, (int)target, v->getArity());
    }
    else if (foundKeyInBuiltins) {
      return call_builtin(programCounter, (BuiltinCode)target);
    }
    else if (foundKeyInFunctions) {
      return ((bool (*)(Engine *))target)(this);
    }
    else
      return false;
  }
  
  enum KeyLocation {
    klLabels,
    klBuiltins,
    klFunctions
  };

  bool Engine::profile(size_t & programCounter) {
    Variable *file = getArg(0)->deref();
    if (!file->isAtom())
      return false;
    const char *fileName = file->getString();
    KeyLocation kl;
    Variable *v = getArg(1)->deref();
    size_t target = (size_t)(-1);
    ClauseKey key(v->getString(), v->getArity(), 1);
    if (program->labels.find(key)!=program->labels.end())
      kl = klLabels;
    else if (program->builtins.find(key)!=program->builtins.end())
      kl = klBuiltins;
    else if (program->functions.find(key)!=program->functions.end())
      kl = klFunctions;
    else
      return false;
    switch (kl) {
    case klLabels:
      target = program->labels[key];
      break;
    case klBuiltins:
      target = program->builtins[key];
      break;
    case klFunctions:
      target = program->functions[key];
    }
    if (v->isStructure()) {
      size_t cnt;
      for (cnt = 0; cnt < v->getArity(); cnt++) {
	get_a_ref(cnt)->setReference(v->getArg(cnt));
      }
    }
    profiler_init(fileName);
    switch (kl) {
    case klLabels:
      call(programCounter, (int)target, v->getArity());
      return true;
    case klBuiltins:
      return call_builtin(programCounter, (BuiltinCode)target);
    case klFunctions:
      return ((bool (*)(Engine *))target)(this);
    }
    return false;
  }
  
  bool Engine::time(size_t & programCounter) {
    KeyLocation kl;
    Variable *v = getArg(1)->deref();
    size_t target = (size_t)(-1);
    ClauseKey key(v->getString(), v->getArity(), 1);
    if (program->labels.find(key)!=program->labels.end())
      kl = klLabels;
    else if (program->builtins.find(key)!=program->builtins.end())
      kl = klBuiltins;
    else if (program->functions.find(key)!=program->functions.end())
      kl = klFunctions;
    else
      return false;
    switch (kl) {
    case klLabels:
      target = program->labels[key];
      break;
    case klBuiltins:
      target = program->builtins[key];
      break;
    case klFunctions:
      target = program->functions[key];
    }
    if (v->isStructure()) {
      size_t cnt;
      for (cnt = 0; cnt < v->getArity(); cnt++) {
	get_a_ref(cnt)->setReference(v->getArg(cnt));
      }
    }
    timeInit = hrtime();
    switch (kl) {
    case klLabels:
      call(programCounter, (int)target, v->getArity());
      return true;
    case klBuiltins:
      return call_builtin(programCounter, (BuiltinCode)target);
    case klFunctions:
      return ((bool (*)(Engine *))target)(this);
    }
    return false;
  }
}

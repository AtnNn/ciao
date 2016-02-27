/*
** EngineImpl.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Nov 15 18:59:08 2007 Edison Mera
** Last update Thu Nov 15 18:59:08 2007 Edison Mera
*/

#ifndef   	_EngineImpl_H_
# define   	_EngineImpl_H_

#include "Engine.h"
#include <stdlib.h>

namespace wam {
  
  // creates a new WAM with program data initialized to aProgram
  _INLINE Engine::Engine(Program *aProgram, void *heap, void *stack,
			 void *trail)
    : program(aProgram), heap(heap), stack(stack), numArgs(0), trail(trail),
    maxNumArgs(2048), arguments(NULL) {
    ein  = &cin;
    eout = &cout;
    arguments = (Variable *)malloc(sizeof(Variable)*maxNumArgs);
    reset();
  };
  
  _INLINE size_t Engine::getNumArgs() const {
    return numArgs;
  };
  
  _INLINE void Engine::setNumArgs(size_t aNumArgs) {
    if (aNumArgs >= maxNumArgs) {
      arguments = (Variable *)realloc((void *)arguments,
				      sizeof(Variable)*aNumArgs);
      maxNumArgs = aNumArgs;
    }
    numArgs = aNumArgs;
  };
  
  _INLINE VariableWPtr Engine::getArg(size_t index) const {
    return const_cast<Variable *>(arguments + index);
  };
  
  _INLINE ChoicePointPtr Engine::getChoicePoint() const {
    return choicePoint;
  };
  
  _INLINE EnvironmentPtr Engine::getEnvironment() const {
    return environment;
  };
  
  _INLINE void Engine::setChoicePoint(const ChoicePointPtr cp) {
    choicePoint = cp;
  };
  
  _INLINE void Engine::setEnvironment(const EnvironmentPtr en) {
    environment = en;
  };
  
  _INLINE VariableWPtr Engine::get_y_ref(size_t index) {
    return environment->getVariable(index);
  };
  
  _INLINE void Engine::set_variable(size_t v) {
    environment->setVariable(v, VariablePtr(new (heap) Variable()));
  }
  
  _INLINE VariableWPtr Engine::get_a_ref(size_t index) {
    return getArg(index);
  };
  
  _INLINE void Engine::do_trail(Variable *v) {
    if (v < _HB || heap.getTopAddress() < v)
      trail.addEntry(v);
  }
  
  _INLINE void Engine::profile_end() {
    profiler_end();
  };
  
  _INLINE bool Engine::call(size_t &programCounter, size_t target,
			    size_t arity) {
    if (target == (size_t)(-1))
      return false;
    continuationPointer = programCounter;
    cutPoint = choicePoint;
    setNumArgs(arity);
    programCounter = target;
    return true;
  };
  
  _INLINE bool Engine::call_builtin(size_t &programCounter, BuiltinCode index) {
    switch(index) {
    case callCall:
      return call_call(programCounter);
    default:
      return false;
    }
  };
  
  _INLINE bool Engine::execute(size_t &programCounter, size_t target,
			       size_t arity) {
    if (target == (size_t)(-1))
      return false;
    cutPoint = choicePoint;
    setNumArgs(arity);
    programCounter = target;
    return true;
  };
  
  _INLINE void Engine::allocate(int numVars) {
    void *newE;
    if ((void *)environment > (void *)choicePoint)
      newE = (void *)((VariablePtr *)(environment + 1)
		      + environment->getNumMaxVariables());
    else
      newE = (void *)((VariablePtr)(choicePoint + 1)
		      + choicePoint->getNumArgs());
    environment = new (newE)
      Environment(continuationPointer, numVars, environment);
  };
  
  _INLINE void Engine::deallocate() {
    continuationPointer = environment->getReturnAddress();
    setEnvironment(environment->getLastEnvironment());
  };
  
  _INLINE void Engine::proceed(size_t &programCounter) {
    programCounter = continuationPointer;
  };
  
  _INLINE void Engine::cut(size_t variable) {
    ChoicePointPtr cutLevel = get_y_ref(variable)->getCutLevel();
    /*       if (choicePoint > cutLevel) { */
# ifdef USE_GC
    while (choicePoint > cutLevel) {
      ChoicePoint *cp = choicePoint->lastCP;
      choicePoint->~ChoicePoint();
      choicePoint = cp;
    }
# else
    choicePoint = cutLevel;
# endif
    /*       tidy_trail; } */
  };
  
  _INLINE void Engine::backtrack(size_t & programCounter)
  {
    DO_DEBUG(cerr << "-> backtrack" << endl, 0);
    DO_DEBUG(backtrackCount++, 0);
    failed = true;
    if (choicePoint != NULL) {
      cutPoint = choicePoint->cutPoint;
      programCounter = choicePoint->nextClause;
    } else {
      for (int i = (int)trail.getLength() - 1; i >= 0; i--)
	trail.undo(i);
      trail.setLength(0);
      heap.setTop(0);
      programCounter = (size_t)(-1);
      //	heap.clear();
    }
  };
  
  _INLINE bool Engine::get_struct(const char *name, size_t arity,
				  size_t variable) {
    set_variable(variable);
    Variable * const v = get_y_ref(variable);
    v->setStructure(name, arity);
    return true;
  }
  
  _INLINE bool Engine::get_list(size_t head, size_t variable) {
    Variable * const v = get_y_ref(variable)->deref();
/*     Variable * const v =  new (heap) Variable(); */
/*     environment->variables.push_back(v); */
    Variable * h = get_y_ref(head);
    v->tag = LIS;
    v->setHead(h);
    return true;
  }
  
  _INLINE void Engine::restore_cp() {
    int tp = (int)choicePoint->trailPointer;
    for (int i = (int)trail.getLength() - 1; i >= tp; i--)
      trail.undo(i);
    trail.setLength(tp);
    setNumArgs(choicePoint->getNumArgs());
    for (size_t i = 0; i < choicePoint->getNumArgs(); i++)
      getArg(i)->copyFrom(choicePoint->getArguments() + i);
    continuationPointer = choicePoint->returnAddress;
    setEnvironment(choicePoint->lastEnvironment);
    heap.setTop(choicePoint->heapPointer);
    heap.setLastSize(choicePoint->lastSize);
    _HB = heap.getTopAddress();
  }
  
  _INLINE void Engine::try_me_else(size_t address) {
    void *newB;
    if ((void *)environment > (void *)choicePoint)
      newB = (void *)((VariablePtr *)(environment + 1)
		      + environment->getNumMaxVariables());
    else
      newB = (void *)((VariablePtr)(choicePoint + 1)
		      + choicePoint->getNumArgs());
    ChoicePointPtr cp =
      new (newB) ChoicePoint(arguments,
			     getNumArgs(),
			     trail.getLength(),
			     heap.getTop(),
			     continuationPointer);
    cp->lastSize = heap.getLastSize();
    cp->lastCP = getChoicePoint();
    cp->cutPoint = cutPoint;
    choicePoint = cp;
    cp->nextClause = address;
    cp->lastEnvironment = getEnvironment();
    _HB = heap.getTopAddress();//(void *)0x7FFFFFFF;
  }
  
  _INLINE void Engine::retry_me_else(size_t address) {
    choicePoint->nextClause = address;
    restore_cp();
  };
  
  _INLINE void Engine::trust_me() {
    restore_cp();
# ifdef USE_GC
    ChoicePoint *cp = choicePoint->lastCP;
    choicePoint->~ChoicePoint();
    choicePoint = cp;
# else
    choicePoint = choicePoint->lastCP;
# endif
  };
  
  _INLINE ostream & Engine::os_query_variable(ostream &os,
				     map<const Variable *,
					      const char *> &variableNames,
				     const Variable &s) {
    return os_variable(os, variableNames, s, &queryVariables);
  };
  
}

#endif 	    /* _EngineImpl_H_ */

/*
** InstructionSpecImpl.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Nov 15 20:58:18 2007 Edison Mera
** Last update Sun Dec 27 21:04:31 2009 Edison Mera
*/

#ifndef   	_InstructionSpecImpl_H_
# define   	_InstructionSpecImpl_H_

#include "Profiler.h"
#include "InstructionSpec.h"

namespace wam {  

  _INLINE ostream& Instruction_i_i::outInst(ostream& os, Engine *w) {
    VariableWPtr Vn = w->get_y_ref(getArg1());
    VariableWPtr Ai = w->get_a_ref(getArg2());
    os << getFunction();
    return os << "( " << getArg1() << "=_" << w->heap.getIndex(Vn) << ", "
	      << getArg2() <<"=" << *Ai << " )" << endl;
  };
  
  /*
    _INLINE void InstNotCall::execute(__Execute_Args__) {
    int target = getArg1();
    w->not_call(target);
    };
  */
  
  _INLINE void InstAllocate::execute(__Execute_Args__) {
    w->allocate(getArg1());
  }
  
  _INLINE bool InstCall::execute(__Execute_Args__) {
    return w->call(programCounter, getCounter(), getArity());
  }
  
  _INLINE bool InstCallBuiltin::execute(__Execute_Args__) {
    return w->call_builtin(programCounter, (BuiltinCode)index);
  }
  
  _INLINE bool InstCallFunction::execute(__Execute_Args__) {
    return getFunction()(w);
  }
  
  _INLINE bool InstExecuteFunction::execute(__Execute_Args__) {
    programCounter = w->continuationPointer;
    return getFunction()(w);
  }
  
  _INLINE void InstCreateQVariable::execute(__Execute_Args__) {
    const char *name = getName();
    if (strcmp(name, "_")) { // keep "_" from being displayed as solution
      Variable *q = w->get_y_ref(getVariable());
      w->queryVariables[name] = q;
    }
  };
  
  _INLINE void InstCut::execute(__Execute_Args__) {
    w->cut(getVariable());
  }
  
  _INLINE void InstDeallocate::execute(__Execute_Args__) {
    w->deallocate();
  }
  
  _INLINE bool InstExecute::execute(__Execute_Args__) {
    return w->execute(programCounter, getCounter(), getArity());
  }
  
  _INLINE bool InstEqual::execute(__Execute_Args__) {
    VariableWPtr v1 = w->get_y_ref(getArg1())->deref();
    VariableWPtr v2 = w->get_y_ref(getArg2())->deref();
    return w->equal2(v1, v2);
  }
  
  _INLINE void InstGetLevel::execute(__Execute_Args__) {
    w->set_variable(getArg1());
    VariableWPtr v = w->get_y_ref(getArg1());
    v->setCutLevel(w->cutPoint);
  }
  
  _INLINE void InstGetStruct::execute(__Execute_Args__) {
    w->get_struct(name, arity, variable);
  };
  
  _INLINE void InstGetList::execute(__Execute_Args__) {
    w->get_list(head, variable);
  };
  
  _INLINE bool InstGetValue::execute(__Execute_Args__) {
    VariableWPtr v2 = w->get_a_ref(getArg2());
    VariableWPtr v1 = w->get_y_ref(getArg1());
    return w->unify_variable2(v2, v1);
  };
  
  _INLINE void InstGetVariable::execute(__Execute_Args__) {
    VariableWPtr Vn;
    VariableWPtr Ai;
    w->set_variable(getArg1());
    Vn = w->get_y_ref(getArg1());
    Ai = w->get_a_ref(getArg2())->deref();
    Vn->copyFrom(Ai);
  }
  
  _INLINE void InstNoOp::execute(__Execute_Args__) {}
  
  _INLINE void InstProceed::execute(__Execute_Args__) {
    w->proceed(programCounter);
  }
  
  _INLINE bool InstProfile::execute(__Execute_Args__) {
    return w->profile(programCounter);
  }
  
  _INLINE bool InstProfileInit::execute(__Execute_Args__) {
    VariableWPtr v = w->getArg(0)->deref();
    if (v->isAtom()) {
      profiler_init(v->getString());
      return true;
    } else
      return false;
  }
  
  _INLINE void InstProfileEnd::execute(__Execute_Args__) {
    w->profile_end();
  }
  
  _INLINE bool InstTime::execute(__Execute_Args__) {
    return w->time(programCounter);
  }
  
  _INLINE void InstTimeInit::execute(__Execute_Args__) {
    timeInit = hrtime();
  }
  
  _INLINE void InstTimeEnd::execute(__Execute_Args__) {
    timeEnd = hrtime();
  }
  
  _INLINE void InstPutValue::execute(__Execute_Args__) {
    VariableWPtr Vi = w->get_y_ref(getArg1())->deref();
    VariableWPtr An = w->get_a_ref(getArg2());
    An->copyFrom(Vi);
  }
  /*
  _INLINE void InstPutVariable::execute(__Execute_Args__) {
    VariableWPtr Vn = w->get_y_ref(getArg1())->deref();
    VariableWPtr Ai = w->get_a_ref(getArg2());
    Ai->setReference(Vn);
  }
  */
  _INLINE void InstRetryMeElse::execute(__Execute_Args__) {
    w->retry_me_else(address);
  };
  
  _INLINE void InstSetVariable::execute(__Execute_Args__) {
    w->set_variable(getArg1());
  };
  
  _INLINE void InstTryMeElse::execute(__Execute_Args__) {
    w->try_me_else(address);
  };
  
  _INLINE void InstTrustMe::execute(__Execute_Args__) {
    w->trust_me();
  }
  
  _INLINE bool InstUnequal::execute(__Execute_Args__) {
    VariableWPtr v1 = w->get_y_ref(getArg1())->deref();
    VariableWPtr v2 = w->get_y_ref(getArg2())->deref();
    return !(w->equal2(v1, v2));
  }
  
  _INLINE bool InstUnifyVariable::execute(__Execute_Args__) {
    VariableWPtr v1 = w->get_y_ref(getArg1());
    VariableWPtr v2 = w->get_y_ref(getArg2());
    return w->unify_variable2(v1, v2);
  }

  _INLINE size_t InstUnifyVariable::getParameter(Engine *w) const {
    VariableWPtr v1 = w->get_y_ref(getArg1())->deref();
    VariableWPtr v2 = w->get_y_ref(getArg2())->deref();
    if (v1->isFree() && (!v2->isFree() || v2 < v1))
      return 0;
    if (v2->isFree())
      return 1;
    if (v1->tag!=v2->tag) // Two different basic types
      return 2;
    switch (v1->tag) {
    case INT:
      return 3;
    case ATM:
      return 5;
    case STR:
      return 6;
    case LIS:
      if (v1->getHead()->deref()->tag==INT /* bug: This part is
					      for lists of length 100 */
	  && v1->getTail()->deref()->tag==LIS
	  && v2->getHead()->deref()->tag==INT
	  && v2->getTail()->deref()->tag==LIS)
	return 7;
      return 4;
    default:
      return 8;
    };
  };
  
#include "InstructionBC.h"
  
}

#endif 	    /* INSTRUCTIONSPECIMPL_H_ */

/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * Instruction.java contains the class Instruction, representing a
 * single line of WAM code, e.g. "true: proceed".
 ******************************************************************************/


#ifndef _InstructionSpec_H_
#define _InstructionSpec_H_

#include "ByteCode.h"
#include "WAMUtils.h"
#include "Engine.h"
#include "Instruction.h"
#include "ClauseKey.h"
#include "ChoicePoint.h"
#include "Atoms.h"

#include <string.h>

using namespace std;

namespace wam {

  template<typename I>
    inline void executeCanFail(I *inst, __Execute_Args__) {
    programCounter += sizeof(I);
    if (!inst->execute(programCounter, w)) {
      w->backtrack(programCounter);
    }
  }
  
  template<typename I>
    inline void executeNotFail(I *inst, __Execute_Args__) {
    programCounter += sizeof(I);
    inst->execute(programCounter, w);
  }
  
  template<typename I>
    inline void controlCanFail(I *inst, __Execute_Args__) {
    if (!inst->execute(programCounter, w)) {
      w->backtrack(programCounter);
    }
  }
  
  template<typename I>
    inline void controlNotFail(I *inst, __Execute_Args__) {
    inst->execute(programCounter, w);
  }
  
  inline string outArg(const char *s) {
    string r = "\'";
    return (r + s) + "\'";
  }
  
  inline int outArg(const int i) {
    return i;
  }
  
  class Instruction_i : public Instruction {
  private:
    int arg1;
  public:
  Instruction_i(ByteCode aOperator, int aArg1)
    : Instruction(aOperator) {
      arg1 = aArg1;
    };
    
    int getArg1() const { return arg1; };
    void setArg1(int aArg1) { arg1 = aArg1; }
    
    virtual ostream &outArgs(ostream &os) const
    { return os << "( " << arg1 << " )"; }
  };
  
  // Instructions with two arguments:
  class Instruction_i_i : public Instruction_i {
  private:
    int arg2;
  public:
  Instruction_i_i(ByteCode aOperator, int aArg1, int aArg2)
    : Instruction_i(aOperator, aArg1), arg2(aArg2) {};
    int getArg2() const { return arg2; };
    void setArg2(int aArg2) {
      arg2 = aArg2;
    }
    
    virtual ostream &outArgs(ostream &os) const
    { return os << "( " << getArg1() << ", " << arg2 << " )"; }
    
    virtual ostream& outInst(ostream& os, Engine *w);
  };
  
  class InstUnknown         : public Instruction {
  public:
  InstUnknown() : Instruction(opUnknown) {};
    void execute(__Execute_Args__) {}
  };
  
  class InstAllocate        : public Instruction_i {
  public:
  InstAllocate(int numVars) : Instruction_i(opAllocate, numVars) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };

  class InstructionLabel: public Instruction {
  public:
  InstructionLabel(ByteCode aOperator) : Instruction(aOperator) {};
    virtual ClauseKey getLabel() const = 0;
    virtual void setAddress(size_t address) = 0;
  };
  
  typedef bool (*FunctionPtr)(Engine *);
  class InstCallBase : public InstructionLabel {
  private:
    union {
      size_t counter;
      FunctionPtr function;
    } jumpto;
    const char * name;
    size_t arity;
  public:
  InstCallBase(ByteCode byteCode, size_t counter,
	       const char *aName, int arity)
    : InstructionLabel(byteCode), arity(arity)
    {	name = Atoms::copyValue(aName); jumpto.counter = counter; };
    
  InstCallBase(ByteCode byteCode, bool (*function)(Engine *),
	       const char *aName, int arity)
    : InstructionLabel(byteCode), arity(arity)
    {	name = Atoms::copyValue(aName); jumpto.function = function; };
    
    const char * getName() const { return name; }
    size_t getArity() const { return arity; }
    virtual ClauseKey getLabel() const {
      return ClauseKey(name, arity, 1);
    }
    size_t getCounter() const { return jumpto.counter; }
    FunctionPtr getFunction() const { return jumpto.function; }
    virtual void setAddress(size_t address) { jumpto.counter = address; }
    void setFunction(FunctionPtr function) { jumpto.function = function; }
  };
  
  class InstCall            : public InstCallBase {
  public:
    
  InstCall(size_t counter, const char *name, int arity)
    :InstCallBase(opCall, counter, name, arity) {};
    
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << getCounter() << ", " << getName()
		<< "/" << getArity() << " )";
    }
  };
  
  class InstExecute         : public InstCallBase {
  public:
    
  InstExecute(size_t counter, const char *name, int arity)
    : InstCallBase(opExecute, counter, name, arity) {};
    
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << getCounter() << ", " << getName()
		<< ", " << getArity() << " )";
    }
  };
  
  class InstExecuteFunction : public InstCallBase {
  public:
    
  InstExecuteFunction(bool (*address)(Engine *), const char *name, int arity)
    : InstCallBase(opExecuteFunction, address, name, arity) {};
    
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << getCounter() << ", " << getName()
		<< ", " << getArity() << " )";
    }
  };
  
  class InstFail : public Instruction {
  public:
    
  InstFail(): Instruction(opFail) {};

    bool execute(__Execute_Args__){ return false; };
    virtual ostream &outArgs(ostream &os) const {
      return os;
    };
  };
  
  class InstCallBuiltin : public Instruction {
    size_t index;
  public:
    
  InstCallBuiltin(size_t index) : Instruction(opCallBuiltin), index(index) {}
    bool execute(__Execute_Args__);
    virtual size_t getParameter(Engine *w) const {
      return index;
    }
    virtual ostream &outArgs(ostream &os) const {
      return os << "( A" << index << " )";
    }
  };
  
  class InstCallFunction     : public InstCallBase {
  public:
  InstCallFunction(bool (*address)(Engine *), const char *name, int arity)
    :InstCallBase(opCallFunction, address, name, arity) {};
    
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << getFunction() << ", " << getName()
		<< ", " << getArity() << " )";
    }
  };
  /*
    class InstNotCall         : public Instruction_i_s_i {
    public:
    InstNotCall(int arg1, const string &arg2, int arg3)
    : Instruction_i_s_i(opNotCall, arg1, arg2, arg3) {};
    void execute(__Execute_Args__);
    };
  */
  class InstCreateQVariable : public Instruction {
    size_t variable;
    const char *name;
  public:
    const char *getName() const { return name; };
    size_t getVariable() const { return variable; };
  InstCreateQVariable(size_t variable, const char *aName)
    : Instruction(opCreateQVariable), variable(variable) {
      name = Atoms::copyValue(aName);
    };
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << variable << ", " << outArg(name) << " )";
    }
  };
  class InstCut             : public Instruction {
    size_t variable;
  public:
    size_t getVariable() const { return variable; };
  InstCut(size_t variable) : Instruction(opCut), variable(variable) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << variable << " )";
    }
  };
  class InstDeallocate      : public Instruction {
  public:
  InstDeallocate() : Instruction(opDeallocate) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  class InstEqual         : public Instruction_i_i {
  public:
  InstEqual(int arg1, int arg2) : Instruction_i_i(opEqual, arg1, arg2) {};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << ", V" << getArg2() << " )";
    }
  };

  template<typename T, typename K>
    class InstGetConstant : public Instruction {
    T value;
    size_t variable;
  public:
    InstGetConstant(K aValue, size_t variable);
    bool execute(__Execute_Args__) {
      Variable * const v = w->get_a_ref(variable)->deref();
      if (v->isFree()) {
	w->do_trail(v);
	v->setValue(value);
	return true;
      } else if (v->isConstant()) {
	if (*v==value)
	  return true;
      }
      return false;
    };
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << value << ", A" << variable << " )";
    }
    virtual ostream& outInst(ostream& os, Engine *w) {
      Variable * const v = w->get_a_ref(variable)->deref();
      os << getFunction();
      return os << "( " << value << ", A"
		<< variable <<"=" << *v << " )" << endl;
    }
  };

  template<> inline InstGetConstant<const char *, const char *>
    ::InstGetConstant(const char *aValue, size_t variable)
    : Instruction(opGetConstantAtom), variable(variable) {
    value = Atoms::copyValue(aValue);
  };
  
  template<> inline InstGetConstant<int, const int>
    ::InstGetConstant(const int aValue, size_t variable)
    : Instruction(opGetConstantInt), value(aValue), variable(variable) {};

  typedef InstGetConstant<const char *, const char *> InstGetConstantAtom;
  typedef InstGetConstant<int,          const int>    InstGetConstantInt;
  
  // class InstGetLevel     : public InstructionSpec<size_t, Instruction, 1> {
  class InstGetLevel        : public Instruction_i {
  public:
  InstGetLevel(int arg1) : Instruction_i(opGetLevel, arg1) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << " )";
    }
  };
  class InstGetStruct       : public Instruction {
    const char *name;
    size_t arity;
    size_t variable;
  public:
  InstGetStruct(const char *aName, size_t arity, size_t variable)
    : Instruction(opGetStruct), arity(arity), variable(variable) {
      name = Atoms::copyValue(aName);
      };
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << name << "/" << arity << ", V"
		<< variable << " )";
    }
  };
  class InstGetList         : public Instruction {
    size_t head;
    size_t variable;
  public:
    void setHead(size_t aHead) { head = aHead; };
  InstGetList(size_t head, size_t variable)
    : Instruction(opGetList), head(head), variable(variable) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << head << ", V" << variable << " )";
    }
  };
  class InstGetValue        : public Instruction_i_i {
  public:
  InstGetValue(int arg1, int arg2) : Instruction_i_i(opGetValue, arg1, arg2) {};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << ", A" << getArg2() << " )";
    }
  };
  class InstGetVariable     : public Instruction_i_i {
  public:
  InstGetVariable(int arg1, int arg2)
    : Instruction_i_i(opGetVariable, arg1, arg2) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << ", A" << getArg2() << " )";
    }
  };
  class InstHalt            : public Instruction {
  public:
  InstHalt() : Instruction(opHalt) {};
    void execute(__Execute_Args__) {}
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  
  template<char op>
    class InstCompare       : public Instruction_i_i {
  private:
    bool compare(int z);
  public:
    InstCompare(int arg1, int arg2);
    virtual ~InstCompare(){};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << ", V" << getArg2() << " )";
    }
  };
  
  template<char op>
    _INLINE bool InstCompare<op>::execute(__Execute_Args__) {
    VariableWPtr v1 = w->get_y_ref(getArg1())->deref();
    VariableWPtr v2 = w->get_y_ref(getArg2())->deref();
    if ((v1->isInteger()) && (v2->isInteger())) {
      int compareValue;
      try {
	compareValue = v1->getInteger() - v2->getInteger();
      } catch (exception e) {
	compareValue = (v1->getInteger()==v2->getInteger());
      }
      return compare(compareValue);
    } else
      return false;
  }
  
  template<char op> inline int operation(int z1, int z2);
  
  template<> inline int operation<'+'>(int z1, int z2)
    { return z1 + z2; }
  template<> inline int operation<'-'>(int z1, int z2)
    { return z1 - z2; }
  template<> inline int operation<'*'>(int z1, int z2)
    { return z1 * z2; }
  template<> inline int operation<'/'>(int z1, int z2)
    { return z1 / z2; }
  template<> inline int operation<'%'>(int z1, int z2)
    { return z1 % z2; }
  
  template<char op>
    class InstArith         : public Instruction {
  private:
    size_t variable;
    size_t value1;
    size_t value2;
  public:
    InstArith(size_t variable, size_t value1, size_t value2);
    virtual ~InstArith(){};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << variable << ", V"
		<< value1 << ", V" << value2 << " )";
    }
    virtual ostream& outInst(ostream& os, Engine *w) {
      VariableWPtr v1, v2, v3;
      v1 = w->get_y_ref(value1)->deref();
      v2 = w->get_y_ref(value2)->deref();
      v3 = w->get_y_ref(variable)->deref();
      os << getFunction();
      return os << "( "
		<< value1 << "=" << *v1 << ", "
		<< value2 << "=" << *v2 << ", "
		<< variable << "=" << *v3 <<" )" << endl;
    }
  };
  
  // is manages integer arithmetic (floating point may be added later)
  template<char op>
    _INLINE bool InstArith<op>::execute(__Execute_Args__) {
    VariableWPtr v1, v2, v3;
    int z1, z2, z3;
    v1 = w->get_y_ref(value1)->deref();
    if (v1->isInteger())
      z1 = v1->getInteger();
    else { return false; }
    
    v2 = w->get_y_ref(value2)->deref();
    if (v2->isInteger())
      z2 = v2->getInteger();
    else { return false; }
    // check which variable is referenced by variable
    v3 = w->get_y_ref(variable)->deref();
    try {
      // do the arithmetic
      z3 = operation<op>(z1, z2);
      // if v3 (the variable) has already been bound, consider this an
      //      equality check
      //      if ((v3.isConstant()) && (parseInt(v3.value) != z3))
      //      // do not allow this for now, since problems might occur
      //        backtrack(programCounter);
      if (v3->isFree()) {
        // if it has not been bound yet, bind it to constant value z3
        // (the integer number)
        w->do_trail(v3);
        v3->setValue(z3);
	return true;
      }
      // only when alle stricke reissen: backtrack!
      else
        return false;
    } catch (exception e) {
      return false;
    }
    return false;
  };
  
  template<> inline InstArith<'+'>::InstArith(size_t variable,
					      size_t value1,
					      size_t value2)
    : Instruction(opArithAdd),
    variable(variable),
    value1(value1),
    value2(value2) {}
  
  template<> inline InstArith<'-'>::InstArith(size_t variable,
					      size_t value1,
					      size_t value2)
    : Instruction(opArithSub),
    variable(variable),
    value1(value1),
    value2(value2) {}
  
  template<> inline InstArith<'*'>::InstArith(size_t variable,
					      size_t value1,
					      size_t value2)
    : Instruction(opArithMul),
    variable(variable),
    value1(value1),
    value2(value2) {}

  template<> inline InstArith<'/'>::InstArith(size_t variable,
					      size_t value1,
					      size_t value2)
    : Instruction(opArithDiv),
    variable(variable),
    value1(value1),
    value2(value2) {}

  template<> inline InstArith<'%'>::InstArith(size_t variable,
					      size_t value1,
					      size_t value2)
    : Instruction(opArithMod),
    variable(variable),
    value1(value1),
    value2(value2) {}
  
  typedef InstArith<'+'> InstArithAdd;
  typedef InstArith<'-'> InstArithSub;
  typedef InstArith<'*'> InstArithMul;
  typedef InstArith<'/'> InstArithDiv;
  typedef InstArith<'%'> InstArithMod;
  
  typedef InstCompare<opBigger>    InstBigger;
  typedef InstCompare<opBiggerEq>  InstBiggerEq;
  typedef InstCompare<opSmaller>   InstSmaller;
  typedef InstCompare<opSmallerEq> InstSmallerEq;
  
  
  template<> inline InstCompare<opBigger>::InstCompare(int arg1, int arg2)
    : Instruction_i_i(opBigger, arg1, arg2){};
  
  template<> inline InstCompare<opBiggerEq>::InstCompare(int arg1, int arg2)
    : Instruction_i_i(opBiggerEq, arg1, arg2){};
  
  template<> inline InstCompare<opSmaller>::InstCompare(int arg1, int arg2)
    : Instruction_i_i(opSmaller, arg1, arg2){};
  
  template<> inline InstCompare<opSmallerEq>::InstCompare(int arg1, int arg2)
    : Instruction_i_i(opSmallerEq, arg1, arg2){};
  
  
  template<> inline bool InstCompare<opBigger>::compare(int z)
    { return z >  0; };
  
  template<> inline bool InstCompare<opBiggerEq>::compare(int z)
    { return z >= 0; };

  template<> inline bool InstCompare<opSmaller>::compare(int z)
    { return z <  0; };

  template<> inline bool InstCompare<opSmallerEq>::compare(int z)
    { return z <= 0; };
  
  class InstNoOp            : public Instruction {
  public:
  InstNoOp() : Instruction(opNoOp) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  class InstProceed         : public Instruction {
  public:
  InstProceed() : Instruction(opProceed) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  class InstProfile : public Instruction {
  public:
  InstProfile(): Instruction(opProfile) {};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    };
  };
  class InstProfileInit     : public Instruction {
  public:
  InstProfileInit()
    : Instruction(opProfileInit) {};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  class InstProfileEnd      : public Instruction {
  public:
  InstProfileEnd() : Instruction(opProfileEnd) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  
  class InstTime : public Instruction {
  public:
  InstTime() : Instruction(opTime) {};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    };
  };
  class InstTimeInit     : public Instruction {
  public:
  InstTimeInit() : Instruction(opTimeInit) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  class InstTimeEnd      : public Instruction {
  public:
  InstTimeEnd() : Instruction(opTimeEnd) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  
  template<typename T, typename K>
    class InstPutAConstant : public Instruction {
    T value;
    size_t variable;
  public:
    InstPutAConstant(K aValue, size_t variable);
    void execute(__Execute_Args__) {
      VariableWPtr Ai = w->get_a_ref(variable);
      Ai->setValue(value);
    };
    
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << outArg(value) << ", A" << variable << " )";
    }
  };

  template<>
    inline InstPutAConstant<const char *, const char *>
    ::InstPutAConstant(const char *aValue, size_t variable)
    : Instruction(opPutAConstantAtom), variable(variable) {
    value = Atoms::copyValue(aValue);
    };
  
  template<>
    inline InstPutAConstant<int, const int>
    ::InstPutAConstant(const int aValue, size_t variable)
    : Instruction(opPutAConstantInt), value(aValue), variable(variable) {};

  typedef InstPutAConstant<const char *, const char *> InstPutAConstantAtom;
  typedef InstPutAConstant<int,          const int>    InstPutAConstantInt;
  
  template<typename T, typename K>
  class InstPutConstant : public Instruction {
    T value;
    size_t variable;
  public:
    InstPutConstant(K aValue, size_t variable);
    void execute(__Execute_Args__) {
      VariableWPtr Vi;
      w->set_variable(variable);
      Vi = w->get_y_ref(variable);
      Vi->setValue(value);
    };
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << outArg(value) << ", V" << variable << " )";
    }
  };

  template<> inline InstPutConstant<const char *, const char *>::InstPutConstant(const char *aValue, size_t variable)
    : Instruction(opPutConstantAtom), variable(variable) {
    value = Atoms::copyValue(aValue);
    };
  
  template<> inline InstPutConstant<int, const int>::InstPutConstant(const int aValue, size_t variable)
    : Instruction(opPutConstantInt), value(aValue), variable(variable) {};

  typedef InstPutConstant<const char *, const char *> InstPutConstantAtom;
  typedef InstPutConstant<int,          const int>    InstPutConstantInt;
  
  template<typename T, typename K>
  class InstSetConstant : public Instruction {
    T value;
  public:
    InstSetConstant(K aValue);
    void execute(__Execute_Args__) {
      VariableWPtr Vi = new (w->heap) Variable();
      Vi->setValue(value);
    };
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << outArg(value) << " )";
    }
  };
  
  template<> inline InstSetConstant<const char *, const char *>
    ::InstSetConstant(const char *aValue)
    : Instruction(opSetConstantAtom) {
    value = Atoms::copyValue(aValue);
  };
  template<> inline InstSetConstant<int, const int>
    ::InstSetConstant(const int aValue)
    : Instruction(opSetConstantInt), value(aValue) {};
  
  typedef InstSetConstant<const char *, const char *> InstSetConstantAtom;
  typedef InstSetConstant<int,          const int>    InstSetConstantInt;
  
  class InstPutValue        : public Instruction_i_i {
  public:
  InstPutValue(int arg1, int arg2) : Instruction_i_i(opPutValue, arg1, arg2) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << ", A" << getArg2() << " )";
    }
  };
  
  class InstRetryMeElse     : public InstructionLabel {
    size_t address;
    const char *name;
    size_t arity;
    size_t clauseNum;
  public:
  InstRetryMeElse(size_t address, const char *aName, size_t arity,
		  size_t clauseNum)
    : InstructionLabel(opRetryMeElse), address(address),
      arity(arity), clauseNum(clauseNum) {
      name = Atoms::copyValue(aName);
    };
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << address  << ", " << name << "/"
		<< arity << "/" << clauseNum << " )";
    }
    virtual ClauseKey getLabel() const {
      return ClauseKey(name, arity, clauseNum);
    }
    virtual void setAddress(size_t address) { this->address = address; }
  };
  class InstSetVariable : public Instruction_i {
  public:
    InstSetVariable(int arg1)
      : Instruction_i(opSetVariable, arg1) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << " )";
    }
  };
  class InstTrustMe         : public Instruction {
  public:
  InstTrustMe() : Instruction(opTrustMe) {};
    void execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os;
    }
  };
  class InstTryMeElse       : public InstructionLabel {
    size_t address;
    const char *name;
    size_t arity;
    size_t clauseNum;
  public:
  InstTryMeElse(size_t address, const char *aName, int arity,
		size_t clauseNum)
    : InstructionLabel(opTryMeElse), address(address),
      arity(arity), clauseNum(clauseNum) {
      name = Atoms::copyValue(aName);
    };
    void execute(__Execute_Args__);
    size_t getAddress() const { return address; }
    virtual ostream &outArgs(ostream &os) const {
      return os << "( " << address  << ", " << name << "/"
		<< arity << "/" << clauseNum << " )";
    }
    virtual ClauseKey getLabel() const {
      return ClauseKey(name, arity, clauseNum);
    }
    virtual void setAddress(size_t address) { this->address = address; }
  };
  class InstUnequal         : public Instruction_i_i {
  public:
  InstUnequal(int arg1, int arg2) : Instruction_i_i(opUnequal, arg1, arg2) {};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << ", V" << getArg2() << " )";
    }
  };
  class InstUnifyVariable   : public Instruction_i_i {
  public:
  InstUnifyVariable(int arg1, int arg2)
    : Instruction_i_i(opUnifyVariable, arg1, arg2) {};
    bool execute(__Execute_Args__);
    virtual ostream &outArgs(ostream &os) const {
      return os << "( V" << getArg1() << ", V" << getArg2() << " )";
    }
    virtual size_t getParameter(Engine *w) const;
  };
};

# ifdef INLINE
#  include "InstructionSpecImpl.h"
# endif

#endif // _InstructionSpec_H_

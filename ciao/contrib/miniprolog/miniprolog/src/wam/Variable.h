/*
** Variable.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Nov  1 01:37:11 2007 Edison Mera
** Last update Sun Dec 27 20:56:51 2009 Edison Mera
*/

#ifndef   	_Variable_H_
# define   	_Variable_H_

# include "hrtime.h"
# include "WAMUtils.h"
# include "Settings.h"
# include "Atoms.h"

# ifdef USE_GC
#  include "gc.h"
# endif

# include <iostream>
# include <map>
# include <set>

using namespace std;

namespace wam {
  
  class Engine;
  class Variable;
  class ChoicePoint;
  
# ifdef USE_GC
  typedef GCPtr<Variable> VariablePtr;
# else
  typedef Variable * VariablePtr;
# endif
  
  typedef Variable * VariableWPtr;
  typedef map<const char *, const Variable *> NamedVariables;
  typedef map<const Variable *, const char *> VariableNames;

  extern void cleanVariableNames(VariableNames &vn);
  
  enum Tags {REF, FREE, LIS, STR, INT, ATM};
  
  class Structure {
    const char *name;
    size_t arity;
  public:
    size_t getArity() const { return arity; };
    void setArity(size_t aArity) { arity = aArity; };
    const char *getName() const { return name; };
    void setName(const char *aName) { name = aName; };
    friend bool operator == (const Structure &s1, const Structure &s2) {
      return (strcmp(s1.name, s2.name)==0) && s1.arity==s2.arity;
    }
  };
  
  class Variable {
    union {
      VariableWPtr reference;  // variable's content in case of REF
      size_t constant;         // variable's content in case of INT
			       // (its size is given by the platform)
      ChoicePoint *cutLevel;   // for the cut and get_level
			       // instructions
      Structure structure;     // Name/Arity in case of STR (0 if ATM)
    } value;
  public:
    Tags tag;
    
    Variable();
    Variable(const int v);
    Variable(const VariableWPtr &v);
    Variable(const Variable &v);
    ~Variable();
    
    void setHead(VariablePtr aHead);
    VariablePtr getHead() const;
    void setTail(VariablePtr aTail);
    VariablePtr getTail() const;
    
    static const char* getClassName() { return "Variable"; };

    DO_DEBUG(static void *memory, 3);
    
    VariableWPtr getReference() const
    {
      return value.reference;
    }
    ChoicePoint *getCutLevel() const { return value.cutLevel; };

    void setCutLevel(ChoicePoint *cutLevel) {
      value.cutLevel = cutLevel;
    };

    VariableWPtr getArg(size_t index) const {
      return const_cast<Variable *>(this) + index + 1;
    }
    Structure getStructure() const {
      return value.structure;
    }
    size_t getArity() const {
      return value.structure.getArity();
    }

    void setArity(size_t aArity) {
      value.structure.setArity(aArity);
    }
    
    // sets internal components to that of source
    void copyFrom(Variable * const source);
    
    // dereferencing: if this variable points to another var, then
    // return that dereferenced
    Variable *deref() const {
      if (isRef()) {
	VariableWPtr result = getReference();
/* 	VariableWPtr update = result; */
	while (result->isRef())
	  result = result->getReference();
/* 	// make dereferencing permanent */
/* 	while (update->isRef()) { */
/* 	  VariableWPtr nextRef = update->getReference(); */
/* 	  update->copyFrom(result); */
/* 	  update = nextRef; */
/* 	} */
	return result;
	//copyFrom(*result);
      }
      else
	return const_cast<Variable *>(this);
    }
    
    friend Variable *deref(Variable * const s) {
      if (s->isRef()) {
	VariableWPtr result = s->getReference();
	while (result->isRef())
	  result = result->getReference();
	return result;
      } else
	return s;
    }
    
    bool operator == (const int value) {
      return isInteger() && (getInteger()==value);
    }
    bool operator == (const char *value) {
      return isAtom() && (strcmp(getString(),value)==0);
    }
    // returns a string in the form NAME = VALUE, representing the
    // variable's value
    friend ostream &operator << (ostream& os, const Variable &s);
    
    bool isRef()       const { return tag == REF; };
    bool isFree()      const { return tag == FREE; };
    bool isList()      const { return tag == LIS; };
    bool isConstant()  const { return tag == INT || tag == ATM; };
    bool isStructure() const { return tag == STR; };
    bool isAtom()      const { return tag == ATM; };
    bool isAtomic()    const { return tag == INT || tag == ATM; };
    bool isInteger()   const { return tag == INT; };
    
    const char *getString() const {
      return value.structure.getName();
    }
    
    size_t getInteger() const {
      return value.constant;
    }
    
    void parseValue(const char *s) {
      if (s[0] != '\0' && '0' <= s[0] && s[0] <= '9') {
	setValue(parseInt(s));
      }
      else {
	copyValue(s);
      }
    }
    
    void setValue(const size_t &i) {
      tag = INT;
      value.constant = i;
    }
    
    void setValue(const char *a) {
      tag = ATM;
      value.structure.setName(a);
      value.structure.setArity(0);
    }
    
    void copyValue(const char *a) {
      tag = ATM;
      value.structure.setName(Atoms::copyValue(a));
      value.structure.setArity(0);
    }
    
    void setStructure(const char *a, size_t arity) {
      tag = STR;
      value.structure.setName(a);
      value.structure.setArity(arity);
    }
    
    void setReference(VariableWPtr const reference) {
      tag = REF;
      value.reference = reference;
    }
    
    void setFree() {
      tag = FREE;
      value.reference = this;
    }
  };
  
  ostream &os_variable(ostream &os,
		       VariableNames &,
		       const Variable &s,
		       NamedVariables *queryVariables);
  
  ostream &os_variable(ostream &os,
		       const Variable &s,
		       NamedVariables *queryVariables);
  
  ostream & operator << (ostream &os, const Variable &s);

  istream & operator >> (istream &is, Variable &s);
  
}

# ifdef INLINE
#  include "VariableImpl.h"
# endif

#endif 	    /* !_Variable_H_ */

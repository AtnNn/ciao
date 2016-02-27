/*
** VariableImpl.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Nov 15 21:43:20 2007 Edison Mera
** Last update Thu Nov 15 21:43:20 2007 Edison Mera
*/

#ifndef   	VARIABLEIMPL_H_
# define   	VARIABLEIMPL_H_

namespace wam {
  _INLINE Variable::Variable() {
    DO_DEBUG(cerr << "*** creating a new unbound variable "
	     << (void *)this << " ***" << endl, 4);
    setFree();
  }
  
  _INLINE Variable::Variable(const int v) {
    DO_DEBUG(cerr << "*** creating a new numeric variable "
	       << (void *)this << " ***" << endl, 3);
      setValue(v);
  }
  
  // constructor for creating a new variable and unifying it
  // with another
  _INLINE Variable::Variable(const VariableWPtr &v) {
    DO_DEBUG(cerr << "*** creating a new reference variable "
	     << (void *)this << " to " << (void *)(&v) << " ***" << endl, 3);
    setReference(v);
  }
  
  // copyFrom-constructor
  _INLINE Variable::Variable(const Variable &v) {
    DO_DEBUG(cerr << "*** creating a new copy variable "
	     << (void *)this << " of " << (void *)(&v) << " ***" << endl, 3);
    copyFrom(const_cast<Variable *>(&v));
  }
  
  _INLINE Variable::~Variable() {
    DO_DEBUG(cerr << "*** destroying a variable "
	     << (void *)this << " ***" << endl, 3);
  }
  
  // sets internal components to that of source
  _INLINE void Variable::copyFrom(Variable * const source) {
    //name = source.name;
    switch(source->tag) {
    case REF:
      setReference(source->getReference());
      break;
    case FREE: case LIS: case STR:
      setReference(source);
      break;
    case INT: case ATM:
      tag = source->tag;
      value = source->value;
    }
  }

  _INLINE void Variable::setHead(VariablePtr aHead) {
    value.reference = aHead;
  }
  
  _INLINE void Variable::setTail(VariablePtr aTail) {
    getArg(0)->copyFrom(aTail);
  }

  _INLINE VariablePtr Variable::getHead() const {
    return value.reference;
  }
  
  _INLINE VariablePtr Variable::getTail() const {
    return getArg(0)->deref();
  }
  
}

#endif 	    /* !VARIABLEIMPL_H_ */

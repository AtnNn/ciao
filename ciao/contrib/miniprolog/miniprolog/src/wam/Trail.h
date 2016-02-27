/*
** Trail.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Nov 15 19:39:52 2007 Edison Mera
** Last update Thu Nov 15 19:39:52 2007 Edison Mera
*/

#ifndef   	TRAIL_H_
# define   	TRAIL_H_

#include "Variable.h"

namespace wam {
    // Trail implements the WAM's trail (undo-list for bindings performed)
  class Trail {
    VariablePtr *memory;
    size_t length;
  public:
/*     vector<VariablePtr> contents; */
    
  Trail(void *memory) : memory((VariablePtr *)memory), length(0) {};
    
    size_t getLength() {return length; };
    
    void setLength(size_t aLength) { length = aLength; };
    
    void clear() {
      setLength(0);
    };
    
    void addEntry(Variable *v) {
      memory[length++] = v;
    };
    
    VariablePtr getEntry(int index) {
      return memory[index];
    };
    
    void undo(int index) {
      VariableWPtr v = memory[index];
      if (v != NULL) {
	v->setFree();
      }
    };
    
    friend ostream& operator << (ostream& os, const Trail &s) {
      return os << "Trail" << endl;
    };
    
    }; // end of class Trail
  
}

#endif 	    /* !TRAIL_H_ */

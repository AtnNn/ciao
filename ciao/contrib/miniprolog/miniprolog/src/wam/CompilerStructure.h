/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * CompilerStructure.java contains the CompilerStructure class, which is needed
 * for transforming the input vector (Prolog program) to the output Program
 * (WAM code) via a certain program structure graph.
 ******************************************************************************/

// Each instance of CompilerStructure represents a node in the program graph
// of the original Prolog program. Every node is of a certain type
// (see constants  below).


#ifndef _CompilerStructure_H_
#define _CompilerStructure_H_

#include <string>
#include <vector>

using namespace std;

namespace wam {
  
  enum CompilerStrucType {
    csNO_TYPE     = -1,   // no type or unknown type
    csQUERY       = +0,   // this is a query (list), composed of
		          // a set of conditions
    csTERM        ,   // this is a term, e.g. "s(a, Y)"
    csLIST        ,   // this is a list, e.g. "s", "a, X, c"
    csATOM        ,   // a, b, c
    csINT         ,   // 1, 2, 3
    csVARIABLE    ,   // this is a variable, e.g. "A", "B", "Z"
    csPREDICATE   ,   // this is a predicate, e.g. "father", "length"
    csCLAUSE      ,   // this is a clause, composed of a HEAD
		      // (this.head) and a BODY (this.tail)
    csPROGRAM     ,   // this is a whole Prolog program, i.e. a
		      // list of PROCEDUREs
    csHEAD        ,   // this is a PROCEDURE's head, composed of a
		      // PREDICATE name and a parameter LIST
    csBODY        ,   // this is a PROCEDURE's body, i.e. a list
		      // of CONDITIONs
    csCALL        ,   // this is a condition, e.g. "father(X, Y)",
		      // composed of the PREDICATE name
		      // and a LIST of calling arguments
		      // and returns true upon failure
    csUNIFICATION ,   // this is a unification of the form "X = Y"
		      // (args in head and tail).
    csASSIGNMENT  ,   // this is an assignment of the form "X = 1
		      // + 3", where X can be found in head, + in
		      // tail.value and 1 (3) in tail.head
		      // (tail.tail)
    csEXPRESSION  ,   // this is an arithmetic expression, to be
		      // used in ASSIGNMENTs, in "X = 1 + 3", "1 +
		      // 3" would be the expression, with + as
		      // value, 1 as (constant) head and 3 as
		      // (constant) tail
    csCOMPARISON  ,   // something like "X < 5" or "Z > Y"
    csSTRUCTURE   ,   // this is a structure, e.g. "s(x, y, X)",
		      // "auto(mobil, nix_is)"
    csCUT             // the value, e.g. the variable's name in
		      // case of type == VARIABLE
  };
  
  class CompilerStructure {
  public:
    
    CompilerStrucType type;         // the type of the
				    // node, as explained
				    // above 
    CompilerStructure *head, *tail; // sub-nodes in case of
				    // non-trivial nodes (lists,
				    // queries, ...)
    vector <CompilerStructure *> args; // used in case of STRUCTURE
    string value;                 // the value, e.g. the variable's
    // name in case of type ==
    // VARIABLE
    
    int arity, number, count; // an integer value, e.g., the clause arity,
    // number, etc. Please put total numbers in
    // count and indexes or partial numbers in
    // number.
    
    // create a new structure of unknown type
    CompilerStructure();
    
    ~CompilerStructure() {
      if (head != NULL)
	delete head;
      if (tail != NULL)
	delete tail;
      for (size_t i = 0; i < args.size(); i++) {
	delete args[i];
      }
    }
    
    void setHead(CompilerStructure *aHead) {
      if (head != NULL)
	delete head;
      head = aHead;
    }
    void setTail(CompilerStructure *aTail) {
      if (tail != NULL)
	delete tail;
      tail = aTail;
    }
    // create a new, trivial structure of type aType with initial value aValue
    CompilerStructure(CompilerStrucType aType, const string &aValue);
    
    // return the string that shall be used to display this node on the screen
    string toString() const;
    
    friend ostream& operator << (ostream& os, const CompilerStructure &s);
    
  };
  
}

#endif // _CompilerStructure_H_

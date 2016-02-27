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
// (see constants below).

#include <vector>
#include <iostream>

#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "CompilerStructure.h"

using namespace std;
using namespace wam;

namespace wam {
  
  // create a new structure of unknown type
  CompilerStructure::CompilerStructure() {
    type = csNO_TYPE;
    head = NULL;
    tail = NULL;
    value = "";
  }; // end of CompilerStructure.CompilerStructure()
  
  // create a new, trivial structure of type aType with initial value aValue
  CompilerStructure::CompilerStructure(CompilerStrucType aType,
				       const string &aValue) {
    type = aType;
    value = aValue;
  }; // end of CompilerStructure.CompilerStructure(int, String)
  
  ostream& operator << (ostream& os, const CompilerStructure &s) {
    return os << s.toString();
  };
  
  // return the string that shall be used to display this node on the screen
  string CompilerStructure::toString() const {
    if (type == csNO_TYPE)
      return "[no type]";
    else if ((type == csTERM) || (type == csQUERY)) {
      if (tail == NULL)
        return head->toString();
      else
        return head->toString() + "(" + tail->toString() + ")";
    } else if (type == csPREDICATE)
      return value;
    else if (type == csATOM)
      return "atm " + value;
    else if (type == csINT)
      return "int " + value;
    else if (type == csVARIABLE)
      return "var " + value;
    else if (type == csPROGRAM) {
      if (tail == NULL)
        return "\n" + head->toString();
      else
        return "\n" + head->toString() + tail->toString();
    } else if (type == csCLAUSE) {
      if (tail == NULL)
        return head->toString() + ".";
      else
        return head->toString() + " :-\n" + tail->toString() + ".";
    } else if (type == csHEAD) {
      if (tail == NULL)
        return head->toString();
      else
        return head->toString() + "(" + tail->toString() + ")";
    } else if (type == csBODY) {
      if (tail == NULL)
        return "  " + head->toString();
      else
        return "  " + head->toString() + ",\n" + tail->toString();
    } else if (type == csCALL) {
      if (tail == NULL)
        return head->toString();
      else
        return head->toString() + "(" + tail->toString() + ")";
    }
    /*
      else if (type == csNOT_CALL) {
      if (tail == NULL)
      return "not " + head->toString();
      else
      return "not " + head->toString() + "(" + tail->toString() + ")";
      }
    */
    else if (type == csCOMPARISON) {
      return head->toString() + " " + value + " " + tail->toString();
    } else if (type == csLIST) {
      if (head == NULL)
        return "[]";
      else {
        if (tail == NULL)
          return head->toString();
        else
          return head->toString() + ", " + tail->toString();
      }
    } else if (type == csSTRUCTURE) {
      if (tail == NULL)
        return head->toString();
      else
        return head->toString() + "(" + tail->toString() + ")";
    } else
      return "[unknown type]";
  };
  
}

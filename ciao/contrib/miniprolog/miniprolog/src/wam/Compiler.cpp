/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * Compiler.java contains the base class Compiler, which both QueryCompiler and
 * PrologCompiler have been derived from.
 ******************************************************************************/


#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "Program.h"
#include "WAMUtils.h"
#include "Compiler.h"
#include "CompilerStructure.h"
#include "InstructionSpec.h"

#include <stdio.h>
#include <sstream>
#include <string>
#include <vector>

using namespace std;
using namespace wam;

namespace wam {
  
  bool Compiler::isPredicate(const string &s) {
    return (isConstant(s) && (!isInt(s)));
  };
  
  bool Compiler::isVariable(const string &s) {
    if (s == "_")
      return true;
    char c = s.at(0);
    if (((c >= 'A') && (c <= 'Z')) || (c == '_')) {
      for (size_t i = 1; i < s.size(); i++) {
        c = s.at(i);
        if (((c < 'a') || (c > 'z'))
	    && ((c < 'A') || (c > 'Z'))
	    && ((c < '0') || (c > '9'))
	    && (c != '_'))
	  {
	    errorString = ("\"" + s + "\" is not a valid variable.");
	    return false;
	  }
      }
      return true;
    } else
      return false;
  }; // end of Compiler.isVariable()
  
  bool Compiler::isAtom(const string &s) {
    char c = s.at(0);
    if ((c >= 'a') && (c <= 'z')) {
      for (size_t i = 1; i < s.size(); i++) {
        c = s.at(i);
        if (((c < 'a') || (c > 'z')) && ((c < 'A') || (c > 'Z'))
	    && ((c < '0') || (c > '9')) && (c != '_')) {
          errorString = ("\"" + s
			 + "\" is not a valid constant or predicate.");
          return false;
        }
      }
      return true;
    }
    if ((c == '\'') && (s.at(s.size() - 1) == '\''))
      return true;
    if ((s == ";") || (s == ".") || (s == "+") || (s == "#"))
      return true;
    return false;
  }
  
  bool Compiler::isConstant(const string &s) {
    if (isAtom(s))
      return true;
    return isInt(s);
  };
  
  /*
    bool Compiler::profile(vector<string> &prog, CompilerStructure &struc) {
    if (prog.size() == 0) return false;
    string q0 = prog[0];
    if (q0 == "profile") {
    struc.type = csCALLPROFILED;
    struc.value = q0;
    prog.erase(prog.begin());
    return true;
    }
    return false;    
    }
  */
  
  bool Compiler::predicate(const vector<string> &prog, size_t &index,
			   CompilerStructure &struc) {
    if (prog.size() <= index) return false;
    string q0 = prog[index];
    if (isPredicate(q0)) {
      struc.type = csPREDICATE;
      struc.value = q0;
      index++;
      return true;
    }
    return false;
  }; // end of Compiler.predicate(Vector, CompilerStructure)
  
  bool Compiler::constant(const vector<string> &prog, size_t &index,
			  CompilerStructure &struc) {
    if (prog.size() <= index) return false;
    string q0 = prog.at(index);
    if (isConstant(q0)) {
      if (isAtom(q0))
        struc.type = csATOM;
      else
        struc.type = csINT;
      if (q0.at(0) == '\'')
        struc.value = q0.substr(1, q0.size() - 2);
      else
        struc.value = q0;
      index++;
      return true;
    }
    size_t oldIndex = index;
    if ((token(prog, index, "["))
	&& (token(prog, index, "]"))) {
      struc.type = csATOM;
      struc.value = "[]";
      return true;
    }
    index = oldIndex;
    return false;
  };
  
  bool Compiler::variable(const vector<string> &prog, size_t &index,
			  CompilerStructure &struc) {
    if (prog.size() <= index) return false;
    string q0 = prog.at(index);
    if (isVariable(q0)) {
      struc.type = csVARIABLE;
      struc.value = q0;
      index++;
      return true;
    }
    return false;
  };
  
  bool Compiler::args(const vector<string> &prog, size_t &index,
		      CompilerStructure &struc) {
    size_t oldIndex = index;
    CompilerStructure *arg = new CompilerStructure();
    struc.args.push_back(arg);
    if (element(prog, index, *arg)) {
      if (isNextToken(prog, index, ",")) {
        token(prog, index, ",");
        if (args(prog, index, struc)) return true;
      } else {
        return true;
      }
    }
    index = oldIndex;
    return false;
  };
  
  bool Compiler::structure(const vector<string> &prog, size_t &index,
			   CompilerStructure &struc) {
    if (prog.size() <= index) return false;
    size_t oldIndex;
    oldIndex = index;
    struc.setHead(new CompilerStructure());
    struc.setTail(new CompilerStructure());
    struc.type = csSTRUCTURE;
    if ((predicate(prog, index, *struc.head))
	&& (token(prog, index, "("))
	&& (args(prog, index, *struc.tail))
	&& (token(prog, index, ")"))) {
      struc.head->type = csATOM;
      return true;
    }
    index = oldIndex;
    if ((variable(prog, index, *struc.head))
	&& (token(prog, index, "("))
	&& (args(prog, index, *struc.tail))
	&& (token(prog, index, ")")))
      return true;
    index = oldIndex;
    return false;
  };
  
  bool Compiler::element(const vector<string> &prog, size_t &index,
			 CompilerStructure &struc) {
    if (prog.size() <= index) return false;
    size_t oldIndex = index;
    if (structure(prog, index, struc))
      return true;
    if (variable(prog, index, struc))
      return true;
    if (constant(prog, index, struc))
      return true;
    if ((token(prog, index, "["))
	&& (list(prog, index, struc))
	&& (token(prog, index, "]")))
      return true;
    index = oldIndex;
    return false;
  };
  
  bool Compiler::isNextToken(const vector<string> &prog, const size_t index,
			     const string &tok) {
    if (prog.size() <= index) return false;
    if (tok == prog.at(index))
      return true;
    return false;
  }; // end of Compiler.isNextToken(Vector, String)
  
  bool Compiler::token(const vector<string> &prog, size_t &index,
		       const string &tok) {
    if (prog.size() <= index) return false;
    if (tok == prog.at(index)) {
      index++;
      return true;
    }
    return false;
  };
  
  bool Compiler::atom(const vector<string> &prog, size_t &index,
		      CompilerStructure &struc) {
    if (constant(prog, index, struc))
      return true;
    if (variable(prog, index, struc))
      return true;
    return false;
  };
  
  bool Compiler::expression(const vector<string> &prog, size_t &index,
			    CompilerStructure &struc) {
    size_t oldIndex = index;
    struc.type = csEXPRESSION;
    struc.setHead(new CompilerStructure());
    struc.setTail(new CompilerStructure());
    int cnt = 1;
    string tok = "";
    do {
      switch (cnt) {
      case 1: tok = "+"; break;
      case 2: tok = "-"; break;
      case 3: tok = "*"; break;
      case 4: tok = "/"; break;
      case 5: tok = "mod"; break;
      }
      if ((atom(prog, index, *struc.head))
	  && (token(prog, index, tok))
	  && (atom(prog, index, *struc.tail))) {
        struc.value = tok;
        return true;
      }
      index = oldIndex;
    } while (++cnt <= 5);
    errorString = "Invalid expression on right side of assignment.";
    return false;
  }; // end of Compiler.expression(Vector, CompilerStructure)
  
  bool Compiler::literal(const vector<string> &prog, size_t &index,
			   CompilerStructure &struc) {
    //    if (prog == NULL) return false;
    size_t oldIndex = index;
    struc.setHead(new CompilerStructure());
    struc.setTail(new CompilerStructure());
    // first type of a condition is a comparison
    if (atom(prog, index, *struc.head)) {
      struc.type = csCOMPARISON;
      if (isNextToken(prog, index, ">")) {
        token(prog, index, ">");
        if (isNextToken(prog, index, "=")) {
          if ((token(prog, index, "="))
	      && (atom(prog, index, *struc.tail))) {
            struc.value = ">=";
            return true;
          }
        } else if (atom(prog, index, *struc.tail)) {
          struc.value = ">";
          return true;
        }
      } else if (isNextToken(prog, index, "<")) {
        token(prog, index, "<");
        if (isNextToken(prog, index, "=")) {
          if ((token(prog, index, "="))
	      && (atom(prog, index, *struc.tail))) {
            struc.value = "<=";
            return true;
          }
        } else if (atom(prog, index, *struc.tail)) {
          struc.value = "<";
          return true;
        }
      } else if (isNextToken(prog, index, "=")) {
        token(prog, index, "=");
        if (isNextToken(prog, index, "<")) {
          if ((token(prog, index, "<"))
	      && (atom(prog, index, *struc.tail))) {
            struc.value = "<=";
            return true;
          }
        } else if ((token(prog, index, "="))
		   && (atom(prog, index, *struc.tail))) {
          struc.value = "==";
          return true;
        }
      } else if (isNextToken(prog, index, "!")) {
        token(prog, index, "!");
        if ((token(prog, index, "="))
	    && (atom(prog, index, *struc.tail))) {
          struc.value = "!=";
          return true;
        }
      } else if (isNextToken(prog, index, "\\")) {
        token(prog, index, "\\");
        if (isNextToken(prog, index, "=")) {
          token(prog, index, "=");
          if (isNextToken(prog, index, "=")) {
            if ((token(prog, index, "="))
		&& (atom(prog, index, *struc.tail))) {
              struc.value = "!=";
              return true;
            }
          }
          else
            if (atom(prog, index, *struc.tail)) {
	      struc.value = "!=";
	      return true;
	    }
        }
      }
    } // end of comparison checks
    index = oldIndex;
    if (element(prog, index, *struc.head)
	&& token(prog, index, "=")
	&& element(prog, index, *struc.tail)) {
      struc.type = csUNIFICATION;
      return true;
    }
    index = oldIndex;
    if (variable(prog, index, *struc.head)
	&& token(prog, index, "is")
	&& expression(prog, index, *struc.tail)) {
      struc.type = csASSIGNMENT;
      return true;
    }
    /*
      prog.clear();
      prog.assign(oldProg.begin(), oldProg.end());
      if ((token(prog, "not")) && (predicate(prog, *struc.head))) {
      struc.type = csNOT_CALL;
      if (isNextToken(prog, "(")) {
      token(prog, "(");
      if ((list(prog, *struc.tail)) && (token(prog, ")")))
      return true;
      } else {
      delete struc.tail;
      struc.tail = NULL;
      return true;
      }
      }
    */
    index = oldIndex;
    if (predicate(prog, index, *struc.head)) {
      struc.type = csCALL;
      if (isNextToken(prog, index, "(")) {
        token(prog, index, "(");
        if ((list(prog, index, *struc.tail)) && (token(prog, index, ")"))) {
          return true;
	}
      } else {
        delete struc.tail;
        struc.tail = NULL;
        return true;
      }
    }
    index = oldIndex;
    if (isNextToken(prog, index, "!")) {
      token(prog, index, "!");
      struc.type = csCUT;
      return true;
    }
    return false;
  }; // end of Compiler.literal(Vector, CompilerStructure)
  
  bool Compiler::body(const vector<string> &prog, size_t &index,
		      CompilerStructure &struc) {
    size_t oldIndex = index;
    struc.type = csBODY;
    struc.setHead(new CompilerStructure());
    if (literal(prog, index, *struc.head)) {
      if (isNextToken(prog, index, ",")) {
        token(prog, index, ",");
        struc.setTail(new CompilerStructure());
        if (body(prog, index, *struc.tail)) return true;
      } else {
        struc.tail = NULL;
        return true;
      }
    }
    index = oldIndex;
    return false;
  }; // end of Compiler.body(Vector, CompilerStructure)
  
  bool Compiler::clause(const vector<string> &prog, size_t &index,
			CompilerStructure &struc) {
    size_t oldIndex = index;
    struc.type = csCLAUSE;
    struc.setHead(new CompilerStructure());
    if (head(prog, index, *struc.head)) {
      if (isNextToken(prog, index, ":")) {
        token(prog, index, ":");
        struc.setTail(new CompilerStructure());
        if ((token(prog, index, "-")) && (body(prog, index, *struc.tail))
	    && (token(prog, index, ".")))
          return true;
      } else if (isNextToken(prog, index, ".")) {
        token(prog, index, ".");
        struc.tail = NULL;
        return true;
      } else {
        struc.tail = NULL;
        errorString = "Missing \".\" at end of clause.";
      }
    }
    index = oldIndex;
    return false;
  }; // end of Compiler.clause(Vector, CompilerStructure)
  
  bool Compiler::program(const vector<string> &prog, size_t &index,
			 CompilerStructure &struc) {
    //    Vector oldProg = (Vector)prog.clone();
    struc.type = csPROGRAM;
    struc.setHead(new CompilerStructure());
    struc.setTail(new CompilerStructure());
    if (clause(prog, index, *struc.head)) {
      if (program(prog, index, *struc.tail))
        return true;
      delete struc.tail;
      struc.tail = NULL;
      return true;
    }
    return false;
  }; // end of Compiler.program(Vector, CompilerStructure)
  
  bool Compiler::head(const vector<string> &prog, size_t &index,
		      CompilerStructure &struc) {
    //    Vector oldProg = (Vector)prog.clone();
    struc.type = csHEAD;
    struc.setHead(new CompilerStructure());
    if (predicate(prog, index, *struc.head)) {
      struc.head->arity = 0;
      if (isNextToken(prog, index, "(")) {
        token(prog, index, "(");
        struc.setTail(new CompilerStructure());
        if ((list(prog, index, *struc.tail, struc.head->arity))
	    && (token(prog, index, ")")))
	  return true;
      } else {
        struc.tail = NULL;
        return true;
      }
    }
    return false;
  };

  bool Compiler::list(const vector<string> &prog, size_t &index,
		      CompilerStructure &struc, int &arity) {
    size_t oldIndex = index;
    struc.type = csLIST;
    struc.setHead(new CompilerStructure());
    if (element(prog, index, *struc.head)) {
      arity++;
      if (isNextToken(prog, index, "|")) {
        token(prog, index, "|");
        struc.setTail(new CompilerStructure());
        if (element(prog, index, *struc.tail)) return true;
      } else if (isNextToken(prog, index, ",")) {
        token(prog, index, ",");
        struc.setTail(new CompilerStructure());
        if (list(prog, index, *struc.tail, arity)) return true;
      } else {
        struc.setTail(NULL);
        return true;
      }
    }
    index = oldIndex;
    return false;
  };
  
  bool Compiler::stringToList(const string &text, vector<string> &result) {
    size_t i;
    string dummy = "";
    string tmp;
    for (i = 0; i < text.size(); i++) {
      char pos = text[i];
      if (pos == '\'') {
        if (dummy.size() > 0) return false;
        dummy = "'";
        do {
          i++;
          dummy += text[i];
          if (text[i] == '\'')
            break;
        } while (i < text.size() - 1);
      } else if (pos == '%') {
	do {
	  i++;
	  if (text[i] == '\n')
	    break;
	} while (i < text.size() - 1);
	dummy = "";
      } else if (!isspace(pos)) {
        if ((pos == '(') || (pos == ')') || (pos == '[') || (pos == ']') ||
	    (pos == ',') || (pos == '.') || (pos == '|') || (pos == '=') ||
	    (pos == '<') || (pos == '>') || (pos == '%') || (pos == '\\') ||
	    (pos == '+') || (pos == '-') || (pos == '*') || (pos == '/')) {
          if (dummy.length() > 0)
            result.push_back(dummy);
          dummy = "";
          tmp = dummy;
          tmp += pos;
          result.push_back(tmp);
        } else
          dummy += pos;
      } else {
        if (dummy.size() > 0)
          result.push_back(dummy);
        dummy = "";
      }
    }
    if (dummy.size() > 0)
      result.push_back(dummy);
    return true;
  };
  
  int SubstitutionList::substituteVariable(const string &variable) {
    if ((variable.size() > 0) && (variable!="_"))
      if (nameVars.find(variable)!=nameVars.end()) {
        lastVar = nameVars[variable];
        return lastVar;
      }
    lastVar = varCount;
    nameVars[variable] = lastVar;
    varCount++;
    return lastVar;
  }; // end of Compiler.substituteVariable(String)

  bool SubstitutionList::firstOccurrence(const string &variable) {
    if ((variable.size() > 0) && (variable!="_"))
      if (nameVars.find(variable) != nameVars.end())
        return false;
    return true;
  }; // end of Compiler.firstOccurrence(String)
  
  int Compiler::substituteVariable(const string &variable) {
    return substitutionList.substituteVariable(variable);
  };
  
  bool Compiler::firstOccurrence(const string &variable) {
    return substitutionList.firstOccurrence(variable);
  };
  
  // structureToCode takes a CompilerStructure, generated by the
  // parser, and constructs a WAM program from it, recursively
  bool Compiler::structureToCode(const CompilerStructure *struc,
				 Program &result)
  {
    if (struc == NULL) return false;
    return structureToCode(*struc, result);
  }
  
  bool Compiler::listToCode(const CompilerStructure *struc,
			    Program &result)
  {
    vector<int> hv;
    int headVar, i = 0, v;
    const CompilerStructure *list = struc;
    while(list!=NULL && list->type==csLIST) {
      structureToCode(list->head, result);
      if (list->head->type==csVARIABLE) {
	if (firstOccurrence(list->head->value)) {
	  headVar = substituteVariable(list->head->value);
	  new (result) InstSetVariable(headVar);
	}
	else
	  headVar = substituteVariable(list->head->value);
      }
      else
	headVar = getLastVar();
      hv.push_back(headVar);
      i++;
      list = list->tail;
    }
    if (list==NULL);
    else if (list->type!=csLIST) {
      structureToCode(list, result);
      hv.push_back(getLastVar());
    }
    list = struc;
    i = 0;
    v = substituteVariable("");
    new (result) InstSetVariable(v);
    headVar = v;
    while(list!=NULL && list->type==csLIST) {
      new (result) InstGetList(hv[i++], v);
      list = list->tail;
      if (list!=NULL) {
	v = substituteVariable("");
	new (result) InstSetVariable(v);
      }
    }
    if (list==NULL) {
      new (result) InstSetConstantAtom("[]");
    } else
      new (result) InstUnifyVariable(hv[i], v);
    setLastVar(headVar);
    return true;
  }

  void Compiler::createVariableIfRequired(const CompilerStructure *s,
					  Program &result)
  {
    if (firstOccurrence(s->value)) {
      int v = substituteVariable(s->value);
      new (result) InstSetVariable(v);
      createQueryVariableIfRequired(s, result);
    }
    else
      substituteVariable(s->value);
  }
  
  bool Compiler::headToCode(const CompilerStructure *struc,
			    Program &result)
  {
    switch (struc->type) {
    case csHEAD: {
      string name = struc->head->value;
      int atAll = struc->head->count;
      int count = struc->head->number;
      int arity = struc->head->arity;
      if (count < atAll) {
	if (count == 1) {
	  result.labels[ClauseKey(struc->head->value.c_str(), arity, count)]
	    = result.getTop();
	  new (result)
	    InstTryMeElse((size_t)-1, name.c_str(), arity, count + 1);
	}
	else {
	  result.labels[ClauseKey(struc->head->value.c_str(), arity, count)]
	    = result.getTop();
	  new (result)
	    InstRetryMeElse((size_t)-1, name.c_str(), arity, count + 1);
	}
      } else {
	result.labels[ClauseKey(struc->head->value.c_str(), arity, count)]
	  = result.getTop();
	if (atAll > 1)
	  new (result) InstTrustMe();
	// 	else
	// 	  new (result) InstNoOp();
      }
      if (struc->tail != NULL) {
	CompilerStructure * s = struc->tail;
	int argCount = 0;
	do {
	  if (s->head->type == csINT) {
	    new (result) InstGetConstantInt(parseInt(s->head->value),
					    argCount);
	  } else if (s->head->type == csATOM) {
	    new (result) InstGetConstantAtom(s->head->value.c_str(),
					     argCount);
	  }
	  else if (s->head->type == csVARIABLE) {
	    if (firstOccurrence(s->head->value)) {
	      int v = substituteVariable(s->head->value);
	      new (result) InstGetVariable(v, argCount);
// 	      new (result) InstSetVariable(v);
// 	      new (result) InstGetValue(v, argCount);
	    }
	    else {
	      int v = substituteVariable(s->head->value);
	      new (result) InstGetValue(v, argCount);
	    }
	  } else { // csLIST || csSTRUCTURE
	    int subst = substituteVariable("");
	    new (result) InstGetVariable(subst, argCount);
// 	    new (result) InstSetVariable(subst);
// 	    new (result) InstGetValue(subst, argCount);
	    structureToCode(s->head, result);
	    new (result) InstUnifyVariable(subst, getLastVar());
	  }
	  argCount++;
	  s = s->tail;
	} while (s != NULL);
      }
    } break; // end of case struc.HEAD
    default:
      cerr << "Something bad happens here" << endl;
      return true;
    }
    return true;
  }

  bool Compiler::clauseToCode(const CompilerStructure *struc,
			      Program &result)
  {
    size_t initialPosition = result.getTop();
    switch (struc->type) {
    case csCLAUSE: {
      substitutionList.clear();
      bodyCalls = 0;
      //        lastCallPosition = -1;
      headToCode(struc->head, result);
      structureToCode(struc->tail, result);
      if ((substitutionList.numVars() > 0) || (bodyCalls > 0)) {
	size_t nextInitialPosition = initialPosition;
	ByteCode op = result.getInstruction(nextInitialPosition)->byteCode;
	if (op==opTrustMe || op==opTryMeElse || op==opRetryMeElse)
	  result.incProgramPointer(nextInitialPosition);
	size_t lastInstPos = result.getTop() - result.getLastSize();
	op = result.getInstruction(lastInstPos)->byteCode;
	if (op==opCall) { // LCO
	  result.getInstruction(lastInstPos)->byteCode=opExecute;
	  if (lastInstPos != nextInitialPosition) {
	    result.reserveSpace(lastInstPos, sizeof(InstDeallocate));
	    new (result.getInstruction(lastInstPos)) InstDeallocate();
	    result.reserveSpace(nextInitialPosition, sizeof(InstAllocate));
	    new (result.getInstruction(nextInitialPosition))
	      InstAllocate(getVarCount());
	  }
	} else {
	  result.reserveSpace(nextInitialPosition, sizeof(InstAllocate));
	  new (result.getInstruction(nextInitialPosition))
	    InstAllocate(getVarCount());
	  new (result) InstDeallocate();
	  new (result) InstProceed();
	}
      } else
	new (result) InstProceed();
      substitutionList.clear();
    } break; // end of case struc.CLAUSE
    default:
      return false;
    }
    return true;
  }
  
  bool Compiler::bodyToCode(const CompilerStructure *struc,
			    Program &result)
  {
    if (struc==NULL)
      return false;
    size_t initialPosition = result.getTop();
    switch(struc->type) {
    case csBODY: {
      CompilerStructure const *s = struc;
      do {
	if (s->head->type == csCUT) {
	  int y = substituteVariable("");
	  result.reserveSpace(initialPosition, sizeof(InstGetLevel));
	  new (result.getInstruction(initialPosition)) InstGetLevel(y);
	  new (result) InstCut(y);
	} else {
	  structureToCode(s->head, result);
	}
	s = s->tail;
      } while (s != NULL);
    } break; // end of case struc.BODY
    default:
      return false;
    }
    return true;
  }

  bool Compiler::structureToCode(const CompilerStructure &struc,
				 Program &result)
  {
    switch(struc.type) {
    case csPROGRAM: {
      if (struc.head == NULL) return false;
      clauseToCode(struc.head, result);
      structureToCode(struc.tail, result);
    } break; // end of case struc.PROGRAM
    case csHEAD:
      // for now this line is required for execute a query, should be revised:
      headToCode(&struc, result);
      break;
    case csBODY:
      bodyToCode(&struc, result);
      break;
    case csCALL:
      //      case csNOT_CALL:
      {
        bodyCalls++;
        int argCount = 0;
        if (struc.tail != NULL) {
          CompilerStructure *s = struc.tail;
          do {
            if (s->head->type == csINT) {
	      new (result) InstPutAConstantInt(parseInt(s->head->value),
					       argCount);
	    }
	    else if (s->head->type == csATOM) {
	      new (result) InstPutAConstantAtom(s->head->value.c_str(),
						argCount);
            }
            else if (s->head->type == csVARIABLE) {
              createVariableIfRequired(s->head, result);
	      new (result)
		InstPutValue(substituteVariable(s->head->value.c_str()),
			     argCount);
            } else {
              structureToCode(s->head, result);
	      new (result) InstPutValue(getLastVar(), argCount);
            }
            argCount++;
            s = s->tail;
          } while (s != NULL);
        }
        /*
	  lastCallPosition = result.getInstructionCount();
	  if (struc.type == csCALL) {
	*/
	if ((struc.head->value == "profile_init") && (argCount == 1)) {
	  new (result) InstProfileInit();
	} else if ((struc.head->value == "profile_end") && (argCount == 0)) {
	  new (result) InstProfileEnd();
	} else if ((struc.head->value == "profile") && (argCount == 2)) {
	  new (result) InstProfile();
	  new (result) InstProfileEnd();
	} else if ((struc.head->value == "time_init") && (argCount == 0)) {
	  new (result) InstTimeInit();
	} else if ((struc.head->value == "time_end") && (argCount == 0)) {
	  new (result) InstTimeEnd();
	} else if ((struc.head->value == "time") && (argCount == 1)) {
	  new (result) InstTime();
	  new (result) InstTimeEnd();
	} else if ((struc.head->value == "true") && (argCount == 0)) {
	  // do nothing
// 	} else if ((struc.head->value == "wam_proceed") && (argCount == 0)) {
// 	  new (result) InstProceed();
	} else if ((struc.head->value == "fail") && (argCount == 0)) {
	  new (result) InstFail();
	} else {
	  ClauseKey key(struc.head->value.c_str(), argCount, 1);
	  if (result.builtins.find(key)!=result.builtins.end()) {
	    new (result) InstCallBuiltin(result.builtins[key]);
	  } else {
	    new (result) InstCall((size_t)-1, struc.head->value.c_str(),
				  argCount);
	  }
	}
      } break; // end of case csCALL / csNOT_CALL
    case csUNIFICATION: {
      if (struc.head->type == csVARIABLE && struc.tail->type == csATOM
	  && firstOccurrence(struc.head->value))
	new (result)
	  InstPutConstantAtom(struc.tail->value.c_str(),
			      substituteVariable(struc.head->value));
      else if (struc.head->type == csVARIABLE && struc.tail->type == csINT
	       && firstOccurrence(struc.head->value))
	new (result)
	  InstPutConstantInt(parseInt(struc.tail->value),
			     substituteVariable(struc.head->value));
      else if (struc.tail->type == csVARIABLE && struc.head->type == csATOM
	       && firstOccurrence(struc.tail->value))
	new (result)
	  InstPutConstantAtom(struc.head->value.c_str(),
			      substituteVariable(struc.tail->value));
      else if (struc.tail->type == csVARIABLE && struc.head->type == csINT
	       && firstOccurrence(struc.tail->value))
	new (result)
	  InstPutConstantInt(parseInt(struc.head->value),
			     substituteVariable(struc.tail->value));
      else {
	structureToCode(struc.head, result);
	int headVar = getLastVar();
	structureToCode(struc.tail, result);
	int tailVar = getLastVar();
	new (result) InstUnifyVariable(headVar, tailVar);
      }
    } break; // end of case struc.UNIFICATION
    case csATOM: {
      //       cerr << "*** We should eliminate this segment ***" << endl;
      new (result) InstPutConstantAtom(struc.value.c_str(),
				       substituteVariable(""));
    } break;
    case csINT: {
      //       cerr << "*** We should eliminate this segment ***" << endl;
      new (result) InstPutConstantInt(parseInt(struc.value),
				      substituteVariable(""));
    } break;
    case csVARIABLE: {
      createVariableIfRequired(&struc, result);
    } break;
    case csSTRUCTURE: {
      vector<int> hv;
      vector<int> vv;
      int i, j = 0;
      const CompilerStructure *arg;
      int v = substituteVariable("");
      new (result)
	InstGetStruct(struc.head->value.c_str(),
		      (int)struc.tail->args.size(), v);
      for (i = 0; i < (int)struc.tail->args.size(); i++) {
	arg = struc.tail->args[i];
	if (arg->type==csATOM) {
	  new (result) InstSetConstantAtom(arg->value.c_str());
	} else if (arg->type==csINT) {
	  new (result) InstSetConstantInt(parseInt(arg->value));
	} else if (arg->type==csVARIABLE) {
	  if (firstOccurrence(arg->value)) {
	    int v1 = substituteVariable(arg->value);
	    new (result) InstSetVariable(v1);
	  } else {
	    int v1 = substituteVariable(arg->value);
	    int v2 = substituteVariable("");
	    new (result) InstSetVariable(v2);
	    new (result) InstUnifyVariable(v1, v2);
	  }
	} else {
	  vv.push_back(substituteVariable(""));
	  new (result) InstSetVariable(vv[j]);
	  hv.push_back(i);
	  j++;
	}
      }
      for (i = 0; i < j; i++) {
	arg = struc.tail->args[hv[i]];
	structureToCode(arg, result);
	new (result) InstUnifyVariable(getLastVar(), vv[i]);
      }
      setLastVar(v);
      return true;
    };
    case csLIST: {
      listToCode(&struc, result);
      return true;
    }
    case csQUERY: {
      if (struc.head == NULL)
	return false;
      structureToCode(struc.head, result);
      if (struc.tail != NULL) {
	structureToCode(struc.tail, result);
      }
      new (result) InstHalt();
    } break; // end of case struc.BODY
    case csCOMPARISON: {
      structureToCode(struc.head, result);
      int headVar = getLastVar();
      structureToCode(struc.tail, result);
      int tailVar = getLastVar();
      if (struc.value==">") {
	new (result) InstCompare<opSmaller>(tailVar, headVar);
      }
      else if (struc.value=="<") {
	new (result) InstCompare<opSmaller>(headVar, tailVar);
      }
      else if (struc.value==">=") {
	new (result) InstCompare<opSmallerEq>(tailVar, headVar);
      }
      else if (struc.value=="<=") {
	new (result) InstCompare<opSmallerEq>(headVar, tailVar);
      }
      else if (struc.value == "!=") {
	new (result) InstUnequal(headVar, tailVar);
      }
      else if (struc.value == "==") {
	new (result) InstEqual(headVar, tailVar);
      }
    } break; // end of case struc.COMPARISON
    case csASSIGNMENT: {
      structureToCode(struc.tail->head, result);
      int headVar = getLastVar();
      structureToCode(struc.tail->tail, result);
      int tailVar = getLastVar();
      structureToCode(struc.head, result);
      if (struc.tail->value=="+")
	new (result) InstArith<'+'>(getLastVar(), headVar, tailVar);
      else if (struc.tail->value=="-")
	new (result) InstArith<'-'>(getLastVar(), headVar, tailVar);
      else if (struc.tail->value=="*")
	new (result) InstArith<'*'>(getLastVar(), headVar, tailVar);
      else if (struc.tail->value=="/")
	new (result) InstArith<'/'>(getLastVar(), headVar, tailVar);
      else if (struc.tail->value=="mod")
	new (result) InstArith<'%'>(getLastVar(), headVar, tailVar);
    } break; // end of case struc.COMPARISON
    default:
      return true;
    }
    return true;
  };
}

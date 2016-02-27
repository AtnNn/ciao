/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * Compiler.java contains the base class Compiler, which both QueryCompiler and
 * PrologCompiler have been derived from.
 ******************************************************************************/


#ifndef _Compiler_H_
#define _Compiler_H_

#include <string>
#include <vector>
#include <map>

using namespace std;

namespace wam {
  
  class Engine;
  class CompilerStructure;
  class Program;
  
  class SubstitutionList {
    map<string, int> nameVars;
    map<int, int *> varReferences;
    size_t varCount;
  public:
    int lastVar;
    void clear() {
      varCount = 0;
      nameVars.clear();
      varReferences.clear();
    }
    size_t numVars() const {
      return nameVars.size();
    }
    size_t getVarCount() const {
      return varCount;
    }
    int substituteVariable(const string &variable);
    bool firstOccurrence(const string &variable);
  };
  
  class Compiler {
  public:
    Engine *owner;
    string errorString;
    SubstitutionList substitutionList;
    int bodyCalls;
    
    virtual ~Compiler() {
      substitutionList.clear();
    };
    
    int getLastVar() const { return substitutionList.lastVar; };
    void setLastVar(int aLastVar) { substitutionList.lastVar = aLastVar; };
    size_t getVarCount() const { return substitutionList.getVarCount(); };
    bool isPredicate(const string &s);
    bool isVariable(const string &s);
    bool isConstant(const string &s);
    bool isAtom(const string &s);
    bool predicate(const vector<string> &prog, size_t &index,
		   CompilerStructure &struc);
    bool constant(const vector<string> &prog, size_t &index,
		  CompilerStructure &struc);
    bool variable(const vector<string> &prog, size_t &index,
		  CompilerStructure &struc);
    bool structure(const vector<string> &prog, size_t &index,
		   CompilerStructure &struc);
    bool element(const vector<string> &prog, size_t &index,
		 CompilerStructure &struc);
    bool isNextToken(const vector<string> &prog, const size_t index,
		     const string &tok);
    bool token(const vector<string> &prog, size_t &index, const string &tok);
    bool atom(const vector<string> &prog, size_t &index,
	      CompilerStructure &struc);
    bool expression(const vector<string> &prog, size_t &index,
		    CompilerStructure &struc);
    bool literal(const vector<string> &prog, size_t &index,
		   CompilerStructure &struc);
    bool body(const vector<string> &prog, size_t &index,
	      CompilerStructure &struc);
    bool clause(const vector<string> &prog, size_t &index,
		CompilerStructure &struc);
    bool program(const vector<string> &prog, size_t &index,
		 CompilerStructure &struc);
    bool head(const vector<string> &prog, size_t &index,
	      CompilerStructure &struc);
    bool list(const vector<string> &prog, size_t &index,
	      CompilerStructure &struc, int &arity);
    bool list(const vector<string> &prog, size_t &index,
	      CompilerStructure &struc) {
      int _x;
      return list(prog, index, struc, _x);
    };
    bool args(const vector<string> &prog, size_t &index,
	      CompilerStructure &struc);
    bool stringToList(const string &text, vector<string> &result);
    int substituteVariable(const string &variable);
    bool firstOccurrence(const string &variable);
    
    // structureToCode takes a CompilerStructure, generated by the
    // parser, and constructs a WAM program from it, recursively
    bool structureToCode(const CompilerStructure *struc, Program &result);
    bool structureToCode(const CompilerStructure &struc, Program &result);
    bool headToCode(const CompilerStructure *, Program &result);
    bool bodyToCode(const CompilerStructure *, Program &result);
    bool clauseToCode(const CompilerStructure *, Program &result);
    bool listToCode(const CompilerStructure *, Program &result);
    
    virtual void createQueryVariableIfRequired(const CompilerStructure *s,
					       Program &result) = 0;
    void createVariableIfRequired(const CompilerStructure *s,
				  Program &result);
    virtual bool compile(const string &code, Program &prog) = 0;
    
  };
  
}

#endif // _Compiler_H_
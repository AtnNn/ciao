/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * QueryCompiler.java contains the QueryCompiler class, a child-class of
 * the Compiler class. QueryCompiler compiles user-written queries into WAM
 * code for execution.
 ******************************************************************************/


#ifndef CLASS_WAM_QUERYCOMPILER
#define CLASS_WAM_QUERYCOMPILER

#include <vector>
#include <string>

#include "Compiler.h"

using namespace std;

namespace wam {
  
  class CompilerStructure;
  class Program;
  class WAM;
  
  class QueryCompiler : public Compiler {
  public:
    
    QueryCompiler(Engine *anOwner);
    
    bool query(const vector<string> &prog, size_t &index, CompilerStructure &struc);
    
    string vectorToString(vector<string> &v);
    
    virtual bool compile(const string &aQuery, Program &prog);
    
    virtual void createQueryVariableIfRequired(const CompilerStructure *s,
					       Program &result);
    
  };
  
}

#include "Program.h"

#endif


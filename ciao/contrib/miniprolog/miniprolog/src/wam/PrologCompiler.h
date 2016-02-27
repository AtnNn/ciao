/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * PrologCompiler.java contains the class PrologCompiler, which transforms
 * a Prolog program (given as a string or by its filename) into an equivalent
 * WAM program.
 ******************************************************************************/


#ifndef _PrologCompiler_H_
#define _PrologCompiler_H_

#include "Compiler.h"

#include <stdio.h>

#include <vector>
#include <string>
#include <iostream>

using namespace std;


namespace wam {
  
  class CompilerStructure;
  class Program;
  class WAM;
  
  class PrologCompiler : public Compiler {
  public:
    
    PrologCompiler(Engine *anOwner);
    
    virtual bool compile(const string &programCode, Program &prog);
    
    // compileSimpleClause can be used in order to implement
    // assert(...) operations
    bool compileSimpleClause(const string &programCode, Program &result);
    
    bool compileFile(const string &fileName, Program &prog) {
      bool compiled = doCompileFile(fileName, prog);
      if (!compiled)
	if (fileName.find(".pl") == string::npos) {  // if compilation
	                                             // didn't work, try
	                                             // with different
                                                     // file extension
	  compiled = doCompileFile(fileName + ".pl", prog);
	}
      return compiled;
    }
    bool doCompileFile(const string &fileName, Program &prog);

    string readCode(FILE *r);
    
    void updateNames(CompilerStructure &struc);
    
    virtual void createQueryVariableIfRequired(const CompilerStructure *s,
					       Program &result);
    
  };
  
}

#endif // _PrologCompiler_H_

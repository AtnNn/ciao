/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * PrologCompiler.java contains the class PrologCompiler, which transforms
 * a Prolog program (given as a string or by its filename) into an equivalent
 * WAM program.
 ******************************************************************************/

#include <map>
#include <vector>
#include <sstream>
#include <string>
#include <iostream>

#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "WAMUtils.h"
#include "PrologCompiler.h"
#include "CompilerStructure.h"
#include "InstructionSpec.h"

using namespace wam;
using namespace std;

namespace wam {
  
  
  PrologCompiler::PrologCompiler(Engine *anOwner) {
    owner = anOwner;
    errorString = "";
  };
  
  void PrologCompiler::createQueryVariableIfRequired(const CompilerStructure *,
						     Program &)
  {
    // nothing to do
  }
  
  bool PrologCompiler::compile(const string &programCode, Program &prog) {
    DO_DEBUG(uint64 ms = hrtime(), 3);
    DO_DEBUG(cerr << "Program Code:" << endl, 2);
    DO_DEBUG(cerr << programCode << endl, 2);
    vector<string> programList;
    size_t index = 0;
    if (!stringToList(programCode, programList))
      return false;
    DO_DEBUG(cerr << "Program List:" << endl, 2);
    DO_DEBUG(cerr << "String to List: " << (hrtime() - ms) << " ms." << endl, 3);
//     DO_DEBUG(cerr << programList << endl, 2);
    CompilerStructure struc;
    bool compiled;
    DO_DEBUG(ms = hrtime(), 3);
    compiled = (program(programList, index, struc)) && (programList.size() == index);
    if (compiled) {
      DO_DEBUG(cerr << "List to Structure: " << (hrtime() - ms) << " ms." << endl, 3);
      updateNames(struc);
      DO_DEBUG(cerr << struc << endl, 2);
      //            ms = hrtime();
      //            Program * p = structureToCode(struc);
      //            DO_DEBUG(cerr << "Structure to Code: " << (hrtime() - ms) << " ms." << endl, 0);
      return structureToCode(struc, prog);
    } else {
      if (errorString.size() > 0)
        cerr << errorString << endl;
      return false;
    }
  };
  
  // compileSimpleClause can be used in order to implement assert(...) operations
  bool PrologCompiler::compileSimpleClause(const string &programCode, Program &result) {
    vector<string> programList;
    size_t index = 0;
    stringToList(programCode, programList);
    CompilerStructure struc;
    bool compiled = (clause(programList, index, struc)) && (programList.size() == index);
    if (compiled) {
      CompilerStructure program;
      program.type = csPROGRAM;
      program.head = &struc;
      updateNames(program);
      return structureToCode(&program, result);
    } else {
      return false;
    }
  };
  
  bool PrologCompiler::doCompileFile(const string &fileName, Program &prog) {
    string code;
    string dummy;
    DO_DEBUG(uint64 ms = hrtime(), 3);
    FILE *r;
    r = fopen(fileName.c_str(), "r");
    if (r == NULL) {
      return false;
    }
    code = readCode(r);
    DO_DEBUG(cerr << "File Operations: " << (hrtime() - ms) << " ms." << endl, 3);
    fclose(r);
    return compile(code, prog);
  };
  
  string PrologCompiler::readCode(FILE *r) {
    string s = "";
    char c;
    while (fread(&c, sizeof(char), 1, r)== sizeof(char)) {
      s += c;
    }
    return s;
  };

  void PrologCompiler::updateNames(CompilerStructure &struc) {
    map<string, int> procedureCount;
    CompilerStructure *s, *proc;
    if ((struc.type == csPROGRAM) && (struc.head != NULL)) {
      s = &struc;
      do {
        proc = s->head->head->head;
	proc->number = ++procedureCount[proc->value + "/" + toString(proc->arity)];
        s = s->tail;
      } while (s != NULL);
    }
    if ((struc.type == csPROGRAM) && (struc.head != NULL)) {
      s = &struc;
      do {
        proc = s->head->head->head;
        proc->count = procedureCount[proc->value + "/" + toString(proc->arity)];
        s = s->tail;
      } while (s != NULL);
    }
  };
}

/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * QueryCompiler.java contains the QueryCompiler class, a child-class of
 * the Compiler class. QueryCompiler compiles user-written queries into WAM
 * code for execution.
 ******************************************************************************/

#include <iostream>

#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "WAMUtils.h"
#include "CompilerStructure.h"
#include "QueryCompiler.h"
#include "InstructionSpec.h"

using namespace wam;

namespace wam {
  
  QueryCompiler::QueryCompiler(Engine *anOwner) {
    owner = anOwner;
    errorString = "";
  }
  
  void QueryCompiler::createQueryVariableIfRequired(const CompilerStructure *s,
						    Program &result) {
    new (result) InstCreateQVariable(substituteVariable(s->value),
				     s->value.c_str());
  }
  
  bool QueryCompiler::query(const vector<string> &prog, size_t &index, CompilerStructure &struc) {
    size_t oldIndex = index;
    struc.type = csQUERY;
    struc.setHead(new CompilerStructure());
    struc.setTail(new CompilerStructure());
    if ((body(prog, index, *struc.tail)) && (token(prog, index, "."))) {
      struc.head->type = csHEAD;
      struc.head->setHead(new CompilerStructure());
      struc.head->head->type = csPREDICATE;
      struc.head->head->value = "query$";
      struc.head->head->arity = 0;
      struc.head->head->number = 1;
      struc.head->head->count = 1;
      struc.head->setTail(NULL);
      return true;
    }
    index = oldIndex;
    return false;
  }; // end of QueryCompiler.query(Vector, CompilerStructure)
  
  string QueryCompiler::vectorToString(vector<string> &v) {
    string result = "[";
    for (size_t i=0; i < v.size(); i++) {
      result += v[i] + ",";
    }
    return result + "]";
  };
  
  bool QueryCompiler::compile(const string &aQuery, Program &prog) {
    vector<string> queryList;
    size_t index = 0;
    stringToList(aQuery, queryList);
    CompilerStructure struc;
    errorString = "";
//     DO_DEBUG(cerr << "List:      " << queryList << endl, 2);
    if (query(queryList, index, struc)) {
      if (queryList.size() != index) {
        if (errorString.size() > 0)
          cout << errorString;
        return false;
      }
      DO_DEBUG(cerr << "Structure: " << struc << endl, 2);
      substitutionList.clear();
      return structureToCode(struc, prog);
    }
    if (errorString.size() > 0)
      cout << errorString;
    return false;
  };
}

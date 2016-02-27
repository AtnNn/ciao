/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * Program.java contains the WAM program management class Program. A Program
 * consists of an array of Instructions (cf. Instruction.java).
 ******************************************************************************/

#include <sstream>
#include <iostream>

#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "WAMUtils.h"
#include "Program.h"
#include "InstructionSpec.h"
#include "ByteCode.h"
#include "glue_map.h"

using namespace std;
using namespace wam;

namespace wam {

  Program::Program(void *memory) : MemSegment(memory) {
    
    builtins[ClauseKey("call", 1, 1)] = (size_t)callCall;

    functions[ClauseKey("atom",         1, 1)] = (size_t)btIsAtom;
    functions[ClauseKey("atom_concat",  3, 1)] = (size_t)btAtomConcat;
    functions[ClauseKey("atom_number",  2, 1)] = (size_t)btAtomNumber;
    functions[ClauseKey("atoms",        0, 1)] = (size_t)btAtoms;
    functions[ClauseKey("atomic",       1, 1)] = (size_t)btIsAtomic;
    functions[ClauseKey("integer",      1, 1)] = (size_t)btIsInteger;
    functions[ClauseKey("var",          1, 1)] = (size_t)btIsVar;
    functions[ClauseKey("write",        1, 1)] = (size_t)btWrite1;
    functions[ClauseKey("write",        2, 1)] = (size_t)btWrite2;
    functions[ClauseKey("open",         3, 1)] = (size_t)btOpen;
    functions[ClauseKey("open_input",   2, 1)] = (size_t)btOpenInput;
    functions[ClauseKey("open_output",  2, 1)] = (size_t)btOpenOutput;
    functions[ClauseKey("close",        1, 1)] = (size_t)btClose;
    functions[ClauseKey("close_input",  1, 1)] = (size_t)btCloseInput;
    functions[ClauseKey("close_output", 1, 1)] = (size_t)btCloseOutput;
    functions[ClauseKey("delete_file",  1, 1)] = (size_t)btDeleteFile;
    functions[ClauseKey("writeln",      1, 1)] = (size_t)btWriteLn1;
    functions[ClauseKey("writeln",      2, 1)] = (size_t)btWriteLn2;
    functions[ClauseKey("nl",           0, 1)] = (size_t)btNewLine0;
    functions[ClauseKey("nl",           1, 1)] = (size_t)btNewLine1;
    functions[ClauseKey("newline",      0, 1)] = (size_t)btNewLine0;
    functions[ClauseKey("newline",      1, 1)] = (size_t)btNewLine1;
    functions[ClauseKey("consult",      1, 1)] = (size_t)btConsult;
    functions[ClauseKey("savewamcode",  1, 1)] = (size_t)btSaveWAMCode;
    /* 	functions[ClauseKey("reconsult",    1, 1)] = (size_t)btReconsult; */
    functions[ClauseKey("readln",       1, 1)] = (size_t)btReadLn1;
    functions[ClauseKey("readln",       2, 1)] = (size_t)btReadLn2;
    functions[ClauseKey("hrtime",       2, 1)] = (size_t)btHRTime;
    functions[ClauseKey("time_interval",2, 1)] = (size_t)btTimeInterval;
    functions[ClauseKey("limits",       0, 1)] = (size_t)btLimits;
    functions[ClauseKey("status",       0, 1)] = (size_t)btStatus;
    functions[ClauseKey("halt",         0, 1)] = (size_t)btHalt;
    functions[ClauseKey("procedures",   0, 1)] = (size_t)btProcedures;
    functions[ClauseKey("rename_file",  2, 1)] = (size_t)btRenameFile;
    functions[ClauseKey("termtofile",   2, 1)] = (size_t)btTermToFile;
    functions[ClauseKey("help",         0, 1)] = (size_t)btHelp;
    functions[ClauseKey("labels",       0, 1)] = (size_t)btLabels;
//     functions[ClauseKey("atoms",        0, 1)] = (size_t)btAtoms;
    functions[ClauseKey("wamcode",      0, 1)] = (size_t)btWAMCode;
    functions[ClauseKey("intcode",      0, 1)] = (size_t)btIntCode;
    functions[ClauseKey("showgclist",   0, 1)] = (size_t)btShowGCList;
    functions[ClauseKey("tuneprof",     3, 1)] = (size_t)btTuneProfiler;
    functions[ClauseKey("sub2",         6, 1)] = (size_t)btSub2;
  }
  
  // Program class manages WAM programs, consisting of list (vector)
  // of instructions
  void Program::addProgram(Program &p) {
    //size_t cnt = p.getTop();
    for (labels_type::iterator i = p.labels.begin();
	 i != p.labels.end(); i++) {
      if (labels.find(i->first)!=labels.end()) {
	cerr << "Error: Multiple occurrence of label \"" << i->second
	     << " at "  << i->first << "\". Use reconsult." << endl;
	return;
      }
      else {
	labels[i->first] = getTop() + i->second;
      }
    }
    memcpy(getTopAddress(), p.getAddress(0), p.getTop());
    incTop(p.getTop());
  }
  
  void Program::deleteFromLine(size_t lineNumber) {
    if (lineNumber != (size_t)(-1)) {
      size_t i = lineNumber;
      labels.erase(getLabel(i));
      while (i < getTop()) {
	if (getLabel(i).name[0]!='\0') {
	  memmove(getInstruction(lineNumber), getInstruction(i), getTop() - i);
	  decTop(i - lineNumber);
	  for (labels_type::iterator k = labels.begin();
	       k != labels.end(); k++) {
	    if ((k->second != (size_t)(-1)) && (k->second > lineNumber)) {
	      k->second -= i - lineNumber;
	    }
	  }
	  updateLabels();
	  return;
	}
	incProgramPointer(i);
      }
      setTop(lineNumber);
      updateLabels();
    }
  }
  
  size_t Program::getLabelIndex(const ClauseKey &label) {
    if (labels.find(label)!=labels.end())
      return labels[label];
    return (size_t)(-1);
  }
  
  // updateLabels converts String label names in call, try_me_else and
  // retry_me_else instructions to integer values. internal predicates
  // (e.g. write, consult) are transformed to negative line numbers
  void Program::updateLabels() {
    //    labels = new DHashtable();
    size_t i = 0;
    while (i < getTop()) {
      Instruction *s0 = getInstruction(i);
      if ((s0->byteCode==opCall) ||
	  (s0->byteCode==opExecute) ||
	  (s0->byteCode==opCallFunction) ||
	  //	  (s0->byteCode==opNotCall) ||
	  (s0->byteCode==opTryMeElse) ||
	  (s0->byteCode==opRetryMeElse)) {
	InstructionLabel *s = (InstructionLabel *)s0;
	ClauseKey label(s->getLabel());
	if (labels.find(label) != labels.end()) {
	  s->setAddress((size_t)labels[label]);
	}
	else { // label is undefined or a built-in predicate
	  if (functions.find(label) != functions.end())
	    {
	      if (s->byteCode==opCall)
		s->byteCode = opCallFunction;
	      else if (s->byteCode==opExecute)
		s->byteCode = opExecuteFunction;
	      ((InstCallBase *)s)
		->setFunction((bool (*)(Engine *))functions[label]);
	    }
	  else
	    {
	      if (s->byteCode==opCallFunction)
		s->byteCode = opCall;
	      s->setAddress((size_t)(-1));
	      cerr << "WARNING: undefined " << s->getLabel() << endl;
	    }
	}
      }
      incProgramPointer(i);
    }
  }
  
  ostream& Program::outputLabel(ostream &os, Instruction *p) {
    ClauseKey label(getLabel(getIndex(p)));
    string keyname;
    if (label.name==";")
      os << "; " << p->getFunction();
    if (label.name[0]!='\0') {
      os << keyname << ": ";
      if (keyname.size() < 12)
	for (size_t i = 1; i <= 12 - keyname.size(); i++)
	  os << " ";
    } else
      os << "              ";
    return os;
  }
  
  ostream& Program::dumpWAMCode(ostream& os) {
    size_t i = 0;
    Instruction *ins;
    while (i < getTop()) {
      ins = getInstruction(i);
      labels_type::iterator l = locateLabel(i);
      if (l != labels.end()) {
	if (i != 0)
	  os << " ] )." << endl << endl;
	os << "wamcode_db( '" << l->first.name << "/" << l->first.arity
	   << "/" << l->first.number << "', [" << endl;
      } else
	os << "," << endl;
      os << "\t    i( " << i << ", " << *ins << " )";
      incProgramPointer(i);
    }
    if (i!=0)
      os << " ] )." << endl;
    return os;
  }
  
  ostream& Program::dumpIntCode(ostream& os) {
    size_t i = 0;
    Instruction *ins;
    while (i < getTop()) {
      ins = getInstruction(i);
      labels_type::iterator l = locateLabel(i);
      if (l != labels.end()) {
	if (i != 0)
	  os << " ] )." << endl << endl;
	os << "'" << l->first.name << "'";
	if (l->first.arity > 0) {
	  os << "(A0" ;
	  for (size_t j = 1; j < l->first.arity; j++)
	    os <<", A" << j;
	  os << ")";
	}
	os << " :- % clause " << l->first.number << endl;
      } else
	os << "," << endl;
      os << "\t    " << *ins; // << " % " << i ;
      incProgramPointer(i);
    }
    if (i!=0)
      os << " ] )." << endl;
    return os;
  }
}

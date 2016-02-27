// this file should not use any file that requires the files that this
// program generates.

#include <iomanip>
#include <iostream>
#include <fstream>

#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

using namespace std;

enum ByteCodeExecType {
  etCanFail,
  etNotFail,
  etCtCFail, // Control Can Fail
  etControl,
  etHalt
};

typedef char * PChar;

typedef const char * CPChar;

struct ByteCodeDescriptor {
  const char *name;         // Name in java-like format, example
			    // getConstant
  size_t arity;             // Number of arguments the bytecode
			    // requires
  ByteCodeExecType execType;
  bool returnFromLoop;
  bool initNewLiteral;
  bool calibrated;
  CPChar *params; // Null terminated List of parameters considered
};

const char *Configure_h        = "Configure.h";
const char *Configure_cpp      = "Configure.cpp";
const char *EngineLoop_h       = "EngineLoop.h";
const char *miniprolog_auto_pl = "../miniprolog_auto.pl";
const char *InstructionBC_h    = "InstructionBC.h";


// Parameters that are considered in the profiler.  Note that some
// benchmarks are the same to several types.
CPChar unifyVariableParams[] = {"[[gnd,var]]",
			       "[[var,gnd]]",
			       "[[con1,con2]]",
			       //			       "[[atm,int],[atm,lst],[atm,str],[int,atm],[int,str],[int,lst],[lst,atm],[lst,int],[lst,str],[str,atm],[str,int],[str,lst]]",
			       "[[int,int]]",
			       "[[lst(1),lst(1)]]",
			       "[[atm,atm]]",
			       "[[str,str]]",
			       "[[lst(100),lst(100)]]",
			       NULL};

ByteCodeDescriptor bcd[] = {
  {"Unknown",          0, etNotFail, false, false, false, NULL}, //*
  {"Allocate",         0, etNotFail, false, false, false, NULL},
  {"ArithAdd",         3, etCanFail, false, true , true , NULL},
  {"ArithDiv",         3, etCanFail, false, true , true , NULL},
  {"ArithMod",         3, etCanFail, false, true , true , NULL},
  
  {"ArithMul",         3, etCanFail, false, true , true , NULL},
  {"ArithSub",         3, etCanFail, false, true , true , NULL},
  {"Bigger",           2, etCanFail, false, true , true , NULL},
  {"BiggerEq",         2, etCanFail, false, true , true , NULL},
  {"Call",             2, etCanFail, false, true , false, NULL},
  
  
  {"CallFunction",     2, etCanFail, false, true , false, NULL}, //*
  {"CallBuiltin",      1, etCanFail, false, true , false, NULL}, //*
  {"CreateQVariable",  2, etNotFail, false, false, false, NULL}, //*
  {"Cut",              1, etNotFail, false, true , true , NULL},
  {"Deallocate",       0, etNotFail, false, false, false, NULL},
  
  {"Equal",            2, etCanFail, false, true , true , NULL},
  {"Execute",          2, etCtCFail, false, true , true , NULL},
  {"ExecuteFunction",  2, etCtCFail, false, true , false, NULL},
  {"Fail",             0, etCanFail, false, true , false, NULL},
  {"GetConstantAtom",  2, etCanFail, false, false, true , NULL},


  {"GetConstantInt",   2, etCanFail, false, false, true , NULL},
  {"GetLevel",         1, etNotFail, false, false, false, NULL}, //*
  {"GetList",          2, etNotFail, false, false, true , NULL},
  {"GetStruct",        2, etNotFail, false, false, false, NULL},
  {"GetValue",         2, etCanFail, false, false, true , NULL},

  {"GetVariable",      2, etNotFail, false, false, true , NULL},
  {"Halt",             0, etHalt,    true,  false, false, NULL}, //*
  {"NoOp",             0, etNotFail, false, false, false, NULL}, //*
  {"Proceed",          0, etControl, false, false, false, NULL},
  {"Profile",          0, etCanFail, true,  false, false, NULL}, //*


  {"ProfileInit",      0, etCanFail, true,  false, false, NULL}, //*
  {"ProfileEnd",       0, etNotFail, true,  false, false, NULL}, //*
  {"Time",             0, etCanFail, true,  false, false, NULL}, //*
  {"TimeInit",         0, etNotFail, true,  false, false, NULL}, //*
  {"TimeEnd",          0, etNotFail, true,  false, false, NULL}, //*

  {"PutAConstantAtom", 2, etNotFail, false, false, true , NULL},
  {"PutAConstantInt",  2, etNotFail, false, false, true , NULL},
  {"PutConstantAtom",  2, etNotFail, false, false, true , NULL},
  {"PutConstantInt",   2, etNotFail, false, false, true , NULL},
  {"PutValue",         2, etNotFail, false, false, true , NULL},

  {"RetryMeElse",      2, etNotFail, false, false, false, NULL},
  {"SetConstantAtom",  1, etNotFail, false, false, true , NULL},
  {"SetConstantInt",   1, etNotFail, false, false, true , NULL},
  {"SetVariable",      1, etNotFail, false, false, true , NULL},
  {"Smaller",          2, etCanFail, false, true , true , NULL},

  {"SmallerEq",        2, etCanFail, false, true , true , NULL},
  {"TrustMe",          0, etNotFail, false, false, false, NULL},
  {"TryMeElse",        2, etNotFail, false, false, false, NULL},
  {"Unequal",          2, etCanFail, false, true , true , NULL},
  {"UnifyVariable",    2, etCanFail, false, false, true , unifyVariableParams},
  {NULL,               0, etControl, true,  false, false, NULL}  //*
};

const char *java_to_c_format(const char *name) {
  static char cname[64];
  char *p = cname;
  while(*name) {
    if (isupper(*name)) {
      if (p != cname)
	*p++ = '_';
      *p++ = (char)tolower(*name++);
    } else
      *p++ = *name++;
  }
  *p = '\0';
  return cname;
}

void genConfigure_h(void) {
  ofstream fConfigure_h(Configure_h);
  
  fConfigure_h
    << "#ifndef _Configure_H_" << endl
    << "#define _Configure_H_" << endl
    << endl
    << "namespace wam {" << endl
    << endl
    << "  enum ByteCode {" << endl;
  ByteCodeDescriptor *p;
  int i;
  for (i = 0, p = bcd; p->name != NULL; p++, i++) {
    if (p!=bcd) {
      fConfigure_h << "," << endl;
    }
    fConfigure_h << "    op" << p->name;
    if (p==bcd)
      fConfigure_h << "=0";
  };
  fConfigure_h
    << endl
    << "  };" << endl << endl
    << "  const size_t NUM_BYTECODES=" << i << ";" << endl;

  fConfigure_h
    << endl
    << "  const size_t numParameters[NUM_BYTECODES] = { ";

  for (p = bcd; p->name != NULL; p++) {
    if (p!=bcd) {
      fConfigure_h << ", ";
    }
    int k = 1;
    for (CPChar *j = p->params; j != NULL && *j != NULL; j++)
      k++;
    fConfigure_h << k;
  }
  
  fConfigure_h
    << " };" << endl;
  
  fConfigure_h
    << endl
    << "  const size_t numArguments[NUM_BYTECODES] = { ";

  for (p = bcd; p->name != NULL; p++) {
    if (p!=bcd) {
      fConfigure_h << ", ";
    }
    fConfigure_h << p->arity;
  }

  fConfigure_h
    << " };" << endl
    << "}" << endl
    << "#endif // _Configure_H_" << endl;
};

void genConfigure_cpp(void) {
  ofstream fConfigure_cpp(Configure_cpp);
  fConfigure_cpp
    << endl
    << "#include <string.h>" << endl
    << "#include <iostream>" << endl
    << endl
    << "#ifdef __BORLANDC__" << endl
    << "#pragma hdrstop" << endl
    << "#pragma package(smart_init)" << endl
    << "#endif" << endl
    << "" << endl
    << "#include \"Configure.h\"" << endl
    << "" << endl
    << "using namespace std;" << endl
    << "" << endl
    << "namespace wam {" << endl
    << "  "
    << endl
    << "  const char *byteCodeToFunction(ByteCode op) {" << endl
    << "    switch (op) {" << endl;
  for (int i = 0; bcd[i].name != NULL; i++) {
    fConfigure_cpp << "    case op" << left << setw(16) << bcd[i].name
		   << ": return \""
		   << java_to_c_format(bcd[i].name) << "\";" << endl;
  }
  fConfigure_cpp
    << "    default:" << endl
    << "      cerr << \"Invalid opcode: \" << op << endl;" << endl
    << "      return \"unknown\";" << endl
    << "    }" << endl
    << "  }" << endl
    << "  " << endl
    << "  ByteCode functionToByteCode(const char *function) {" << endl;
  for (int i = 0; bcd[i].name != NULL; i++) {
    fConfigure_cpp
      << "    if (!strcmp(function, \""
      << java_to_c_format(bcd[i].name)
      << "\")) return op" << bcd[i].name << ";" << endl;
  }
  fConfigure_cpp
    << "    return opUnknown;" << endl
    << "  };" << endl
    << "}" << endl
    << "" << endl;
}

void genEngineLoop_h() { 
  ofstream fEngineLoop_h(EngineLoop_h);
  fEngineLoop_h
    << "#define executeInstruction(pC, s) \\" << endl
    << "  { \\" << endl
    << "    DO_DEBUG(s->outInst(cerr << \"(\" << pC << \") \", this) << endl, 3); \\"
    << endl
    << "    switch(s->byteCode) { \\" << endl;
  for (int i = 0; bcd[i].name != NULL; i++) {
    fEngineLoop_h
      << "    case op" << left << setw(17) << bcd[i].name << ":";
    switch (bcd[i].execType) {
    case etCanFail:
      fEngineLoop_h
	<< "executeCanFail((Inst" << bcd[i].name << " *)s, pC, this);";
      break;
    case etNotFail:
      fEngineLoop_h
	<< "executeNotFail((Inst"  << bcd[i].name << " *)s, pC, this);";
      break;
    case etCtCFail:
      fEngineLoop_h
	<< "controlCanFail((Inst"  << bcd[i].name << " *)s, pC, this);";
      break;
    case etControl:
      fEngineLoop_h
	<< "controlNotFail((Inst"  << bcd[i].name << " *)s, pC, this);";
      break;
    case etHalt:
      fEngineLoop_h
	<< "bContinue = false;";
    }
    if (bcd[i].returnFromLoop)
      fEngineLoop_h << " return; \\" << endl;
    else
      fEngineLoop_h << " break; \\" << endl;
  }
  fEngineLoop_h
    << "    default: \\" << endl
    << "      { \\" << endl
    << "	cout << \"Invalid operation \" << s->byteCode << \" in line \" \\"
    << endl
    << "	     << pC << endl; \\" << endl
    << "	backtrack(pC); \\" << endl
    << "      } \\" << endl
    << "    } \\" << endl
    << "  }" << endl;
}

void genInstructionBC_h(void) {
  ofstream fInstructionBC_h(InstructionBC_h);
  fInstructionBC_h
    << "  _INLINE size_t byteCodeToSize(ByteCode op) {" << endl
    << "    switch (op) {" << endl;
  for (int i = 0; bcd[i].name != NULL; i++) {
    fInstructionBC_h << "    case " << "op" << left << setw(16) << bcd[i].name
		   << ": return sizeof(Inst" << bcd[i].name << ");" << endl;
  }
  fInstructionBC_h
    << "    default:" << endl
    << "      cerr << \"Invalid opcode: \" << op << endl;" << endl
    << "      return 0;" << endl
    << "    }" << endl
    << "  }" << endl;
}

void genMiniPrologAuto_pl(void) {
  ByteCodeDescriptor *p;
  ofstream fMiniPrologAuto_pl(miniprolog_auto_pl);
  fMiniPrologAuto_pl
    << ":- module(miniprolog_auto, _, [fsyntax, assertions])." << endl
    << endl
    << "can_fail := " << endl
    << "\t";
  bool first = true;
  for (p = bcd; p->name != NULL; p++) {
    if (p->execType==etCanFail) {
      if (!first) {
	fMiniPrologAuto_pl << endl << "\t|";
      } else
	first = false;
      fMiniPrologAuto_pl << java_to_c_format(p->name);
    }
  }
  fMiniPrologAuto_pl
    << "." << endl
    << endl
    << "init_new_literal :=" << endl
    << "\t";
  first = true;
  for (p = bcd; p->name != NULL; p++) {
    if (p->initNewLiteral) {
      if (!first) {
	fMiniPrologAuto_pl << endl << "\t|";
      } else
	first = false;
      fMiniPrologAuto_pl << java_to_c_format(p->name);
    }
  }
  fMiniPrologAuto_pl
    << "." << endl
    << endl;
  
  for (p = bcd; p->name != NULL; p++) {
    fMiniPrologAuto_pl
      << "bytecode(" << java_to_c_format(p->name) << ", "
      << p->arity << ")." << endl;
  }
  fMiniPrologAuto_pl << endl;
  
  for (p = bcd; p->name != NULL; p++) {
    if ( p->params != NULL ) {
      fMiniPrologAuto_pl
	<< "bc_params(" << java_to_c_format(p->name) << ") := " << endl
	<< "\t[" << endl;
      first = true;
      for (int j = 0; p->params[j] != NULL; j++) {
	if (!first) {
	  fMiniPrologAuto_pl << ",\n";
	} else
	  first = false;
	fMiniPrologAuto_pl << "\t    " << p->params[j];
      }
      fMiniPrologAuto_pl << endl <<"\t]." << endl;
    }
  }
}

int processFileName(const char *fileName) {
  if (!strcmp(fileName, Configure_h))
    genConfigure_h();
  else if (!strcmp(fileName, Configure_cpp))
    genConfigure_cpp();
  else if (!strcmp(fileName, EngineLoop_h))
    genEngineLoop_h();
  else if (!strcmp(fileName, InstructionBC_h))
    genInstructionBC_h();
  else if (!strcmp(fileName, miniprolog_auto_pl))
    genMiniPrologAuto_pl();
  else {
    cerr << "ERROR: Unrecognized file name `" << fileName << "\'." << endl;
    return (EXIT_FAILURE);
  }
  return (EXIT_SUCCESS);
}

void processAll() {
  genConfigure_h();
  genConfigure_cpp();
  genEngineLoop_h();
  genInstructionBC_h();
  genMiniPrologAuto_pl();
}

int main(int argc, char *argv[]) {
  if (argc>=2) {
    for (int i = 1; i < argc; i++) {
      if (processFileName(argv[i])==EXIT_FAILURE)
	return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
  }
  else if (argc == 1) {
    processAll();
  }
  else {
    cerr << "ERROR: Invalid number of arguments." << endl;
    return (EXIT_FAILURE);
  }
}

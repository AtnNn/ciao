#include <stdlib.h>
#include <iostream>
#include <fstream>

#ifdef __BORLANDC__
# pragma hdrstop
# pragma package(smart_init)
#endif

#include "Program.h"
#include "Functions.h"
#include "Engine.h"
#include "PrologCompiler.h"
#include "InstructionSpec.h"
#include "Profiler.h"
#include "compat.h"

using namespace std;

namespace wam {

  bool btIsAtom(Engine *w) {
    return w->getArg(0)->deref()->isAtom();
  }
  
  bool btTuneProfiler(Engine *w) {
    Variable *v = w->getArg(2)->deref();
    if (v->isInteger()) {
      profileOverhead = v->getInteger();
      return true;
    }
    return false;
  }
  
  bool btRenameFile(Engine *w) {
    Variable *v1, *v2;
    v1 = w->getArg(0)->deref();
    v2 = w->getArg(1)->deref();
    if (v1->isAtom() && v2->isAtom()) {
      const char *s1 = v1->getString();
      const char *s2 = v2->getString();
      return rename(s1, s2)==0;
    }
    return false;
  }

  bool btDeleteFile(Engine *w) {
    Variable *v1;
    v1 = w->getArg(0)->deref();
    if (v1->isAtom()) {
      const char *s1 = v1->getString();
      return _unlink(s1)==0;
    }
    return false;
  }

//     if (v->isFree()) {
//       Variable *vn = w->getArg(0)->deref();
//       Variable *vm = w->getArg(1)->deref();
//       if (vn->isInteger() && vm->isInteger()) {
// 	w->do_trail(v);
// 	v->setValue(tune_profiler((size_t)vn->getInteger(),
// 				  (size_t)vm->getInteger()));
// 	return true;
//       }
//     }
//     else if (v->isInteger()) {
//       profileOverhead = (int)v->getInteger();
//       return true;
//     }
//     return false;
//   }

  bool btIsInteger(Engine *w) {
    return w->getArg(0)->deref()->isInteger();
  }
  
  bool btIsAtomic(Engine *w) {
    return w->getArg(0)->deref()->isAtomic();
  }
  
  bool btIsVar(Engine *w) {
    Variable *v;
    v = w->getArg(0)->deref();
    return v->isFree();
  }
  
  bool btAtomConcat(Engine *w) {
    Variable *v1, *v2, *v3;
    v1 = w->getArg(0)->deref();
    v2 = w->getArg(1)->deref();
    v3 = w->getArg(2)->deref();
    if (v1->isAtom() && v2->isAtom()) {
      const char *s1 = v1->getString();
      const char *s2 = v2->getString();
      size_t l1 = strlen(s1);
      size_t l2 = strlen(s2);
      if (v3->isFree()) {
	size_t l = l1 + l2 + 1;
	char *m = new char[l];
	strcpy_s(m, l1 + 1, s1);
	strcpy_s(m + l1, l2 + 1, s2);
	w->do_trail(v3);
	v3->setValue(Atoms::insertValue(m));
	return true;
      } else if (v3->isAtom()) {
	const char *s3 = v3->getString();
	return strncmp(s1, s3, l1)==0 && strcmp(s2, s3 + l1)==0;
      }
    } else if (v1->isFree() && v2->isAtom() && v3->isAtom()) {
      const char *s2 = v2->getString();
      const char *s3 = v3->getString();
      size_t l2 = strlen(s2);
      size_t l3 = strlen(s3);
      if ((l2 <= l3) && strcmp(s2, s3 + l3 - l2)==0) {
	size_t l = l3 - l2 + 1;
	char *m = new char[l];
	strncpy_s(m, l, s3, l3 - l2);
	m[l3 - l2] = '\0';
	v1->setValue(Atoms::insertValue(m));
	return true;
      }
    }
    else if (v1->isAtom() && v2->isFree() && v3->isAtom()) {
      const char *s1 = v1->getString();
      const char *s3 = v3->getString();
      size_t l1 = strlen(s1);
      size_t l3 = strlen(s3);
      if ((l1 <= l3) && strncmp(s1, s3, l1)==0) {
	size_t l = l3 - l1 + 1;
	char *m = new char[l];
	strcpy_s(m, l, s3 + l1);
	v2->setValue(Atoms::insertValue(m));
	return true;
      }
    }
    return false;
  }
  
  bool btAtomNumber(Engine *w) {
    Variable *v1, *v2;
    v1 = w->getArg(0)->deref();
    v2 = w->getArg(1)->deref();
    if (v1->isAtom() && v2->isFree()) {
      w->do_trail(v2);
      v2->setValue(atoi(v1->getString()));
      return true;
    } else if (v1->isFree() && v2->isInteger()) {
      char buff[64];
      sprintf(buff, "%ld", (long)v2->getInteger());
      w->do_trail(v1);
      v1->copyValue(buff);
      return true;
    }
    return false;
  }
  
  bool btWrite1(Engine *w) {
    Variable *v;
    v = w->getArg(0)->deref();
    *w->eout << *v;
    return true;
  }

  bool btWrite2(Engine *w) {
    Variable *vstrm;
    vstrm = w->getArg(0)->deref();
    if (vstrm->isInteger()) {
      Variable *v = w->getArg(1)->deref();
      *((ostream *)(vstrm->getInteger())) << *v;
      return true;
    }
    return false;
  }
  
  bool btWriteLn1(Engine *w) {
    Variable *v;
    v = w->getArg(0)->deref();
    *w->eout << *v << endl;
    return true;
  }
  
  bool btWriteLn2(Engine *w) {
    Variable *vstrm, *v;
    vstrm = w->getArg(0)->deref();
    if (vstrm->isInteger()) {
      v = w->getArg(1)->deref();
      *((ostream *)(vstrm->getInteger())) << *v << endl;
      return true;
    }
    return false;
  }
  
  bool btNewLine0(Engine *w) {
    *w->eout << endl;
    return true;
  }
  
  bool btNewLine1(Engine *w) {
    Variable *vstrm;
    vstrm = w->getArg(0)->deref();
    if (vstrm->isInteger()) {
      *((ostream *)(vstrm->getInteger())) << endl;
      return true;
    }
    return false;
  }
  
  // consult compiles a prolog program and loads the resulting
  // code into memory
  bool doConsult(Engine *w, const string &fileName) {
    PrologCompiler pc(w);
    Program prog(w->program->getTopAddress());
    int i = 0;
    bool compiled = pc.compileFile(fileName, prog);
    while (!compiled && vpaths[i] != NULL) {
      compiled = pc.compileFile(vpaths[i] + fileName, prog);
      i++;
    }
    if (!compiled) { // program could not be compiled for whatever reason
      cerr << "File \"" << fileName << "\" could not be opened." << endl;
      return false;
    }
    else {
      w->program->addProgram(prog);  // add program to that already in memory
      w->program->updateLabels();    // and don't forget to update the jump
				     // labels
      return true;
    }
  }
  
  bool btConsult(Engine *w) {
    Variable *v = w->getArg(0)->deref();
    if (v->isAtom()) {
      return doConsult(w, v->getString());
    }
    else
      return false;
  }
  
  bool btSaveWAMCode(Engine *w) {
    Variable *v = w->getArg(0)->deref();
    if (v->isAtom()) {
      ofstream fos(v->getString());
      if (fos.fail())
	return false;
      else {
	fos << "% -*- mode: ciao; -*-" << endl;
	w->program->dumpWAMCode(fos);
	fos.close();
	return true;
      }
    }
    else
      return false;
  }
  
  bool btReadLn1(Engine *w) {
    Variable *v = w->getArg(0)->deref();
    if (v->isFree()) {
      w->do_trail(v);
      cin >> *v;
      return true;
    }
    return false;
  }
  
  bool btReadLn2(Engine *w) {
    Variable *vstrm = w->getArg(0)->deref();
    if (vstrm->isInteger()) {
      Variable *v = w->getArg(1)->deref();
      if (v->isFree()) {
	w->do_trail(v);
	*((istream *)(vstrm->getInteger())) >> *v;
	return true;
      }
    }
    return false;
  }
  
  bool btHRTime(Engine *w) {
    Variable *v_hi = w->getArg(0)->deref();
    Variable *v_lo = w->getArg(1)->deref();
    if (v_hi->isFree() && v_lo->isFree()) {
      uint64 time;
      uint32 time_hi, time_lo;
      w->do_trail(v_hi);
      w->do_trail(v_lo);
      time = hrtime();
      time_hi = (uint32)(time >> 32);
      time_lo = (uint32)(time);
      v_hi->setValue(time_hi);
      v_lo->setValue(time_lo);
      return true;
    };
    return false;
  }
  
  bool btTimeInterval(Engine *w) {
    Variable *v_hi = w->getArg(0)->deref();
    Variable *v_lo = w->getArg(1)->deref();
    if (v_hi->isFree() && v_lo->isFree()) {
      uint64 time;
      uint32 time_hi, time_lo;
      w->do_trail(v_hi);
      w->do_trail(v_lo);
      time = timeEnd - timeInit;
      time_hi = (uint32)(time >> 32);
      time_lo = (uint32)(time);
      v_hi->setValue(time_hi);
      v_lo->setValue(time_lo);
      return true;
    }
    return false;
  }
  
  bool btSub2(Engine *w) {
    Variable *va_hi = w->getArg(0)->deref();
    Variable *va_lo = w->getArg(1)->deref();
    Variable *vb_hi = w->getArg(2)->deref();
    Variable *vb_lo = w->getArg(3)->deref();
    Variable *vr_hi = w->getArg(4)->deref();
    Variable *vr_lo = w->getArg(5)->deref();
    
    if (va_hi->isInteger() && va_lo->isInteger() && vb_hi->isInteger()
	&& vb_lo->isInteger() && vr_hi->isFree() && vr_lo->isFree()) {

      size_t a_hi=va_hi->getInteger();
      size_t a_lo=va_lo->getInteger();
      size_t b_hi=vb_hi->getInteger();
      size_t b_lo=vb_lo->getInteger();

      w->do_trail(vr_hi);
      w->do_trail(vr_lo);
      
      size_t r_hi, r_lo;
      
      r_lo = a_lo - b_lo;
      r_hi = a_hi - b_hi - (a_lo < b_lo);

      vr_hi->setValue(r_hi);
      vr_lo->setValue(r_lo);
      return true;
    };
    return false;
  }
  
  bool btOpenOutput(Engine *w) {
    Variable *vfile = w->getArg(0)->deref();
    Variable *vstrm = w->getArg(1)->deref();
    if (vstrm->isFree() && vfile->isAtom()) {
      const char *file = vfile->getString();
      w->do_trail(vstrm);
      vstrm->setValue((size_t)w->eout);
      w->eout = new ofstream(file);
      return true;
    }
    return false;
  }
  
  bool btCloseOutput(Engine *w) {
    Variable *vstrm = w->getArg(0)->deref();
    if (vstrm->isInteger()) {
      delete w->eout;
      w->eout = (ostream *)(vstrm->getInteger());
      return true;
    }
    return false;
  }
  
  bool btOpenInput(Engine *w) {
    Variable *vfile = w->getArg(0)->deref();
    Variable *vstrm = w->getArg(1)->deref();
    if (vstrm->isFree() && vfile->isAtom()) {
      const char *file = vfile->getString();
      w->do_trail(vstrm);
      vstrm->setValue((size_t)w->ein);
      w->ein = new ifstream(file);
      return true;
    }
    return false;
  }

  bool btCloseInput(Engine *w) {
    Variable *vstrm = w->getArg(0)->deref();
    if (vstrm->isInteger()) {
      delete w->ein;
      w->ein = (istream *)(vstrm->getInteger());
      return true;
    }
    return false;
  }
  
  bool btOpen(Engine *w) {
    Variable *vfile = w->getArg(0)->deref();
    Variable *vmode = w->getArg(1)->deref();
    Variable *vstrm = w->getArg(2)->deref();
    if (vstrm->isFree() && vmode->isAtom() && vfile->isAtom()) {
      iostream *strm;
      const char *file = vfile->getString();
      switch (vmode->getString()[0]) {
      case 'r':
	strm = new fstream(file, ios_base::in);
	break;
      case 'w':
	strm = new fstream(file, ios_base::out);
	break;
      case 'a':
	strm = new fstream(file, ios_base::out | ios_base::app);
	break;
      default:
	return false;
      }
      w->do_trail(vstrm);
      vstrm->setValue((size_t)strm);
      return true;
    };
    return false;
  }

  bool btClose(Engine *w) {
    Variable *vstrm = w->getArg(0)->deref();
    if (vstrm->isInteger()) {
      iostream *strm = (iostream *)(vstrm->getInteger());
      delete strm;
      return true;
    }
    return false;
  }

  bool btTermToFile(Engine *w) {
    Variable *vterm = w->getArg(0)->deref();
    Variable *vfile = w->getArg(1)->deref();
    if (vfile->isAtom()) {
      ofstream os(vfile->getString());
      os << *vterm;
      return true;
    }
    return false;
  }

  bool btLimits(Engine *w) {
    *w->eout
      << "  sizeof('Instruction') := " << sizeof(Instruction) << '.' << endl
      << "  sizeof('ByteCode')    := " << sizeof(ByteCode)    << '.' << endl
      << "  sizeof('Environment') := " << sizeof(Environment) << '.' << endl
      << "  sizeof('ChoicePoint') := " << sizeof(ChoicePoint) << '.' << endl
      << "  sizeof('Variable')    := " << sizeof(Variable)    << '.' << endl
      //<< "  sizeof('Atom')        := " << sizeof(Atom)        << '.' << endl
      << "  sizeof('Structure')   := " << sizeof(Structure)   << '.' << endl
      << "  sizeof('Engine')      := " << sizeof(Engine)      << '.' << endl
      << "  sizeof('Trail')       := " << sizeof(Trail)       << '.' << endl
      << "  sizeof(char)          := " << sizeof(char)        << '.' << endl
      << "  sizeof(short)         := " << sizeof(short)       << '.' << endl
      << "  sizeof(int)           := " << sizeof(int)         << '.' << endl
      << "  sizeof(size_t)        := " << sizeof(size_t)      << '.' << endl
      << "  sizeof('void *')      := " << sizeof(void *)      << '.' << endl
      << "  sizeof(long)          := " << sizeof(long)        << '.' << endl
      << "  sizeof(long long)     := " << sizeof(long long)   << '.' << endl
      << "  sizeof(uint64)        := " << sizeof(uint64)      << '.' << endl;
    return true;
  }
  
  bool btStatus(Engine *w) {
    *w->eout << "  B := " << ((char *)w->getChoicePoint() - (char *)w->stack)
	  << endl;
    *w->eout << "  E := " << ((char *)w->getEnvironment() - (char *)w->stack)
	  << endl;
    *w->eout << "  H := " << w->heap.getTop() << endl;
    *w->eout << "  S := " << (void *)w->stack << endl;
    return true;
  }
  
  // input "halt." means: end the WAM now, dude!
  bool btHalt(Engine *w) {
    // exit(EXIT_SUCCESS);
    w->bContinue = false;
    w->bHalt = true;
    return true;
  }
  
  bool btProcedures(Engine *w) {
    for (Program::labels_type::iterator i = w->program->labels.begin();
	 i != w->program->labels.end(); i++) {
      ClauseKey m = i->first;
      if ((m.name[0] !='\0')&&(m.number==1))
	*(w->eout) << m.name << '/' << m.arity << endl;
    }
    return true;
  }
  
  bool btHelp(Engine *w) {
    w->showHelp();
    return true;
  }
  
  bool btLabels(Engine *w) {
    for (Program::labels_type::iterator i = w->program->labels.begin();
	 i != w->program->labels.end(); i++) {
      ClauseKey m = i->first;
      if (m.name[0] != '\0')
	*w->eout << m.name << '/' << m.arity
	     << '/' << m.number << endl;
    }
    return true;
  }
  
  bool btAtoms(Engine *w) {
    *w->eout << "Atoms: " << Atoms::atoms.size() << endl;
    for (Atoms::atoms_type::iterator i = Atoms::atoms.begin();
	 i != Atoms::atoms.end(); i++) {
      *w->eout << (void *)(*i) << " :[" << *i << ']' << endl;
    }
    return true;
  }
  
  bool btShowGCList(Engine *w) {
#ifdef USE_GC
    GCPtr<Variable, 0>::showList();
    GCPtr<Environment, 0>::showList();
    GCPtr<ChoicePoint, 0>::showList();
#else
    *w->eout << "Garbage collector is disabled." << endl;
#endif
    return true;
  }
  
  bool btWAMCode(Engine *w) {
    w->program->dumpWAMCode(*w->eout) << endl;
    return true;
  }

  bool btIntCode(Engine *w) {
    w->program->dumpIntCode(*w->eout) << endl;
    return true;
  }

}

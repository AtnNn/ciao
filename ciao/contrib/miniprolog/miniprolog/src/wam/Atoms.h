/*
** Atoms.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Sun Mar 30 13:08:13 2008 Edison Mera
** Last update Sun Mar 30 13:08:13 2008 Edison Mera
*/

#ifndef  _Atoms_H_
# define _Atoms_H_

# include <set>
# include <string.h>
# include "types.h"
# include "compat.h"

using namespace std;

namespace wam {

  struct CCharComp {
    bool operator()(const CCharPtr s1, const CCharPtr s2) const {
      return strcmp(s1, s2) < 0;
    }
  };

  class Atoms {
  public:
    typedef set<CCharPtr, CCharComp> atoms_type;
    static atoms_type atoms;

    static void atomsClear(atoms_type &atoms) {
      for (atoms_type::iterator i = atoms.begin(); i!= atoms.end(); i++) {
	delete [] *i;
      }
      atoms.clear();
    }
    
    static CCharPtr copyValue(CCharPtr source) {
      atoms_type::iterator p = atoms.find(source);
      if (p == atoms.end()) {
	size_t l = strlen(source)+1;
	char *dest = new char[l];
	strcpy_s(dest, l, source);
	atoms.insert(dest);
	return dest;
      } else {
	return *p;
      }
    }
    
    static CCharPtr insertValue(CCharPtr source) {
      atoms_type::iterator p = atoms.find(source);
      if (p == atoms.end()) {
	CCharPtr dest = source;
	atoms.insert(dest);
	return dest;
      } else {
	delete [] source;
	return *p;
      }
    }
    
    ~Atoms() {
      atomsClear(atoms);
    }
    
    void clear() {
      atomsClear(atoms);
    }
  };

};

#endif // _Atoms_H_

#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include <stdio.h>

#include "Variable.h"

#ifndef INLINE
# include "VariableImpl.h"
#endif

namespace wam
{

  DO_DEBUG(void *Variable::memory, 3);
  
  ostream & operator << (ostream &os, const Variable &s) {
    return os_variable(os, s, NULL);
  };
  
  istream & operator >> (istream &is, Variable &s) {
    char a[1024];
    is >> a;
    s.parseValue(a);
    return is;
  };
  
  void cleanVariableNames(VariableNames &vn) {
    for (VariableNames::iterator i = vn.begin(); i != vn.end(); i++) {
      delete [] i->second;
    }
  }
  
  ostream &os_variable(ostream &os,
		       const Variable &s,
		       VariableNames &variableNames,
		       set<const char *> &names,
		       set<const Variable *> &printedVariables);
  
  ostream &os_variable_(ostream &os, const Variable &s,
			VariableNames &variableNames,
			set<const char *> &names,
			set<const Variable *> &printedVariables);
  
  const char *getNewName(set<const char *> &names)
  {
    int start = 1;
    char *name = new char[16];
    name[0]='_';
    bool found;
    do {
      found = false;
      sprintf(name + 1, "%d", start);
      start++;
      set<const char *>::iterator i = names.begin();
      while (!found && i!=names.end()) {
	if (!strcmp(*i, name))
	  found = true;
	i++;
      }
    } while(found);
    return name;
  }
  
  const char *getVariableName(VariableNames &variableNames,
			 set<const char *> &names,
			 const Variable &s) {
    const VariableNames::iterator p = variableNames.find(&s);
    if (p != variableNames.end() && p->first==&s)
      return p->second;
    else {
#ifdef DEBUG
      char *sname = new char[16];
      sname[0] = '_';
      sprintf(sname + 1, "%d", (char *)&s - (char *)Variable::memory);
#else
      const char *sname = getNewName(names);
#endif
      variableNames.insert(make_pair(&s, sname));
      names.insert(sname);
      return sname;
    }
  }
  
  ostream &os_variable(ostream &os,
		       const Variable &s,
		       VariableNames &variableNames,
		       set<const char *> &names,
		       set<const Variable *> &printedVariables) {
    set<const Variable *>::iterator p = printedVariables.find(&s);
    bool printingVariable = (p == printedVariables.end());
    if (printingVariable)
      printedVariables.insert(&s);
    else
      return os << getVariableName(variableNames, names, s);
    switch (s.tag) {
    case INT:
      os << s.getInteger();
      break;
    case ATM:
      os << s.getString();
      break;
    case LIS:
      {
        os << "[";
	Variable *t = s.getTail();
	os_variable(os, *(s.getHead()->deref()), variableNames, names,
		    printedVariables);
	while ((t != NULL) && ((t=t->deref())->isList())) {
	  os_variable(os << ",", *(t->getHead()->deref()),
		      variableNames, names, printedVariables);
	  t = t->getTail();
	}
	if (t!=NULL && (!t->isAtom() || strcmp(t->getString(), "[]")))
	  os_variable(os << "|", *t, variableNames, names, printedVariables);
	os << "]";
	break;
      }
    case STR:
      {
	os << s.getString();
	os << "(";
	for (size_t i = 0; i < s.getArity(); i++) {
	  if (i)
	    os << ",";
	  os_variable(os, *(s.getArg(i)->deref()), variableNames, names,
		      printedVariables);
	}
	os << ")";
        break;
      }
    case REF:
      os_variable(os, *(s.getReference()->deref()), variableNames, names,
		  printedVariables);
      break;
    case FREE:
    default:
      os << getVariableName(variableNames, names, s);
    }
    if (printingVariable)
      printedVariables.erase(printedVariables.find(&s));
    return os;
  }
  
  ostream &os_variable(ostream &os,
		       VariableNames &variableNames,
		       const Variable &s,
		       NamedVariables *queryVariables) {
    set<const Variable *> printedVariables;
    set<const char *> names;
    if (queryVariables!=NULL) {
      for (NamedVariables::iterator p = queryVariables->begin();
	   p != queryVariables->end(); p++) {
	const char *sname = strcpy(new char[strlen(p->first) + 1], p->first);
	if (sname[0]!='\0' && sname[0] != '_') {
	  variableNames.insert(make_pair(p->second->deref(), sname));
	  names.insert(sname);
	}
      }
    }
    return os_variable(os, *s.deref(), variableNames, names, printedVariables);
  }
  
  ostream &os_variable(ostream &os,
		       const Variable &s,
		       NamedVariables *queryVariables)
  {
    VariableNames variableNames;
    os_variable(os, variableNames, s, queryVariables);
    cleanVariableNames(variableNames);
    return os;
  }
}

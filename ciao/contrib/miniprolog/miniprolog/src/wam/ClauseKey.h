/*
** ClauseKey.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Mon Oct 29 18:00:09 2007 Edison Mera
** Last update Mon Oct 29 18:00:09 2007 Edison Mera
*/

#ifndef   	CLAUSEKEY_H_
# define   	CLAUSEKEY_H_

#include <string.h>

using namespace std;

namespace wam {

  class ClauseKey {
  public:
    const char *name;
    size_t arity;
    size_t number;

  ClauseKey(const char *_name, size_t _arity, size_t _number)
    : arity(_arity), number(_number) {
    size_t l = strlen(_name)+1;
    char *buff = new char[l];
    strcpy_s(buff, l, _name);
    name = buff;
  };
    
  ClauseKey(const ClauseKey &clauseKey) :
    arity(clauseKey.arity), number(clauseKey.number) {
    size_t l = strlen(clauseKey.name)+1;
    char *buff = new char[l];
    strcpy_s(buff, l, clauseKey.name);
    name = buff;
    };

  ClauseKey() : name(""), arity(0), number(1) {};

    ~ClauseKey() {
      delete [] name;
    }
    
    friend bool operator ==(const ClauseKey &x, const ClauseKey &y) {
      return !strcmp(x.name, y.name) && (x.arity == y.arity)
      && (x.number == y.number);
    };
    friend bool operator <(const ClauseKey &x, const ClauseKey &y) {
      int c = strcmp(x.name, y.name);
      return
	(c < 0)
	|| ((c == 0)
	    && ((x.arity < y.arity)
		|| ((x.arity == y.arity)
		    && (x.number < y.number))
		)
	    );
    }
    friend ostream &operator<<(ostream &os, const ClauseKey &y) {
      return os << y.name << "/" << y.arity << "/" << y.number;
    }
  };
};

#endif 	    /* !CLAUSEKEY_H_ */

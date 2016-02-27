/*
** types.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Sat Jan 12 14:22:28 2008 Edison Mera
** Last update Sat Jan 12 14:22:28 2008 Edison Mera
*/

#ifndef   	TYPES_H_
# define   	TYPES_H_

typedef char * CharPtr;
typedef const char * CCharPtr;

typedef unsigned long uint32;
  
#ifdef __BORLANDC__
typedef unsigned __int64 uint64;
typedef __int64 int64;
#else
typedef unsigned long long uint64;
typedef long long int64;
#endif

#endif 	    /* !TYPES_H_ */

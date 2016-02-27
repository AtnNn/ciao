/*
** compat.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Mon Apr 21 21:11:37 2008 Edison Mera
** Last update Mon Apr 21 21:11:37 2008 Edison Mera
*/

#ifndef   	COMPAT_H_
# define   	COMPAT_H_

#if !defined(_MSC_VER)

#define strcpy_s(D,L,S)    strcpy((D),(S))
#define strncpy_s(D,L,S,C) strncpy((D),(S),(C))
#define _unlink(F)         unlink((F))

#endif

#endif 	    /* !COMPAT_H_ */

/*
** Functions.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Wed Oct 24 16:13:05 2007 Edison Mera
** Last update Wed Oct 24 16:13:05 2007 Edison Mera
*/

#ifndef   	_Functions_H_
# define   	_Functions_H_

#include "Engine.h"

namespace wam {
  bool btAtomConcat(Engine *w);
  bool btAtomNumber(Engine *w);
  bool btAtoms(Engine *w);
  bool btRenameFile(Engine *w);
  bool btDeleteFile(Engine *w);
  bool btIsAtom(Engine *w);
  bool btIsInteger(Engine *w);
  bool btIsAtomic(Engine *w);
  bool btIsVar(Engine *w);
  bool btOpen(Engine *w);
  bool btOpenInput(Engine *w);
  bool btOpenOutput(Engine *w);
  bool btClose(Engine *w);
  bool btCloseInput(Engine *w);
  bool btCloseOutput(Engine *w);
  bool btWrite1(Engine *w);
  bool btWrite2(Engine *w);
  bool btWriteLn1(Engine *w);
  bool btWriteLn2(Engine *w);
  bool btNewLine0(Engine *w);
  bool btNewLine1(Engine *w);
  bool btConsult(Engine *w);
  bool btSaveWAMCode(Engine *w);
  bool btReadLn1(Engine *w);
  bool btReadLn2(Engine *w);
  bool btHRTime(Engine *w);
  bool btTimeInterval(Engine *w);
  bool btLimits(Engine *w);
  bool btStatus(Engine *w);
  bool btSub2(Engine *w);
  bool btTermToFile(Engine *w);
  bool btHalt(Engine *w);
/*   bool btList(Engine *w); */
  bool btProcedures(Engine *w);
  bool btHelp(Engine *w);
  bool btLabels(Engine *w);
  bool btTuneProfiler(Engine *w);
  bool btWAMCode(Engine *w);
  bool btIntCode(Engine *w);
  bool btShowGCList(Engine *w);
};

#endif 	    /* !BUILTINS_H_ */

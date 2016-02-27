/*
** MainProfReport.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Feb  7 17:01:22 2008 Edison Mera
** Last update Thu Feb  7 17:01:22 2008 Edison Mera
*/

#ifndef   	MainProfReport_H_
# define   	MainProfReport_H_

#include <iostream>

#include "Profiler.h"

using namespace wam;

class TotalProfileData {

 public:
  //  ByteCode byteCode;
  uint32 count;
  uint64 sumMinExecTime;  // This is a kludge to handle the case in
  uint64 sumMaxExecTime;  // which the average is equal to the min/max
  uint64 sumExecTime;
  uint64 overhead;
  uint64 sum2ExecTime;

  double getAvgExecTime() { return ((double)sumExecTime)/count; }
  double getMinExecTime() { return ((double)sumMinExecTime)/count; }
  double getMaxExecTime() { return ((double)sumMaxExecTime)/count; }

  TotalProfileData(): count(0), sumMinExecTime(0), sumMaxExecTime(0), sumExecTime(0),
    sum2ExecTime(0) {};

  friend ostream &operator << (ostream &os, const TotalProfileData &tpd) {
    return os << tpd.count << '\t'
	      << tpd.sumMinExecTime << '\t'
	      << tpd.sumMaxExecTime << '\t'
	      << tpd.sumExecTime << '\t'
	      << tpd.sum2ExecTime << '\t'
	      << tpd.overhead;
  }
  
  friend istream &operator >> (istream &in, TotalProfileData &tpd) {
    return in >> tpd.count
	      >> tpd.sumMinExecTime
	      >> tpd.sumMaxExecTime
	      >> tpd.sumExecTime
	      >> tpd.sum2ExecTime
	      >> tpd.overhead;
  }
  
};

class TotalProfileDatas {
 public:
  size_t count;
  TotalProfileData *tpd[NUM_BYTECODES][2];

  void initMem() {
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
      for (size_t failed = 0; failed < 2; failed++)
	tpd[bc][failed] = new TotalProfileData[numParameters[bc]];
  }

 TotalProfileDatas(size_t aCount) : count(aCount) {
    initMem();
  };

 TotalProfileDatas() : count(0) {
    initMem();
  };

 ~TotalProfileDatas() {
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
      for (size_t failed = 0; failed < 2; failed++)
	delete [] tpd[bc][failed];
  }

  friend ostream &operator << (ostream &os, const TotalProfileDatas &tpds) {
    os << tpds.count << endl;
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
      for (size_t failed = 0; failed < 2; failed++)
	for (size_t parameter = 0; parameter < numParameters[bc]; parameter++)
	  os << tpds.tpd[bc][failed][parameter] << endl;
    return os;
  }
  
  friend istream &operator >> (istream &in, TotalProfileDatas &tpds) {
    in >> tpds.count;
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
      for (size_t failed = 0; failed < 2; failed++)
	for (size_t parameter = 0; parameter < numParameters[bc]; parameter++)
	  in >> tpds.tpd[bc][failed][parameter];
    return in;
  }
};

#endif 	    // MainProfReport_H_

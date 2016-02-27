//
// File:   Profiler.h
// Author: edison
//
// Created on 21 de marzo de 2007, 13:57
//

#ifndef _Profiler_H_
#define	_Profiler_H_

#include <fstream>
#include <iostream>

#include "Settings.h"
#include "hrtime.h"
#include "ByteCode.h"
#include "WAMUtils.h"

using namespace std;

namespace wam {
  
#define PD_BUF_SIZE 65536*4
  //#define PD_BUF_SIZE 1

#define HR_PROFILE_FORMAT // Write profile info in human readable format
  
# if defined(PACK_PROFILE_DATA)
#  if defined(__BORLANDC__) || defined(_MSC_VER)
#   pragma pack(push)
#   pragma pack(1)
#  endif
# endif
  
  extern uint64 timeInit, timeEnd;
  
  struct ProfileData {
    //  Instruction *inst;
    size_t offset;
    uint32 exectime;
    uint32 overhead;
    bool failed;
    size_t parameter;
    ByteCode byteCode:8;

/*     bool getFailed() const { return (parameter & 1) == 1; } */
/*     void setFailed(bool failed) { */
/*       if (failed) parameter |= 1; */
/*       else */
/* 	parameter &= ~1; */
/*     } */
/*     bool getParameter() const { return (parameter >> 1) }; */
/*     void setParameter(size_t aParameter) { */
/*       parameter = (aParameter << 1) | (parameter & 1); */
/*     } */
  }
# if defined(PACK_PROFILE_DATA)
#  if defined(__GNUC__)
  __attribute__ ((packed))
#  endif
# endif
    ;

  extern ProfileData profileDatas[PD_BUF_SIZE], *profileData;
  
# if defined(PACK_PROFILE_DATA)
#  if defined(__BORLANDC__) || defined(_MSC_VER)
#   pragma pack(pop)
#  endif
# endif

  extern bool prof_binary;
  extern bool bProfile;
  extern ofstream *ofProfile;    // file descriptor to the profiler
				 // output
  extern size_t profileCounter;
  extern size_t profileOverhead;
  
  extern uint64 resource0, resource1, profile_aux0;
  
  
  extern void (*profile_write)(ostream &, ProfileData *, size_t);
  void profile_write_bin(ostream &in, ProfileData *profileDatas, size_t n);
  void profile_write_txt(ostream &in, ProfileData *profileDatas, size_t n);
  void profiler_init(const char *baseName);
  void profiler_end(void);
/*   uint32 tune_profiler(size_t n, size_t m); */
  
  
/*   static inline string statementToString(Instruction *s) { */
/*     string result = ""; */
/*     if (s==NULL) */
/*       return "\"null\""; */
/*     result = string("\"") + byteCodeToFunction(s->byteCode) */
/*       + s->getArgs() + "\""; */
/*     return result; */
/*   } */
  
  // profile_data(wam_statement, spend_time, profile_time).
  static inline void writeCurrProfileData(void) {
    profileData++;
    profileCounter++;
    if (profileData >= profileDatas + PD_BUF_SIZE) {
      profile_write(*ofProfile, profileDatas, PD_BUF_SIZE);
      profileData = profileDatas;
    }
  }
  
#define putInst(s)					  \
  {							  \
    register uint64 x = hrtime();			  \
    resource0 = x;					  \
    register uint64 y = resource1;			  \
    profileData->exectime = (uint32)(x - y);		  \
    profileData->failed = failed;			  \
    if (profile_aux0==0)				  \
      profileData->overhead = 0;			  \
    else						  \
      profileData->overhead = (uint32)(y - profile_aux0); \
    profile_aux0 = x;					  \
    writeCurrProfileData();				  \
    profileData->byteCode = s->byteCode;		  \
    profileData->offset = programCounter;		  \
    profileData->parameter = s->getParameter(this);	  \
    resource1 = hrtime();				  \
  }
  
  /*
  static inline void putInst(const Instruction *s) {
    static uint64 x;
    hrtime(profileData->resource0);
    profileData->resource1 = x;
    writeCurrProfileData();
    profileData->byteCode = s->byteCode;
    profileData->parameter = s->getParameter();
    x = hrtime();
  }
  */
  
  inline istream& operator >> (istream &is, ProfileData &pd) {
    char buff[128];
    uint32 exectime;
    uint32 overhead;
    bool failed;
    size_t offset;
    size_t parameter;
    is >> offset >> buff >> failed >> parameter >> exectime >> overhead;
    pd.offset    = offset;
    pd.byteCode  = functionToByteCode(buff);
    pd.failed    = failed;
    pd.parameter = parameter;
    pd.exectime  = exectime;
    pd.overhead  = overhead;
    return is;
  }
  
  inline ostream& operator << (ostream &os, const ProfileData &pd) {
    return os << pd.offset    << "\t"
	      << byteCodeToFunction(pd.byteCode) << "\t"
	      << pd.failed    << "\t"
	      << pd.parameter << "\t"
	      << pd.exectime  << "\t"
	      << pd.overhead;
  }
}

#endif	// _Profiler_H_

#include <string.h>

#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "Profiler.h"
#include "ByteCode.h"

using namespace std;

namespace wam {
  ProfileData profileDatas[PD_BUF_SIZE], *profileData;
  
  uint64 timeInit = 0, timeEnd = 0;
  
  bool prof_binary = true;
  bool bProfile = false;
  size_t profileCounter = 0;
  
  ofstream *ofProfile, *ofCounter;
  
  void (*profile_write)(ostream &, ProfileData *, size_t);
  
  uint64 tune_var;
  
  size_t profileOverhead = 0;
  uint64 resource0 = 0, resource1 = 0, profile_aux0 = 0;

  /*  
  uint32 tune_profiler(size_t n, size_t m) {
    uint64 u0, u1, d0=(uint64)(-1), d1=d0;
    size_t i, j;
    for (j = 0; j < m; j++) {
      u0 = hrtime();
      for (i=0; i < n; i++) {
	tune_var += hrtime();
      }
      u1 = hrtime() - u0;
      if (d0 > u1)
	d0 = u1;
    }
    for (j = 0; j < m; j++) {
      u0 = hrtime();
      for (i = 0; i < n; i++)
	tune_var += 0;
      u1 = hrtime() - u0;
      if (d1 > u1)
	d1 = u1;
    }
    profileOverhead = (uint32)((d0 - d1) / n);
    return (uint32)(d0 - d1);
  }
  */

  void profiler_init(const char *baseName) {
    register uint64 x = hrtime(); // preserve the time of this
				  // function
    char *fileName = new char[strlen(baseName) + 5];
    bProfile = true;
    if (prof_binary)
      profile_write = profile_write_bin;
    else
      profile_write = profile_write_txt;
    strcpy(fileName, baseName);
    strcat(fileName, ".prf");
    ofProfile = new ofstream(fileName, ios::trunc | ios::binary);

    strcpy(fileName, baseName);
    strcat(fileName, ".cnt");
    ofCounter = new ofstream(fileName, ios::trunc | ios::binary);
    profileCounter = 1;
    profileData = &profileDatas[0];
    //    profileData->resource0 = 0;
    profileData->byteCode = opProfileInit;
    profileData->failed = false;
    resource1 = x;
  }
  
  void profile_write_bin(ostream &out, ProfileData *profileDatas, size_t n) {
    out.write((char *)profileDatas,
		    (std::streamsize)(sizeof(ProfileData) * n));
  }
  
  void profile_write_txt(ostream &out, ProfileData *profileDatas, size_t n) {
    for (size_t i = 0; i < n; i++) {
      out << profileDatas[i] << endl;
    }
  }
  
  void profiler_end(void) {
    resource0 = hrtime();
    profile_write(*ofProfile, profileDatas, profileData - profileDatas + 1);
    *ofCounter << profileCounter << endl;
    delete ofProfile;
    delete ofCounter;
    ofProfile = NULL;
    DO_DEBUG(cerr << "NOTE: Profile wrote "
	     << profileCounter
	     << " items of " << sizeof(ProfileData) << " bytes " << endl, 0);
    bProfile = false;
  }
}

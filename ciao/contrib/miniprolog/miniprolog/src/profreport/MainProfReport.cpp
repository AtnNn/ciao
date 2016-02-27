//
// File:   MainProfReport.cpp
// Author: Edison Mera
//
// Created on 11 de Enero de 2007, 16:11
//

#include <fstream>
#include <iostream>
#include <iomanip>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>
#include <string.h>

#if defined(__BORLANDC__) || defined(_MSC_VER)
#include <io.h>
#endif

#if defined(__BORLANDC__)
#pragma hdrstop
#endif

#include "ByteCode.h"
#include "Profiler.h"
#include "MainProfReport.h"

//
//
//

using namespace wam;

#if defined(__BORLANDC__)
#pragma argsused
#endif

const int BUF_SIZE=1024*1024*2;
char buf[BUF_SIZE + 1];

int ignore_per_thousand = 0;
int num_add_files = 0;

enum OutputFormat {ofBinary, ofText, ofTextFmt, ofLatex, ofProlog};

OutputFormat outputFormat = ofText;

CharPtr *argo;
bool show_help = false;
bool bin_input = true;
bool estimate_exec_time = false;
bool can_add_files = false;
bool do_calibrate2 = false;

const char *timing_model_file = NULL;
const char *calib_name = NULL;

size_t num_iterations = 1;

ios_base::openmode input_file_mode = ios::binary;

const char *failText[] = { "succ", "fail" };

int compare_ppd(const void *a, const void *b) {
  ProfileData *ppda = (ProfileData *)a, *ppdb = (ProfileData *)b;
  if (ppda->byteCode < ppdb->byteCode)
    return -1;
  else if (ppda->byteCode > ppdb->byteCode)
    return 1;
  else if (ppda->failed < ppdb->failed)
    return -1;
  else if (ppda->failed > ppdb->failed)
    return 1;
  else if (ppda->parameter < ppdb->parameter)
    return -1;
  else if (ppda->parameter > ppdb->parameter)
    return 1;
  else if (ppda->exectime < ppdb->exectime)
    return -1;
  else if (ppda->exectime > ppdb->exectime)
    return 1;
  else
    return 0;
}

void sumarize(ProfileData *ppd, uint32 profOv, uint32 ini, uint32 end,
	      TotalProfileData &tpd) {
  uint32 overhead, exectime;
  tpd.sumExecTime = 0;
  tpd.sum2ExecTime = 0;
  tpd.overhead = 0;
  tpd.count = 0;
  for (uint32 i = ini; i < end; i++) {
    if (profOv > ppd[i].exectime)
      profOv = ppd[i].exectime;
  }
  for (uint32 i = ini; i < end; i++) {
    exectime = ppd[i].exectime - profOv;
    overhead = ppd[i].overhead + profOv;
    tpd.sumExecTime += exectime;
    tpd.sum2ExecTime += ((uint64)exectime)*exectime;
    tpd.overhead += overhead;
    tpd.count++;
  }
  if (ini < end) {
    tpd.sumMinExecTime = ppd[ini].exectime - profOv;
    tpd.sumMaxExecTime = ppd[end - 1].exectime - profOv;
    tpd.sumMinExecTime *= tpd.count;
    tpd.sumMaxExecTime *= tpd.count;
  } else {
    tpd.sumMinExecTime = 0;
    tpd.sumMaxExecTime = 0;
  }
}

void reader_bin(istream &in, ProfileData *ppd, size_t n) { 
  in.read((char *)ppd, n*sizeof(ProfileData));
}

void reader_txt(istream &in, ProfileData *ppd, size_t n) {
  for (size_t i = 0; i < n; i++) {
    in >> ppd[i];
  }
}

void (*reader)(istream &, ProfileData *, size_t) = reader_bin;

const char *toLatex(const char *s) {
  static char result[256];
  int j = 0;
  while (*s) {
    if (*s == '_') {
      result[j++] = '\\';
    }
    result[j++] = *s++;
  }
  result[j] = '\0';
  return result;
}

void writer(ProfileData *ppd, size_t n) {
  for (size_t i = 0; i < n; i++) {
    cout << ppd[i] << endl;
  }
}

void avg_files(char * infn[], size_t num, istream &cnt)
{
  ProfileData *ppd1, *ppd2;
  size_t n;
  if (bin_input)
    cnt.read((char *)&n, sizeof(uint32));
  else
    cnt >> n;
  if (n==0)
    return;
  ppd1 = new ProfileData[n];
  ppd2 = new ProfileData[n];
  ifstream in1( infn[0], input_file_mode );
  reader(in1, ppd1, n);
  for (size_t a = 1; a < num; a++) {
    ifstream in2( infn[a], input_file_mode );
    reader(in2, ppd2, n);
    for (size_t i = 0; i < n; i++) {
      ppd1[i].exectime += ppd2[i].exectime;
      ppd1[i].overhead += ppd2[i].overhead;
    }
  }
  for (size_t i = 0; i < n; i++) {
    ppd1[i].exectime /= num;
    ppd1[i].overhead /= num;
  }
  writer(ppd1, n);
}

class TotalByteCodeCount {
private:
  size_t *totalByteCodeCount[NUM_BYTECODES][2];
public:
  TotalByteCodeCount() {
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++) {
      totalByteCodeCount[bc][false] = new size_t[numParameters[bc]];
      totalByteCodeCount[bc][true] = new size_t[numParameters[bc]];
    }
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
      for (size_t failed = 0; failed < 2; failed++)
	for (size_t parameter = 0; parameter < numParameters[bc]; parameter++)
	  totalByteCodeCount[bc][failed][parameter] = 0;
  }
  ~TotalByteCodeCount() {
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++) {
      delete [] totalByteCodeCount[bc][false];
      delete [] totalByteCodeCount[bc][true];
    }
  }
  void incCount(size_t bc, size_t failed, size_t parameter) {
    totalByteCodeCount[bc][failed][parameter]++;
  }
  size_t getCount(size_t bc, size_t failed, size_t parameter) const {
    return totalByteCodeCount[bc][failed][parameter];
  }
  void setCount(size_t bc, size_t failed, size_t parameter, size_t value) {
    totalByteCodeCount[bc][failed][parameter] = value;
  }
};

void calculateTotals(istream &in, istream &cnt, size_t &n, ProfileData *&ppd,
		     TotalByteCodeCount &counts)
{
  if (bin_input)
    cnt.read((char *)&n, sizeof(uint32));
  else
    cnt >> n;
  if (n==0)
    return;
  ppd = new ProfileData[n];
  reader(in, ppd, n);
  // discard the first and the last bytecode
  qsort(ppd + 1, n - 2, sizeof(ProfileData), compare_ppd);
  for (uint32 i = 1; i < n - 1; i++) {
    counts.incCount(ppd[i].byteCode, ppd[i].failed, ppd[i].parameter);
  }
}

void calibrate(istream &in, istream &cnt)
{
  TotalByteCodeCount totalByteCodeCount;
  ProfileData *ppd;
  size_t n;
  calculateTotals(in, cnt, n, ppd, totalByteCodeCount);
  TotalProfileDatas tpds(n);
  uint64 tot_sum_exectime, tot_overhead, tot_count;
  if (outputFormat==ofTextFmt)
    cout << "Number of items: " << n << endl;
  switch (outputFormat) {
  case ofTextFmt:
    cout << "============================================================================================" << endl
	 << "ByteCode             fail   Param sum(T)   min    max      mean     stdev%   Overhead  Count" << endl
	 << "============================================================================================" << endl;
    break;
  case ofProlog:
    cout << "% -*- mode: ciao; -*-" << endl << endl
	 << "approx_db( lb, 1 )." << endl
	 << "approx_db( me, 2 )." << endl
	 << "approx_db( ub, 3 )." << endl
	 << "approx_db( s2, 4 )." << endl << endl;
    break;
  case ofLatex:
    cout << "    \\begin{tabular}{|l|r|r|r|r|r|}" << endl
	 << "      \\hline" << endl
         << "      Instruction            & Fail & Param & Lower & Upper & Mean & StDev    \\\\" << endl
	 << "      \\hline\\hline\n";
    break;
  case ofText:
  case ofBinary:
    break;
  }
  uint32 curr = 0, next = 0, bcc, cbcc;
  for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
    for (size_t failed = 0; failed < 2; failed++)
      for (size_t parameter = 0; parameter < numParameters[bc]; parameter++) {
	bcc = totalByteCodeCount.getCount(bc, failed, parameter);
	next += bcc;
	cbcc = bcc - bcc*ignore_per_thousand/1000;
	sumarize(ppd + 1, profileOverhead, curr, curr + cbcc, tpds.tpd[bc][failed][parameter]);
	curr = next;
      }
  tot_sum_exectime = 0;
  tot_overhead = 0;
  tot_count = 0;
  if (outputFormat== ofText)
    cout << tpds;
  else {
    for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
      for (size_t failed = 0; failed < 2; failed++)
	for (size_t parameter = 0; parameter < numParameters[bc]; parameter++) {
	  TotalProfileData *tpd = &tpds.tpd[bc][failed][parameter];
	  if (tpd->count>0) {
	    tot_sum_exectime += tpd->sumExecTime;
	    tot_overhead += tpd->overhead;
	    tot_count += tpd->count;
	    double m = tpd->getAvgExecTime();
	    const char *name = byteCodeToFunction((ByteCode)bc);
	    double error = sqrt(tpd->sum2ExecTime/(double)tpd->count
				- m * m) * 100 / m;
	    switch (outputFormat) {
	    case ofLatex:
	      cout << "      " << left << setw(22) << toLatex(name) << right
		   << " & " << setw(4) << failText[failed]
		   << " & " << setw(5) << parameter
		   << " & " << setw(5) << tpd->getMinExecTime()
		   << " & " << setw(5) << tpd->getMaxExecTime()
		   << " & " << setw(4) << fixed << setprecision(0) << m
		   << " & " << setw(5) << fixed << setprecision(1) << error
		   << " \\% \\\\" << endl;
	      break;
	    case ofTextFmt:
	      cout << left     << setw(20) << name << " "
		   << left     << failText[failed] << " "
		   << right    << setw(5) << parameter
		   << right    << setw(9) << tpd->sumExecTime << " "
		   << setw(5)  << fixed << setprecision(0) << tpd->getMinExecTime() << " "
		   << setw(6)  << fixed << setprecision(0) << tpd->getMaxExecTime() << " "
		   << setw(9)  << fixed << setprecision(2) << m << " "
		   << setw(10) << fixed << setprecision(4) << error << " "
		   << setw(10) << tpd->overhead << " "
		   << setw(6)  << tpd->count << endl;
	      break;
	    case ofProlog:
	      cout << "timing_model_db( " << left << setw(19) << name << " , "
		   << failText[failed] << " , "
		   << parameter << " , m( "
		   << setw(5)  << tpd->sumMinExecTime << "/"
		   << setw(10) << tpd->count << " , "
		   << setw(10) << tpd->sumExecTime << "/"
		   << setw(10) << tpd->count << " , "
		   << setw(5)  << tpd->sumMaxExecTime << "/"
		   << setw(10) << tpd->count << " , "
		   << setw(10) << tpd->sum2ExecTime << " ) ) ." << endl;
	      break;
	    default:
	      break;
	    }
	  }
	}
  }
  switch(outputFormat) {
  case ofTextFmt:
    cout << "============================================================================================" << endl;
    cout << left << setw(30) << "Total" << " " << right << setw(9) << tot_sum_exectime << " "
	 << setw(44) << tot_overhead << " " << setw(6) << tot_count << endl;
    break;
  case ofLatex:
    cout << "       \\hline\\hline" << endl
	 << "    \\end{tabular}" << endl;
    break;
  default:
    break;
  }
}

template<typename T>
T gcd(const T &_a, const T &_b) {
  if (_a == 0)
    return _b;
  else
    return gcd( _b % _a, _a);
}

// Note: the output is in ciao prolog fsyntax to facilitate further processing
void calibrate2(istream &in1, istream &cnt1, istream &in2, istream &cnt2)
{
  TotalByteCodeCount counts, counts1, counts2;
  ProfileData *ppd1, *ppd2;
  size_t n1, n2;
  bool first = true;
  calculateTotals(in1, cnt1, n1, ppd1, counts1);
  calculateTotals(in2, cnt2, n2, ppd2, counts2);
  size_t divisor = 0;
  for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
    for (size_t failed = 0; failed < 2; failed++)
      for (size_t parameter = 0; parameter < numParameters[bc]; parameter++) {
	counts.setCount(bc, failed, parameter,
			counts2.getCount(bc, failed, parameter) - counts1.getCount(bc, failed, parameter));
	divisor = gcd(divisor, counts.getCount(bc, failed, parameter));
      }
  if (divisor % num_iterations != 0) {
    cerr << "Warning: calibrator " << calib_name
	 << " is not multiple of the number of iterations" << endl;
  }
  if (divisor % num_iterations != 0)
    cout << "eq( " << calib_name << " * " << num_iterations << " / " << divisor << " ) := ";
  else if (divisor == num_iterations)
    cout << "eq( " << calib_name << " ) := ";
  else
    cout << "eq( " << calib_name << " * " << num_iterations / divisor << " ) := ";
  
  for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
      for (size_t failed = 0; failed < 2; failed++)
	for (size_t parameter = 0; parameter < numParameters[bc]; parameter++) {
	  if (counts.getCount(bc, failed, parameter)!=0) {
// 	  if (counts.getCount(bc, failed, parameter) > 0) {
	    if (!first) {
	      cout << " + ";
	    }
// 	  }
// 	  else {
// // 	    cout << " - ";
// 	    cerr << "Error: In " << calib_name <<" Number of bytecodes should be positive " << endl
// // 	    counts.setCount(bc, failed, parameter, -counts.getCount(bc, failed, parameter));
// 	  }
	    if (counts.getCount(bc, failed, parameter)/divisor!=1)
	      cout << counts.getCount(bc, failed, parameter)/divisor << " * ";
	  //       cout << counts[bc] << "*";
	    cout << byteCodeToFunction((ByteCode)bc) << "_" << failed << "_" << parameter;
	    first = false;
	  }
	}
  if (first)
    cout << 0;
  cout << "." << endl;
}

void readTimingModel(istream &stm, TotalProfileDatas &tptm) {
  char byteCodeName[64];
  bool failed;
  size_t parameter;
  size_t bc, n;
  stm >> n;
  for (size_t i = 0; i < n; i++) {
    stm >> byteCodeName >> failed >> parameter;
    bc = (size_t)functionToByteCode(byteCodeName);
    TotalProfileData *tpm = tptm.tpd[bc][failed]+parameter;
    stm >> tpm->sumExecTime >> tpm->count;
    // fake values to simulate 0% Error:
    tpm->sumMinExecTime = tpm->sumExecTime;
    tpm->sumMaxExecTime = tpm->sumExecTime;
    tpm->sum2ExecTime = (uint64)(0.5+tpm->getAvgExecTime()*tpm->sumExecTime);
  }
}

void estimate(istream &tot, istream &stm) {
  TotalProfileDatas tpds, tptm;
  double estExecTime = 0;
  double minExecTime = 0;
  double maxExecTime = 0;
  tot >> tpds;
  readTimingModel(stm, tptm);
  for (size_t bc = 0; bc < NUM_BYTECODES; bc++)
    for (size_t failed = 0; failed < 2; failed++) {
      TotalProfileData *tpm = tptm.tpd[bc][failed];
      // This is a kludge to let this match with the static analysis
      for (size_t parameter = 0; parameter < numParameters[bc]; parameter++) {
	if (tptm.tpd[bc][failed][parameter].count > 0) {
	    tpm = tptm.tpd[bc][failed]+parameter;
	    break;
	}
      }

      for (size_t parameter = 0; parameter < numParameters[bc]; parameter++) {
	TotalProfileData *tpd = tpds.tpd[bc][failed]+parameter;
	//	TotalProfileData *tpm = tptm.tpd[bc][failed]+parameter;
	if (tpd->count > 0) {
	  if (tpm->count > 0) {
	    estExecTime += tpd->count * tpm->getAvgExecTime();
	    minExecTime += (uint64)tpd->count * tpm->getMinExecTime();
	    maxExecTime += (uint64)tpd->count * tpm->getMaxExecTime();
	  } else
	    cerr << "NOTE: In " << argo[0] << ", ByteCode "
		 << byteCodeToFunction((ByteCode)bc) << "(" << parameter << ") when "
		 << failText[failed]
		 << " not calibrated.  Estimator will lose precision." << endl;
	  
	}
      }
    }
  cout << setprecision(18) << estExecTime << '\t' << minExecTime
       << '\t' << maxExecTime << endl;
}

bool process_args(int argc, char *argv[], char *argo[]) {
  if ((argc==2 && (!strcmp(argv[1], "--help") || !strcmp(argv[1], "-h")))
      ||(argc==1)) {
    show_help = true;
    return true;
  }
  int i = 1;
  int j = 0;
  while (i < argc) {
    if (!strcmp(argv[i], "-f") && (i + 1 < argc)) {
      i++;
      if (!strcmp(argv[i], "bin"))
	outputFormat = ofBinary;
      else if (!strcmp(argv[i], "textfmt"))
	outputFormat = ofTextFmt;
      else if (!strcmp(argv[i], "text"))
	outputFormat = ofText;
      else if (!strcmp(argv[i], "latex"))
	outputFormat = ofLatex;
      else if (!strcmp(argv[i], "prolog"))
	outputFormat = ofProlog;
      else
	return false;
    }
    else if (!strcmp(argv[i], "-ti")) {
      input_file_mode = ios::in;
      reader = reader_txt;
      bin_input = false;
    }
    else if (!strcmp(argv[i], "-C") && (i + 2 < argc)) {
      i++;
      do_calibrate2 = true;
      calib_name = argv[i];
      i++;
      num_iterations = atoi(argv[i]);
    }
    else if (!strcmp(argv[i], "-p") && (i + 1 < argc)) {
      i++;
      profileOverhead = atoi(argv[i]);
    }
    else if (!strcmp(argv[i], "-i") && (i + 1 < argc)) {
      i++;
      ignore_per_thousand = atoi(argv[i]);
    }
    else if (!strcmp(argv[i], "-a") && (i + 1 < argc)) {
      i++;
      can_add_files = true;
      num_add_files = atoi(argv[i]);
    }
    else if (!strcmp(argv[i], "-e") ) { // New Estimation Method
      estimate_exec_time = true;
    }
    else
      argo[j++] = argv[i];
    i++;
  }
  argo[j] = NULL;
  return true;
}

void print_help(const char *cmd) {
  cerr << "Usage:" << endl
       << cmd << " [Options ...] File1 File2" << endl
       << endl
       << "Where the options are:" << endl
       << endl
       << "-C Calibrator N" << endl
       << "          First stage of calibrate2, process calibrator: " << endl
       << "          do group by bytecode ((File2-File1)/N and write" << endl
       << "          the relationship between calibrator and bytecode." << endl
       << endl
       << "-p T      Specifies the overhead that the profiler has" << endl
       << "          introduced in each measurement. By default is 0." << endl
       << endl
       << "-a N      Add N Files and write a file with the average." << endl
       << endl
       << "-e        Use the time model in the file File1 to" << endl
       << "          generate the estimated execution time for File2." << endl
       << endl
       << "-f Type   Output Format, can be bin, text, latex or prolog." << endl
       << endl
       << "-ti       The input is in text format." << endl
       << endl
       << "-i Ignore Specifies the  part per thousand that will" << endl
       << "          be ignored in the calibration step." << endl
       << endl
       << "File1     By default, the input file name." << endl
       << "File2     By default, the count file name." << endl;
}

int main(int argc, char* argv[]) {
  argo = new CharPtr[argc];
  
  if (!process_args(argc, argv, argo)) {
    print_help(argv[0]);
    return(EXIT_FAILURE);
  }
  if (show_help)
    print_help(argv[0]);
  else if (can_add_files) {
    int i = 0;
    while (i < num_add_files + 1) {
      if (argo[i]==NULL)
	return (EXIT_FAILURE);
      i++;
    }
    ifstream in2( argo[num_add_files], input_file_mode );
    avg_files(argo, num_add_files, in2);
  }
  else if (do_calibrate2) {
    if (argo[0]==NULL || argo[1]==NULL || argo[2] == NULL || argo[3] == NULL)
      return (EXIT_FAILURE);
    ifstream in1( argo[0], input_file_mode );
    ifstream in2( argo[1], input_file_mode );
    ifstream in3( argo[2], input_file_mode );
    ifstream in4( argo[3], input_file_mode );
    calibrate2(in1, in2, in3, in4);
  }
  else {
    if (argo[0]==NULL || argo[1]==NULL)
      return (EXIT_FAILURE);
    ifstream in1( argo[0], input_file_mode );
    ifstream in2( argo[1], input_file_mode );
    if (estimate_exec_time)
      estimate(in1, in2);
    else
      calibrate(in1, in2);
  }
  return (EXIT_SUCCESS);
}

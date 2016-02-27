//
// File:   MainProfView.cpp
// Author: edison
//
// Created on 3 de mayo de 2007, 15:11
//

#if defined(__BORLANDC__)
#pragma hdrstop
#endif

#include <fstream>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#if defined(__BORLANDC__) || defined(_MSC_VER)
#include <io.h>
#endif
#include "ByteCode.h"
#include "InstructionSpec.h"
#include "Profiler.h"
//
//
//

#if defined(__BORLANDC__)
#pragma argsused
#endif

const int BUF_SIZE=1024*1024*2;
char buf[BUF_SIZE + 1];

char *input_file, *output_file;

bool show_help = false;

bool bin_input = true;
bool bin_output = true;

ios_base::openmode input_file_mode = ios::binary,
  output_file_mode = ios::trunc | ios::binary;

void txt_printer(ostream &os, ProfileData &pd) {
  os << byteCodeToFunction((ByteCode)pd.byteCode) << "\t"
     << (int)pd.failed << "\t"
     << pd.parameter << "\t"
     << pd.exectime << "\t"
     << pd.overhead;
  os << pd << endl;
}

void bin_printer(ostream &os, ProfileData &pd) {
  os.write((char *)&pd, sizeof(ProfileData));
}

bool bin_reader(istream &in, ProfileData &pd) {
  return in.read((char *)&pd, sizeof(ProfileData))!=0;
}

bool txt_reader(istream &in, ProfileData &pd) {
  return !(in >> pd).eof();
}

bool (*reader)(istream &, ProfileData &) = bin_reader;

void (*printer)(ostream &, ProfileData &) = bin_printer;

bool process_args(int argc, char *argv[], char *argo[]) {
  if (argc==2 && (!strcmp(argv[1], "--help") || !strcmp(argv[1], "-h"))) {
    show_help = true;
    return true;
  }
  int i = 1;
  int j = 0;
  while (i < argc) {
    if (!strcmp(argv[i], "-ti")) {
      input_file_mode = ios::in;
      reader = txt_reader;
      bin_input = false;
    }
    else if  (!strcmp(argv[i], "-to")) {
      output_file_mode = ios::trunc;
      printer = txt_printer;
      bin_output = false;
    }
    else if (!strcmp(argv[i], "-i") && (i + 1 < argc)) {
      i++;
      input_file = argv[i];
    } else if (!strcmp(argv[i], "-o") && (i + 1 < argc)) {
      i++;
      output_file = argv[i];
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
       << cmd << " [-ti] [-to] -i InputFile -o OutputFile " << endl
       << endl;
}

int main(int argc, char* argv[]) {
  CharPtr *argo = new CharPtr[argc];
  ProfileData profileData;
  uint32 n;

  if (!process_args(argc, argv, argo)) {
    print_help(argv[0]);
    return(EXIT_FAILURE);
  }
  if (show_help) {
    print_help(argv[0]);
    return(EXIT_SUCCESS);
  }

  ofstream os( output_file, output_file_mode );

  // get length of file:
  if (bin_input) {
    ifstream in( input_file,  input_file_mode );
    in.seekg (0, ios::end);
    n = in.tellg();
    in.seekg (0, ios::beg);
    n /= sizeof(ProfileData);
    in.close();
  }
  else {
    ifstream in( input_file,  input_file_mode );
    n = 0;
    while (reader(in, profileData))
      n++;
    in.close();
  }
  if (bin_output)
    os.write((char *)&n, sizeof(uint32));
  else
    os << n << endl;

  ifstream in( input_file,  input_file_mode );
  while (reader(in, profileData)) {
    printer(os, profileData);
  }
  return (EXIT_SUCCESS);
}

#ifdef __BORLANDC__
# pragma hdrstop
# pragma package(smart_init)
#endif

#include "WAMUtils.h"
#include "Program.h"
#include "Engine.h"
#include "Profiler.h"
#include "Settings.h"

#ifdef USE_GC
# include "gc.h"
#endif

#include <stdio.h>
#include <stdlib.h>

using namespace std;
using namespace wam;

bool show_intro = true;
bool show_help = false;

bool process_args(int argc, char *argv[], char *argo[]) {
  if (argc==2 && (!strcmp(argv[1], "--help") || !strcmp(argv[1], "-h"))) {
    show_help = true;
    return true;
  }
  int i = 1;
  int j = 0;
  int k = 0;
  while (i < argc) {
    if (!strcmp(argv[i], "-s"))
      show_intro = false;
    else if (!strcmp(argv[i], "-t"))
      prof_binary = false;
    else if (!strcmp(argv[i], "-C") && (i + 1 < argc)) {
      i++;
      vpaths[k++] = argv[i];
    }
    else
      argo[j++] = argv[i];
    i++;
  }
  argo[j] = NULL;
  vpaths[k] = NULL;
  return true;
}

void print_help(const char *cmd) {
  cerr << "Usage:" << endl
       << cmd << " [-s] [-t] [-C DirName] " << endl
       << endl
       << "Where the options are:" << endl
       << endl
       << "-s         Supress welcome message." << endl
       << "-t         Set the profiler output format to text." << endl
       << "           By default the output is binary." << endl
       << "-C DirName Search for sources in DirName directory." << endl;
}

int main(int argc, char *argv[]) {
  
  CharPtr *argo = new CharPtr[argc];
  vpaths = new CharPtr[argc];
  
  if (!process_args(argc, argv, argo)) {
    print_help(argv[0]);
    return (EXIT_FAILURE);
  }
  
  if (show_help) {
    print_help(argv[0]);
    return (EXIT_SUCCESS);
  }
  
  if (show_intro) {
    cout << "\nMini WAM" << endl;
    cout << "Based on Stu's mighty WAM by Stefan Buettcher" << endl;
    cout << "Migrated from java to C++ and extended by Edison Mera" << endl;
    cout << "Compiled on " << __DATE__ << " " << __TIME__ << endl;
    cout << "Type \"help.\" to get some help." << endl;
  }
  void *code  = malloc(sizeof(char)* 8*1024*1024);
  void *heap  = malloc(sizeof(char)* 8*1024*1024);
  void *stack = malloc(sizeof(char)* 8*1024*1024);
  void *trail = malloc(sizeof(char)* 4*1024*1024);
  DO_DEBUG(cerr << "Debug Level = " << DEBUG_LEVEL << endl, 0);
#ifdef USE_GC
  GCInfo<Variable>::stack
# ifdef __Variable_in_stack__
    = stack;
# else
  = NULL;
# endif
  GCInfo<Environment>::stack   = stack;
  GCInfo<ChoicePoint>::stack   = stack;
  GCInfo<Trail>::stack         = stack;
#endif
  Program p(code);
  DO_DEBUG(Variable::memory = heap, 3);
  Engine engine(&p, heap, stack, trail);
//   GCInfo<Variable>::stack = engine.stack;
//  try {
    char s[1025];
    do {
      cout << "?- " << flush;
      fgets(s, 1024, stdin);
    } while ((s[0] != '\0') && (engine.runQuery(s)));
    //  } catch(exception e) {
    //    cout << e.what();
    //  }
  p.clear();
  cout << "Goodbye!" << endl;
  /*
    GCPtr<Variable>::collect();
    GCPtr<Engine::Environment>::collect();
    GCPtr<Engine::Trail>::collect();
    GCPtr<Engine::ChoicePoint>::collect();
    GCPtr<Variable>::shutdown();
    GCPtr<Engine::Trail>::shutdown();
    GCPtr<Engine::Environment>::shutdown();
    GCPtr<Engine::ChoicePoint>::shutdown();
  */
  free(trail);
  free(stack);
  free(heap);
  free(code);
  return EXIT_SUCCESS;
};

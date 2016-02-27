/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#define _GNU_SOURCE

#include <dlfcn.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <sys/stat.h>

#if defined(Win32)
#include <sys/cygwin.h>
#endif

#include "threads.h"
#include "main.h"
#include "debug.h"
#include "initial.h"
#include "task_areas.h"
#include "float_consts.h"

/* declarations for global functions accessed here */

#include "wam_defs.h"
#include "tasks_defs.h"
#include "unix_utils_defs.h"
#include "initial_defs.h"
#include "start_defs.h"
#include "qread_defs.h"
#include "builtin_defs.h"
#include "own_malloc_defs.h"
#include "alloc_defs.h"
#include "support_defs.h"
#include "locks_defs.h"
#include "profile_defs.h"
#include "startgoal_defs.h"
#include "timing_defs.h"

/* local declarations */


#define USAGE_STRING "Usage: %s [prolog_args] -C [-i] [-q] [-v] -b bootfile\n"

/* #define SUBDIR_WINDOWS_BIN "/Win32/bin" */

extern char *emulator_os;
extern char *emulator_architecture;

bool_t interactive = FALSE;
char *emulatorlength;

/*
extern void init_kanji(), init_latin1();
extern void compute_cwd();
extern void checkasserts(), mem_start_level();
extern void firstgoal();
extern void init_wrb_state_list(), init_once(), init_alloc();
extern void init_each_time();
*/

/* Not required now (DCG) */
/*
static void find_emulator(file)
     char *file;
{
  CIAO_REGISTER char *p;
  char path[MAXPATHLEN+1];

  for (p=file; *p && *p != '/'; p++)
    ;
  if (*p == '/')
    p = ".";
  else
    p = getenv("PATH");	
  
  while (*p) {
      CIAO_REGISTER char *next = path;
      
      while (*p && *p != ':')
	*next++ = *p++;
      *next++ = '/';
      *next++ = 0;
      if (*p) p++;
      if (!strcmp(path, "./")) path[0] = 0;
      strcat(path, file);
      
      if (!access(path, X_OK)){
        int emul_path_length = 1+strlen(path);
        emulator_path = (char *)checkalloc(emul_path_length);
        strcpy(emulator_path, path);
        if (expand_file_name(emulator_path, path)) {
          emulator_path = (char *)checkrealloc((tagged_t *)emulator_path, 
                                               emul_path_length,
                                               1+strlen(path));
          strcpy(emulator_path, path);
          return;
        }
        else checkdealloc((tagged_t *)emulator_path, emul_path_length);
      }
  }
  fprintf(stderr, "%s: emulator not found\n", file);
  at_exit(1);
}
*/

void load_ql_files(Arg, qfile)
     Argdecl;
     FILE *qfile;
{
  int more_ql;

  push_qlinfo(NULL);

  more_ql = qread1(w,qfile,&X(0));	            /* ignore version no. */
  w->global_top = NodeGlobalTop(w->node);              /* Reset heap pointer */
  
  while (more_ql)
    while ((more_ql = qread1(w,qfile,&X(0))) && run_determ_c(w,X(0)))
      w->global_top = NodeGlobalTop(w->node);
  
  pop_qlinfo(NULL);
}

static void open_emulator(file, stream, p)
     char *file;
     FILE **stream;
     char *p;
{
  char path[MAXPATHLEN+1];
  
  while (*p) {
      CIAO_REGISTER char *next = path;
      
      while (*p && *p != ':') *next++ = *p++;
      *next++ = '/';
      *next++ = 0;
      if (*p) p++;
      if (!strcmp(path, "./")) path[0] = 0;
      strcat(path, file);

      if (!access(path, X_OK)) {
	struct stat data;
	stat(path,&data);
	if (data.st_size == atoi(&emulatorlength[38])) {
	  fprintf(stderr, USAGE_STRING, file);
	  at_exit(1);}
	if ((*stream = fopen(path,"r")) == NULL) {
	  fprintf(stderr,"%s: unable to open for read\n", file);
	  at_exit(1);
	}
	fseek(*stream,atoi(&emulatorlength[38]),SEEK_SET);
	return;
      }
  }
  *stream = NULL;
}

/***************************************************************************/

extern char cwd[];

extern char *ciao_suffix;

void ciao_initcode(); /* initialize foreign interface definitions */

int exists(char *cadena, char *valor, char separador)
{
  char *aux;

  while(cadena)
    {
      aux = (char*) strchr(cadena,separador);
      if (aux) { aux[0] = '\0'; aux++;}
      if (!strcmp(cadena,valor)) return 1;
      cadena = aux;
    }

  return 0;
}

void init_emulatorlength(char *emulatorlength_holder)
{
  emulatorlength = emulatorlength_holder;
}

extern char *shell_system;

int start(argc, argv)
     int argc;
     char *argv[];
{
  /*Argdecl;*/
  goal_descriptor_t *first_goal;
  bool_t quiet = FALSE;
  int i;
  char *lc_ctype;
  char *raw_source_path = NULL;
  FILE *qfile = NULL;

#if defined(USE_OWN_MALLOC)
    init_own_malloc();
#endif

  tzset();                                       /* Initialize time zone information */
  init_locks();                                  /* global, first of all! */

  init_statistics();                             /* init the statistics related info */
  fillchardigit();                               /* prepares the char digit table */

#if defined(DEBUG)
  RESET_COUNTER;
#endif

  lc_ctype = getenv("LC_CTYPE");
  if (lc_ctype!=NULL &&
    (strcmp(lc_ctype,"ja_JP.EUC")==SAME || strcmp(lc_ctype,"ja_JP.euc")==SAME))
    init_kanji();
  else
    init_latin1();

  compute_cwd();

  prolog_argc = argc;
  prolog_argv = argv;

  for(i=1; i < argc; i++)                        /* Detect the first "-C" */
    if (strcmp(argv[i], "-C") == SAME){
      prolog_argc = i;
      break;
    }

  if (prolog_argc != argc){                      /* There are C arguments */
    for(i = prolog_argc + 1; i < argc; i++)
      if (strcmp(argv[i],"-b") == SAME)
        raw_source_path = argv[++i];
      else if (strcmp(argv[i],"-i") == SAME)
        prolog_force_interactive = 1;
      else if (strcmp(argv[i],"-q") == SAME)             /* To make quiet */
        quiet = TRUE;
      else if (strcmp(argv[i],"-v") == SAME)           /* To make verbose */
        quiet = FALSE;
    /*
      else if (strcmp(argv[i],"-L") == SAME){    
#if defined(Win32)
	library_directory = (char *)checkalloc(MAXPATHLEN+1);
	expand_file_name(argv[++i],library_directory);
#else
	library_directory = argv[++i];
#endif
      }
    */
      else
#if defined(PROFILE)
        if (strcmp(argv[i], "-prof") == SAME)        /* Simple profile */
          profile_eng = TRUE;
        else if (strcmp(argv[i], "-proft") == SAME)         /* Include time */
          profile_eng = TRUE;
          /* {profile_eng = TRUE; prof_include_time = TRUE;} */
        else
#endif
          if (strcmp(argv[i], "-tp") == SAME)        /* Trace predicates */
            predtrace = TRUE;
#if defined(DBG) || defined(DEBUG)
      else if (strcmp(argv[i], "-dcp") == SAME)  /*debug regular choicepoints*/
        debug_choicepoints = TRUE;
      else if (strcmp(argv[i], "-dconccp") == SAME) /*conc. choicepoints*/
        debug_concchoicepoints = TRUE;
      else if (strcmp(argv[i], "-dt") == SAME)           /* debug threads */
        debug_threads = TRUE;
      else if (strcmp(argv[i], "-dgc") == SAME)      /* debug garb. coll. */
        debug_gc = TRUE;
      else if (strcmp(argv[i], "-dmem") == SAME)       /* debug mem. man. */
        debug_mem = TRUE;
      else if (strcmp(argv[i], "-dconc") == SAME)    /* debug concurrency */
        debug_conc = TRUE;
#endif
#if defined(DEBUG)                         /* Pack here debugging options */
      else if (strcmp(argv[i],"-d") == SAME)
        debug_c = 1;
#endif
      else if (strcmp(argv[i], "-C") != SAME)  /* Ignore other "-C" */
        fprintf(stderr,"Warning: %s ignored\n",argv[i]);
  }
#if defined(PROFILE)
  if (profile||predtrace) stop_on_pred_calls = TRUE;
#else
  if (predtrace) stop_on_pred_calls = TRUE;
#endif


/* Find out the library_directory --- we need it before using '$' anywhere */

/*
#if defined(OldWin32) 
  {
    char *p;
    int slashcount = 0;

    p = argv[0];
    if (*p == '"') p++ ;

    expand_file_name(p, library_directory);

    p = library_directory+strlen(library_directory);

    while(p >= library_directory && slashcount<1)
      if (*p-- == '/') slashcount++;

    *(p+1) = (char)0;

    while(p >= library_directory && slashcount<3)
      if (*p-- == '/') slashcount++;

    if (strcasecmp(++p, SUBDIR_WINDOWS_BIN) == SAME)
      *p = (char)0;
    else {
      fprintf(stderr,
              "%s: should be in the standard location, under Win32/bin/",
              argv[0]);
      at_exit(1); 
    }
  }
#endif
*/

  /* If there is a CIAOLIB variable, we always use its value */
  if (!(library_directory = getenv("CIAOLIB")))
#if defined(Win32)
    /* Otherwise, look in the registry (for Windows executables) and set a
       couple more of variables  */
    if(using_windows()) {
      /* These are for the registry */
      HKEY SOFTWAREKey, CiaoPrologKey;
      DWORD buffer_size = MAXPATHLEN;
      char aux[MAXPATHLEN+1];

      /* These are to locate the shell (needed for the shell/1 call) */
      char *temp_path = (char *)checkalloc(MAXPATHLEN+1);
      char *current_path;
      char *current_path_local;
 
      library_directory = (char *)checkalloc(MAXPATHLEN+1);
     
      if (( RegOpenKeyEx(HKEY_LOCAL_MACHINE, TEXT("SOFTWARE"), 0, KEY_READ,
			 &SOFTWAREKey) == ERROR_SUCCESS ) &&
	  ( RegOpenKeyEx(SOFTWAREKey, TEXT("Ciao Prolog"), 0, KEY_READ,
			 &CiaoPrologKey) == ERROR_SUCCESS ) &&
	  ( RegQueryValueEx(CiaoPrologKey, TEXT("ciao_dir"), NULL, NULL,
		           aux, &buffer_size) == ERROR_SUCCESS ))
	{
	  cygwin32_conv_to_full_posix_path(aux, library_directory);
	  RegCloseKey(SOFTWAREKey);
	  RegCloseKey(CiaoPrologKey);
        } else if (raw_source_path != NULL) // else open the emulator itself
          {
            fprintf(stderr,
                    "%s\n%s\n",
                    "Registry key not found. Please remember to install Ciao Prolog",
                    "or to set the CIAOLIB environment variable!");
            at_exit(1);
          }

      /* Now, we adjust a couple of things to be used inside Windows;
         outstandingly, the PATH and the presence of the SHELL variable.
         We assume that ciaoengine.exe (if there is any), cygwin.dll, and
         sh.exe are in the same directory.  This is placed either in the 
         bin/${CIAOARCH} subdir or in the applications subdir (if it is packed).

         The SHELL environment variable, if not already set, should point to
         the sh.exe executable.
      */
    
      /* 
         We need the library directory here.  It either points to an
         installation directory, and it is stored in the registry, or exists
         because we got it from an environment variable, or we reverted to
         the "current directory", for "packed" distributions.  The last one
         not yet implemented. */

     /* Guess which one exists.  Start with the Win32/bin option */

      if (raw_source_path != NULL)
	{
	  strcpy(temp_path, library_directory);
	  strcat(temp_path, "/../build/objs/");
	  strcat(temp_path, emulator_os);
	  strcat(temp_path, emulator_architecture);
	  if (access(temp_path, F_OK)){ 
	    /* Does not exist --- look in the libdir itself */
	    strcpy(temp_path, library_directory);
	  }
	  
	  /* Is it already in the PATH? */
	  if (!(current_path = getenv("PATH")) ||               /* No path or */
	      !strstr(current_path, temp_path)) {      /* does not contain it */
	    /* Add to $PATH at the end */
	    if (current_path == NULL)
	      current_path_local = temp_path;
	    else {                           /* Do not alter the env. itself! */
	      current_path_local = 
		(char *)checkalloc(strlen(current_path) + MAXPATHLEN + 2);
	      strcpy(current_path_local, current_path);
	      strcat(current_path_local, ":");
	      strcat(current_path_local, temp_path);
	    }
	    setenv("PATH", current_path_local, 1);
	  }
	  
	  /* Check now if the SHELL variable has been defined --- the
	     shell/{0,3} calls depend on it. */
	  if (!getenv("SHELL")){
	    strcat(temp_path, "/sh.exe");  /* CygWin shell --- MUST be here */
	    setenv("SHELL", temp_path, 1);
	    shell_system = temp_path;
	  }
	}
      else   //open the emulator itself
        {
          //SHELL and PATH are the current working directory
          char *path = NULL, *aux, *executable;
          char *shell, *old_path_variable, *new_path_variable;
          int tam = 0;

          executable = (char*) malloc ((strlen(argv[0]) + 1) * sizeof(char));
          strcpy(executable,argv[0]);

          aux = (char*) strchr(executable,':');

          if (aux) //absolute name in the executable whit letter
            {
              aux = (char*) strrchr(executable,'\\');
              if (aux == executable + 2) // current_dir is 'c:\'
                aux++;
              aux[0] = '\0';  //mark the finish '\'
              path = (char*) malloc ((strlen(executable) + 1) * sizeof(char));
              strcpy(path,executable);
            }
          else
            {
              if (executable[0] == '\\') //absolute name in the executable
                {
                  do //path = current_dir
                    {
                      tam += 256;
                      path = (char*) realloc (path, tam*sizeof(char));
                      path = getcwd(path,tam - 1);
                    }while(path == NULL);

                  aux = (char*) strrchr(executable,'\\');
                  if (aux != executable) // path = argv[0] without executable name
                    {
                      path[2] = '\0';
                      aux[0] = '\0';
                      path = (char*) realloc (path, (strlen(executable) + 4) * sizeof(char));
                      strcat(path,executable);
                    }
                  else path[3] = '\0'; //take the letter
                }
              else  //relative name in the executable
                {
                  do //path = current_dir
                    {
                      tam += 256;
                      path = (char*) realloc (path, tam*sizeof(char));
                      path = getcwd(path,tam - 1);
                    }while(path == NULL);

                  aux = (char*) strrchr(executable,'\\');
                  if (aux != NULL) /* executable isntttt in current_dir -->
                                      add relative way without executable name*/
                    {
                      aux[0] = '\0';
                      path = (char*) realloc (path,(strlen(executable) + strlen(path) + 2) * sizeof(char));
                      strcat(path,"\\");
                      strcat(path,executable);
                    }
                }
            }
          //free(executable);

          old_path_variable = getenv("PATH");

          if(!exists(old_path_variable,path,';'))
            {
              new_path_variable = (char*) malloc ((strlen(path) + strlen(old_path_variable) + 2) * sizeof(char));
              strcpy(new_path_variable,old_path_variable);
              if (new_path_variable[strlen(new_path_variable) - 1] != ';') strcat(new_path_variable,";");
              strcat(new_path_variable,path);
              setenv("PATH",new_path_variable,1);
	  //free(old_path_variable);
	  //free(new_path_variable);
            }

          if (!getenv("SHELL"))
            {
              shell = (char*) malloc ((strlen(path) + 8) * sizeof(char));
              strcpy(shell,path);
              strcat(shell, "/sh.exe");
              setenv("SHELL",shell,1);
	      shell_system = shell;
              //free(shell);
            }
          //free(path);
        }
    } else
#endif
      /* Revert to installation-defined library directory otherwise */
      library_directory = default_lib_dir;
  
  /* If there is a CIAOHDIR variable, we always use its value */
  c_headers_directory = getenv("CIAOHDIR");
  if (c_headers_directory == NULL) {
    c_headers_directory = default_c_headers_dir;
  }

    /* No source path -- try to open the emulator itself and load the
       bytecode starting at the end of the emulator.  The length of the
       emulator is stored in a variable */

  if (raw_source_path == NULL) {
    CIAO_REGISTER char *p;

    for (p=argv[0]; *p && *p != '/'; p++);
    if (*p != '/') 
        open_emulator(argv[0],&qfile,getenv("PATH"));
    if (qfile == NULL) 
        open_emulator(argv[0],&qfile,".");
    if (qfile == NULL) { 
        fprintf(stderr,"%s: file not found\n", argv[0]);
        at_exit(1);
    }
  } else {
    expand_file_name(raw_source_path,source_path);
#if defined(Win32)
    i = strlen(source_path)-4;
    if (i > 0 && strcmp(source_path+i,".bat") == SAME){
      int j;
      for(j = 1; ciao_suffix[j] && (i+j < MAXPATHLEN); j++)
	source_path[i+j] = ciao_suffix[j];
    } else if (i > 0 && strcmp(source_path+i, ciao_suffix) != SAME)
      strcat(source_path, ciao_suffix);

    if (access(source_path,R_OK))
      source_path[strlen(source_path)-4] = '\0'; /* Take out ciao_suffix (.cpx) */
#endif
  }


  if (qfile == NULL) qfile = fopen(source_path,"r");
  if (qfile == NULL) {
    fprintf(stderr, "%s: boot file not found\n", source_path);
    at_exit(1);
  } else {                         /* We have a bootfile we can read from */
    
    /* Global initializations */
    checkasserts();
    /*init_wrb_state_list();*/
    init_goal_desc_list();
    init_once();
    init_alloc();
    current_quiet_flag = quiet ? atom_on : atom_off;
    /*mem_prog_count = 0;*/

    ciao_initcode(); /* initialize foreign interface definitions */
    glb_init_each_time();

 /* Make the first wam.  We need it to load the ql's. Main thread is always
    goal # 0 */

    first_goal = init_first_gd_entry();
    load_ql_files(first_goal->worker_registers, qfile);
    fclose(qfile);
    /* wam->next_insn set to boot code in local_init_each_time */
    /*w->node->global_top = w->global_top;*/     /* Isn't this unnecessary? */
    /* w->node->term[0] = X(0) = init_atom_check("internals:boot");*/
    firstgoal(first_goal, "internals:boot");              /*  Fills in worker_entry */
  }
  
  return 0;

}


/* Construct a UNIX-like call and pass it to start() */

int start_prolog_program(char * caller_name,
                         char * program_name)
{
  char *argv[4];
  
  argv[0] = caller_name;
  argv[1] = "-C";
  argv[2] = "-b";
  argv[3] = program_name;
  return start(4, argv);
}


void at_exit(result)
     int result;
{
#if defined(PROFILE)
  if (profile_eng) {
    void (*profile__stop_exit_)(void) = (void (*)(void))dlsym(RTLD_DEFAULT, "profile__stop_exit");
    void (*profile_dump_)(FILE *) = (void (*)(FILE *))dlsym(RTLD_DEFAULT, "profile_dump");
    if (profile__stop_exit_) profile__stop_exit_();
    if (profile_dump_) profile_dump_(stdout);
  }
#endif
  fflush(NULL);
  exit(result);
}

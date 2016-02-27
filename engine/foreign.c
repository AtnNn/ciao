
/* Foreign interface scheme due to Quintus Computer Systems, Inc. */
/* COFF patches due to Rick Stevens. */
/* m88k patches due to newton@csvax.cs.caltech.edu */

/* Support for incremental loading of foreign language modules */
/* Support for various operating system oriented predicates */

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "datadefs.h"
#include "support.h"
#include "compat.h"

#include "foreign_defs.h"


#if defined(FOREIGN_FILES)
/* declarations for global functions accessed here */

#include "alloc_defs.h"

/* local declarations */

static void add_symbol_table(void *handle, char *path);
static int write_glue_functions(FILE *f, TAGGED flist, TAGGED dlist);
static int list_to_ext_file(TAGGED flist, TAGGED dlist, char *buffer, char *pathname);
static char *list_to_string(TAGGED list, char *buffer, char *prefix);
static BOOL internal_load_foreign_files(Argdecl);
static BOOL internal_load_foreign_files(Argdecl);
#if defined(pyr)
static int activate_code(char *rawaddr, int rawsize);
#endif


#if __GNUC__ 
#define DYNCC "gcc"
#define DYNCFLAGS "-O"
#else
#define DYNCC "cc"
#define DYNCFLAGS "-O"
#endif

#if defined(LINUX) && defined(__ELF__)
#define DYNLD "gcc"
#else
#define DYNLD "ld"
#endif

#if defined(__svr4__)                                          /* Solaris */
# include <unistd.h>                                            /* sbrk () */
#else                                                            /* SunOS */
# include <sys/types.h>
# if !defined(MALLOC_DEBUG)
#  include <malloc.h>
# endif
#endif

#if defined(MALLOC_DEBUG)
# include "dmalloc.h"
#endif

#include <sys/types.h>
#include <sys/param.h>

#if !defined(MAXPATHLEN)
# define MAXPATHLEN 1024
#endif


#if defined(UTS)
#define EXEC_PAGESIZE 0x100000
#endif

#include <signal.h>
#include <sys/stat.h>

/* F_OK can be defined in more than one place */

#if !defined(F_OK)
#  define F_OK 0
#endif


#if defined(gould) || defined(NeXT) || defined(m88k) || defined(AIX) && defined(aiws) || defined(__svr4__)  || (defined(LINUX) & defined(__ELF__)) /* MCL */
#else
# include <a.out.h>
/* How do we know when we have Common Object File Format? */
# if mips || alpha
#  define COFF 1
#  define COFF_TXTOFF(x,y) N_TXTOFF(x,y)
# else
#  ifndef N_TXTOFF
#   ifdef TEXT_OFFSET
#    define N_TXTOFF(x) TEXT_OFFSET(x)
#   else
#    define COFF 1
#    define COFF_TXTOFF(x,y) (sizeof(struct filehdr)+(x).f_opthdr+(x).f_nscns*sizeof(struct scnhdr))
#   endif /* TEXT_OFFSET */
#  endif /* N_TXTOFF */
# endif /* mips */
#endif /* gould || NeXT || m88k || AIX && aiws */

#include <dlfcn.h>

struct shared_object {
  struct shared_object *next;
  void *handle;
  char path[ANY];
};
static struct shared_object *shared_objects = NULL;  /* Shared, no locked */

static void add_symbol_table(handle, path)
     void *handle;
     char *path;
{
  struct shared_object *next =
    (struct shared_object *)
    checkalloc(sizeof(struct shared_object)-ANY+strlen(path)+1);
  next->next = shared_objects;
  next->handle = handle;
  strcpy(next->path, path);
  shared_objects = next;
}

/*
static void remove_symbol_table(final)
     int final;
{
  struct shared_object *next = shared_objects;

  while (final && next)
    dlclose(next->handle),
    unlink(next->path),
    next = next->next;
}
*/

/*
static void remove_dynfli_info()
{
  struct shared_object *this = shared_objects, *next;

  while (this)
    {
      next = this->next;
      checkdealloc((TAGGED *)this, sizeof(struct shared_object));
      this = next;
    }
  shared_objects = NULL;
}  
*/

/*extern char *getenv PROTO((char *name));  */
/*extern char *malloc PROTO((unsigned int size));*//* In stdlib.h */
extern char *mktemp PROTO((char *name));

static int write_glue_functions(f,flist,dlist)
     FILE *f;
     TAGGED flist, dlist;
{
  REGISTER int i;
  REGISTER TAGGED t1, t2, u1, u2, u3;
  char /* *tmpdir, */ *sep;
  int fno, ar, valarg;
  char argmap[256];

  DEREF(t1,flist);
  DEREF(u1,dlist);
  for (fno=0; t1!=atom_nil; fno++)
    {
      DerefCar(t2,t1);
      DerefCdr(t1,t1);
      DerefCar(u2,u1);
      DerefCdr(u1,u1);

      if (!TagIsSTR(u2))
	valarg = ar = 0;
      else
	valarg = ar = Arity(TagToHeadfunctor(u2));

      for (i=1; i<=ar; i++)
	{
	  DerefArg(u3,u2,i);
	  if (TagIsLST(u3))
	    {
	      valarg = i;
	      DerefCar(u3,u3);
	      DerefArg(u3,u3,1);
	      if (TagIsATM(u3) && GetString(u3)[0]=='f')
		argmap[i] = 'F';
	      else
		argmap[i] = 'I';
	    }
	  else
	    {
	      if (GetString(TagToHeadfunctor(u3))[0]=='+')
		{
		  DerefArg(u3,u3,1);
		  if (TagIsATM(u3) && GetString(u3)[0]=='f')
		    argmap[i] = 'f';
		  else
		    argmap[i] = 'i';
		}
	      else
		argmap[i] = 'i';
	    }
	}


      fprintf(f, "static __stub_%d(__ip,__fp) register long *__ip; register double *__fp;\n{\n", fno);
      if (valarg)
	{
	  if (argmap[valarg]=='I')
	    fprintf(f, "__ip[%d] = ", (valarg-1)<<1);
	  else if (argmap[valarg]=='F')
	    fprintf(f, "extern double %s();\n", GetString(t2)),
	    fprintf(f, "__fp[%d] = ", (valarg-1));
	}
      fprintf(f ,"%s", GetString(t2)), sep = "(";
      for (i=1; i<=ar; i++)
	{
	  if (argmap[i]=='i')
	    fprintf(f, "%s __ip[%d]", sep, (i-1)<<1), sep = ",";
	  else if (argmap[i]=='f')
	    fprintf(f, "%s __fp[%d]", sep, (i-1)), sep = ",";
	}
      if (sep[0]=='(')
	fprintf(f, "();\n}\n");
      else
	fprintf(f, ");\n}\n");
    }
  return fno;
}

BOOL prolog_prepare_foreign_files(Arg) /* +Functions,+Declarations,+ExtPath */
     Argdecl;
{
  FILE *f;
  REGISTER TAGGED flist, t1;
  REGISTER int i, fno;
  
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));

  /* Re. binary/text mode in NT: this is opening a text file, so it is OK to 
     open it in text mode */

  if (!(f = fopen(GetString(X(2)),"w")))
    return FALSE;

  fno = write_glue_functions(f,X(0),X(1));
  
  fprintf(f, "int (*(ENG_pre_linkage[]))() = {\n");
  for (i=0; i<fno; i++)
    {
      fprintf(f ,"	__stub_%d,\n", i);
    }
  fprintf(f, "0};\n");
  
  flist = X(0);
  fprintf(f, "char *ENG_pre_map[] = {\n");
  while (flist != atom_nil)
    {
      DerefCar(t1,flist);
      DerefCdr(flist,flist);
      fprintf(f ,"	\"%s\",\n", GetString(t1));
    }
  fprintf(f, "0};\n");

  fclose(f);
  return TRUE;
}


static int ENG_linkage_number = 0;  /* Shared, no locked */
static int ci_base = 0;            /* Shared, no locked */
unsigned long *ci_table = NULL;

static int list_to_ext_file(flist,dlist,buffer,pathname)
     TAGGED flist, dlist;
     char *buffer;
     char *pathname;
{
  FILE *f;
  REGISTER int i;
  char *tmpdir, *filename;
  int fno;

  if ((tmpdir = getenv("TMPDIR")) == 0)
    tmpdir = "/usr/tmp";
  
  if (!(f = fopen(pathname,"w")))
    return 0;

  ENG_linkage_number++;
  fprintf(f, "extern int (*(ENG_linkage_%d[]))();\n", ENG_linkage_number);
				/* ENG_main must be first on multimax */
  fprintf(f, "int ENG_main_%d() {return (int)ENG_linkage_%d;}\n",
	  ENG_linkage_number, ENG_linkage_number);

  fno = write_glue_functions(f,flist,dlist);
  
  fprintf(f, "int (*(ENG_linkage_%d[]))() = {\n", ENG_linkage_number);
  for (i=0; i<ci_base; i++)
    {
      fprintf(f ,"	(int (*)())0lx%lx,\n", ci_table[i]);
    }
  for (i=0; i<fno; i++)
    {
      fprintf(f ,"	__stub_%d,\n", i);
    }

  fprintf(f, "0};\n");
  fclose(f);
  filename = pathname + strlen(tmpdir) + 1;
  sprintf(buffer,"cd %s; mv %s %s.c; %s %s %s -c %s.c; /bin/rm %s.c\n",
	  tmpdir, filename, filename, DYNCC, DYNCFLAGS,
#if linux && __ELF__
	  "-fPIC",
#else
#if SunOS4
	  "-fpic",
#else
	  "",
#endif
#endif
	  filename, filename);
  if (system(buffer))
    return 0;
  strcat(pathname,".o");
  return 1;
}

static char *list_to_string(list,buffer,prefix)
     TAGGED list;
     char *buffer, *prefix;
{
  REGISTER char *p;
  REGISTER TAGGED t1,t2;

  p=buffer;
  DEREF(t1,list);
  while(t1!=atom_nil)
    {
      DerefCar(t2,t1);
      DerefCdr(t1,t1);
      strcpy(p,prefix);
      p += strlen(p);
      strcpy(p,GetString(t2));
      p += strlen(p);
      *p++ = ' ';
    }
  *p++ = 0;

  p = (char *)checkalloc(p-buffer);
  strcpy(p,buffer);
  return p;
}

#if defined(aegis)
# define EXEC_PAGESIZE 0x40000
#endif

#ifndef EXEC_PAGESIZE
# ifndef PAGESIZE
#  ifndef PAGSIZ
#   define PAGSIZ 4096
#  endif
#  define PAGESIZE PAGSIZ
# endif
# define EXEC_PAGESIZE PAGESIZE
#endif

#define ADDRESS_ALIGN(cp) \
  ((char *)(((unsigned long)(cp)+EXEC_PAGESIZE-1) & ~(EXEC_PAGESIZE-1)))

/* used in saverestore.c to find the start address of the data seg from the
   passed value which is the end of the text segment. */

char *address_align(cp)
     char *cp;
{
  return ADDRESS_ALIGN(cp);
}

#if defined(pyr)

#include <sys/mman.h>

/* Mark as executable the memory block at 'addr' of legth 'size';
   adjusted to page boundaries. */
static int activate_code(rawaddr,rawsize)
     char *rawaddr;
     long rawsize;
{
  char *addr = (char *)((unsigned long)rawaddr & ~(PAGSIZ-1));
  long size = (rawsize+(rawaddr-addr)+PAGSIZ-1) & ~(PAGSIZ-1);

  return mprotect(addr,size,PROT_READ|PROT_WRITE|PROT_EXEC);
}

int reactivate_code()
{
  unsigned long u, umin=0xFFFFFFFF;
  REGISTER int i;

  for (i=0; i<ci_base; i++)
    u = ci_table[i],
    umin = (umin>u ? u : umin);

  if (umin != 0xFFFFFFFF)
    return activate_code(umin,(unsigned long)sbrk(0)-umin);
  else
    return 0;
  
}
#endif /* pyr */


#if defined(aegis) && !defined(__GNUC__)
#include <ldfcn.h>
#include <apollo/base.h>
#include <apollo/error.h>
#include <apollo/loader.h>
#endif


/* $load_foreign_files(+FileList,+LibraryList,+FunctionList,+DeclList)
 * FileList - a list of object files,
 * LibraryList - a list of library specifications,
 * FunctionList - a list of C function names,
 * DeclList - a list of their Prolog declarations
 *
 * Description: Link the files specified in FileList incrementally
 * relative to baseSymFile.  Determine the size of the linked module.
 * Allocate the needed space. Relink the files with the base of the
 * allocated space at a base address. Load outputFile into the allocated
 * space.
 *
 * This is done as a call to ld(1):
 *
 *   ld -N -x -A SYMBOL_TABLE -T ADDR -o TEMPFILE FUNCLIST FILELIST LIBLIST -lc
 *
 * (where FUNCLIST is an object file with all external references, to force
 *  these to be loaded if those symbols are defined in a library),
 * whereupon the relevant pages from TEMPFILE are read into memory at address
 * ADDR.
 *
 * On HP-UX systems, the -T option is replaced by -R.
 *
 * In SunOS Release 4.0, -x creates a bad symbol table, so we omit it.
 *
 * On Encore Multimax/Apollo/IBM AIX, the dynload()/loader_$cond_load()/load()
 * function is used instead of the above mechanism.
 *
 */

#if defined(__svr4__) || defined(LINUX)
static BOOL internal_load_foreign_files(Arg)
     Argdecl;
{
  fprintf(stderr, 
          "External lang. interface still not working in SVR4 nor LINUX\n");
  return FALSE;
}

#else

static BOOL internal_load_foreign_files(Arg)
     Argdecl;
{
#if defined(__svr4__) || defined(linux) && defined(__ELF__) || defined(IRIX) /*MCL*/
#else
  extern char *mktemp PROTO((char *));
  
  struct exec header;

  unsigned long loadImageSize, tsz, dsz;
  char *loadImageBase;
#endif

  void *handle;
  int (*funcp)();
  REGISTER TAGGED t1;
  BOOL rc = FALSE;
  FILE *loadFile = NULL;
  char *fileList = NULL;
  char *libList = NULL;
  char extPath[MAXPATHLEN];
  char outputPath[MAXPATHLEN];
  char command[5000];
  char entryString[20];
  char *tmpdir;

  if ((tmpdir = getenv("TMPDIR")) == 0) tmpdir = "/usr/tmp";
  
  fileList = list_to_string(X(0),command,"");
  libList = list_to_string(X(1),command,"");
  sprintf(extPath, "%s/extXXXXXX", tmpdir);
  mktemp(extPath);
  if (!list_to_ext_file(X(2),X(3),command,extPath)) {
    ENG_TTYPRINTF0("{ERROR: unable to create externals file}\n");
    goto bomb;
  }

#if defined(SunOS4)
  sprintf(entryString,"-e _ENG_main_%d",ENG_linkage_number);
#else
  sprintf(entryString,"-e ENG_main_%d",ENG_linkage_number);
#endif
  
  sprintf(outputPath,"%s/spsymtabXXXXXX", tmpdir);
  mktemp(outputPath);
  if (strlen(fileList)+strlen(libList)+strlen(emulator_path) > 4900) {
    ENG_TTYPRINTF0("{ERROR: link not attempted - too many arguments}\n");
    goto bomb;
  }



#if linux && __ELF__
  sprintf(command, "%s -shared %s %s %s -o %s",
          DYNLD, extPath, fileList, libList, outputPath);
#else
#if SunOS4
    sprintf(command, "%s -assert pure-text %s %s %s -o %s",
	    DYNLD, extPath, fileList, libList, outputPath);
#else
    sprintf(command, "%s -G %s %s %s -lc -o %s",
	    DYNLD, extPath, fileList, libList, outputPath);
#endif
#endif

    if (system(command)) {
      char fault_msg[70];
      sprintf(fault_msg, "%s -G failed in load_foreign_files/2", DYNLD);
      SERIOUS_FAULT(fault_msg);
      goto bomb;
    }
    if (!(handle = (void *)dlopen(outputPath, 1 /* RTLD_LAZY */))) {
      char fault_msg[120];
      sprintf(fault_msg,
              "dlopen failed in load_foreign_files/2: %s", dlerror());
      unlink(outputPath);
      goto bomb;
    }
    if (!(funcp=(unsigned int *(*)())dlsym(handle, entryString+3))) {
      char fault_msg[120];
      sprintf(fault_msg,"dlsym failed in load_foreign_files/2: %s", dlerror());
      unlink(outputPath);
      goto bomb;
    }
    add_symbol_table(handle, outputPath);


				/* compute number of functions */
  DEREF(t1,X(2));		/* FunctionList */
  for (; t1!=atom_nil; ci_base++) {
    DerefCdr(t1,t1);
  }

  ci_table = (unsigned long *)(*funcp)();
  rc = TRUE;

 bomb:
  if (loadFile) fclose(loadFile);
  if (libList) checkdealloc((TAGGED *)libList,strlen(libList)+1);
  if (fileList) checkdealloc((TAGGED *)fileList,strlen(fileList)+1);
  unlink(extPath);
  return rc;
}

#endif  /* !__svr4__ */


BOOL prolog_load_foreign_files(Arg)
     Argdecl;
{
  extern int (*(ENG_pre_linkage[]));
  extern char *ENG_pre_map[];

  int fno;
  unsigned long *ftable;
  REGISTER int i;
  REGISTER TAGGED t1, flist;
  char *fname;

  if (ENG_pre_linkage[0])
    {
      fno = 0;
      DEREF(flist,X(2));
      while (flist!=atom_nil)
	{
	  fno++;
	  DerefCdr(flist,flist);
	}
      ftable = (unsigned long *)checkalloc(fno<<2);
      DEREF(flist,X(2));
      for (fno=0; flist!=atom_nil; fno++)
	{
	  ftable[fno] = 0;
	  DerefCar(t1,flist);
	  DerefCdr(flist,flist);
	  fname = GetString(t1);
	  for (i=0; ENG_pre_map[i] && !ftable[fno]; i++)
	    if (!strcmp(fname,ENG_pre_map[i]))
	      ftable[fno] = (unsigned long)ENG_pre_linkage[i];
	  if (!ftable[fno])
	    return FALSE;	/* function not found */
	}
      if (ci_table)
	{
	  ci_table = checkrealloc(ci_table, ci_base<<2, (ci_base+fno)<<2);
	  for (i=0; i<fno; i++)
	    ci_table[ci_base++] = ftable[i];
	  checkdealloc(ftable, fno<<2);
	}
      else
	ci_table = ftable,
	ci_base = fno;
      return TRUE;
    }
  else
    return internal_load_foreign_files(Arg);  
}


BOOL prolog_foreign_base(Arg)
     Argdecl;
{
				/* return base function index */
  Unify_constant(MakeSmall(ci_base), X(0)); /* This already returns value */
  return TRUE;                                 /* Make the compiler happy */
}


#define CI_INTEGER 0
#define CI_FLOAT 1
#define CI_ATOM 2
#define CI_STRING 3
#define CI_STRINGA 4
#define CI_STRINGN 5
#define CI_ADDRESS 6
#define CI_VOID 7

static TAGGED *c_arglist;	/* C function's arg list - Shared, no locked */
static TAGGED *c_aux;		/* C function's aux data (arrays etc) */

/* treat '+' args before calling C routine */
BOOL foreign_ci_inarg(Arg,farg,spec)
     Argdecl;
     int farg, spec;
{
  REGISTER TAGGED t1, t2;
  int iarg = farg<<1;

  if ((spec&7) != CI_VOID)
    {
      t1 = X(farg);
      
      DerefSwitch(t1,t2,{return FALSE;});
    }

  switch (spec&7)
    {
    case CI_INTEGER:
      if (!IsNumber(t1)) return FALSE;
      c_arglist[iarg] = GetInteger(t1);
      return TRUE;
    case CI_FLOAT:
      if (!IsNumber(t1)) return FALSE;
      *(ENG_FLT *)(&c_arglist[iarg]) = GetFloat(t1);
      return TRUE;
    case CI_ATOM:
      if (!TagIsATM(t1)) return FALSE;
      c_arglist[iarg] = t1;
      return TRUE;
    case CI_STRING:
      if (!TagIsATM(t1)) return FALSE;
      c_arglist[iarg] = (TAGGED)GetString(t1);
      return TRUE;
    case CI_STRINGA:
      {
	REGISTER int i = spec>>3;
	REGISTER char *temp, *cp;

	if (i<0 || i>=MAXATOM) return FALSE;
	if (!TagIsATM(t1)) return FALSE;
	temp = (char *)c_aux, c_aux += (i-1)/sizeof(TAGGED)+1;
	strncpy(temp,GetString(t1),i);
	cp = temp + i;
	while(cp-- > temp && !*cp)
	  *cp = ' ';
	c_arglist[iarg] = (TAGGED)temp;
	return TRUE;
      }
    case CI_ADDRESS:
      if (!TagIsSmall(t1)) return FALSE;
      c_arglist[iarg] = (TAGGED)TermToPointerOrNull(t1);
      return TRUE;
    case CI_VOID:		/* set up c_arglist, c_data */
      c_arglist = w->global_top + farg;
      c_aux = c_arglist + (spec>>3);
      if ((TAGGED)(c_arglist) & 0x7)
	c_arglist++;		/* now aligned modulo 8 */
      return TRUE;
    default:
      return FALSE;
    }
}

/* treat '-' args before calling C routine */
BOOL foreign_ci_outarg(Arg,farg,spec)
     Argdecl;
     int farg, spec;
{
  REGISTER TAGGED t1;
  int iarg = farg<<1;

  switch (spec&7)
    {
    case CI_FLOAT:
      t1 = (TAGGED)c_aux, c_aux += 3;
      if (t1 & 0x7)
	t1 += 4;
      break;
    case CI_STRINGA:
      {
	REGISTER int i = spec>>3;
	
	if (i<0 || i>=MAXATOM) return FALSE;
	t1 = (TAGGED)c_aux, c_aux += (i-1)/sizeof(TAGGED)+1;
	break;
      }
    default:
      t1 = (TAGGED)(c_aux++);
      break;
    }

  c_arglist[iarg] = t1;
  return TRUE;
}


/* treat '-' args after calling C routine */
BOOL foreign_ci_retval(Arg,farg,spec)
     Argdecl;
     int farg, spec;
{
  REGISTER TAGGED t1;
  REGISTER int i;
  int iarg = farg<<1;

  if (farg>=256)		/* function value */
    farg-=256,
    Heap_End[-1] = (TAGGED)(&c_arglist[farg<<1]),
    iarg = Heap_End-c_arglist-1;
      

  switch (spec&7)
    {
    case CI_INTEGER:
      t1 = MakeInteger(Arg,*(ENG_INT *)c_arglist[iarg]);
      break;
    case CI_FLOAT:
      t1 = MakeFloat(Arg,*(ENG_FLT *)c_arglist[iarg]);
      break;
    case CI_ATOM:
      t1 = *(TAGGED *)c_arglist[iarg];
      if (!TagIsATM(t1)) return FALSE;
      break;
    case CI_STRING:
      t1 = MakeString(*(char **)c_arglist[iarg]);
      break;
    case CI_STRINGA:
      i = spec>>3;
      if (i<0 || i>=MAXATOM) return FALSE;
      strncpy(Atom_Buffer,(char *)c_arglist[iarg],i);
      goto padded_to_atom;
    case CI_STRINGN:
      i = spec>>3;
      if (i<0 || i>=MAXATOM) return FALSE;
      strncpy(Atom_Buffer,*(char **)c_arglist[iarg],i);
    padded_to_atom:
      {
	REGISTER int j;

	for (j=0, --i; i>=0; --i)
	  if (Atom_Buffer[i]!=' ')
	    j=i+1, i= 0;
	Atom_Buffer[j]=0;
      }
      t1 = MakeString(Atom_Buffer);
      break;
    case CI_ADDRESS:
      t1 = PointerToTermOrZero(*(TAGGED **)c_arglist[iarg]);
      break;
    default:
      return FALSE;
    }

  return cunify(Arg,t1,X(farg));
}

/* The following is a subset of the interface functions provided to C
 * by Quintus Prolog.  Not yet complete nor documented in our manual.
 */
char *ENG_string_from_atom(atom)
     ENG_INT atom;
{
  return GetString(atom);
}

TAGGED ENG_atom_from_string(string)
     char *string;
{
  return MakeString(string);
}

unsigned long ENG_padded_string_from_atom(atom,string,length)
     REGISTER char *string;
     unsigned long *atom, *length;
{
  REGISTER char *s = GetString(*atom);
  REGISTER char *cp;
  REGISTER int i = *length;
  
  strncpy(string,s,i);
  cp = string + i;
  while(cp-- > string && !*cp)
    *cp = ' ';
  return strlen(s);
}

/*  Unused???  MCL
unsigned long ENG_atom_from_padded_string(atom,string,length)
     char *string;
     unsigned long *atom, *length;
{
  REGISTER char *cp;
  REGISTER char *s = Atom_Buffer;
  REGISTER int j, i = *length;

  strncpy(s,string,i);
  j = 0;
  cp = s + i;
  while(cp-- > s && *cp == ' ');
  if(cp++ >= s)
    j = cp - s;
  s[j]=0;
  *atom = MakeString(s);
  return j;
}
*/

/* Leave a watchdog to issue a error, just in case... */

unsigned long ENG_atom_from_padded_string(atom,string,length)
     char *string;
     unsigned long *atom, *length;
{
  fprintf(stderr,
          "ENG_atom_from_padded_string called --- no definition in foreign.c\n");
  return 0;
}


#else                                                  /* ! FOREIGN_FILES */

                         /* Give error messages */

BOOL prolog_load_foreign_files(Arg)
     Argdecl;
{
  ENG_TTYPRINTF0(
         "{ERROR: $load_foreign_files: emulator not created with foreign files interface}\n");
    ENG_TTYPRINTF0("{Please recompile with foreing files option turned on}\n");
  return FALSE;
}

BOOL prolog_prepare_foreign_files(Arg) /* +Functions,+Declarations,+ExtPath */
     Argdecl;
{
    ENG_TTYPRINTF0(
         "{ERROR: $prepare_foreign_files: emulator not created with foreign files interface}\n");
    ENG_TTYPRINTF0("{Please recompile with foreing files option turned on}\n");
  return FALSE;
}

BOOL prolog_foreign_base(Arg)
     Argdecl;
{
    ENG_TTYPRINTF0(
         "{ERROR: $foreign_base: emulator not created with foreign files interface}\n");
    ENG_TTYPRINTF0("{Please recompile with foreing files option turned on}\n");
  return FALSE;

}

#endif /* Foreign_files */

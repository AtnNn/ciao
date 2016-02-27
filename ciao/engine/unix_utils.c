/*#include "fix_path.h" */ /* To rename paths like /mounted/... */


# include <string.h>
# include <sys/types.h>
# include <grp.h>
# include <sys/stat.h>
# include <fcntl.h>
# include <unistd.h>
# include <sys/socket.h>
# include <netdb.h>
# include <sys/stat.h>
# include <sys/param.h>
# include <sys/types.h>
# include <sys/wait.h>
# include <stdlib.h>
# include <dirent.h>
# include <pwd.h>
# include <ctype.h>
# include <errno.h>
# include <utime.h>

#include "datadefs.h"
#include "support.h"
#include "compat.h"

/* declarations for global functions accessed here */

#include "unix_utils_defs.h"
#include "streams_defs.h"
#include "stacks_defs.h"
#include "start_defs.h"
#include "alloc_defs.h"

/* local declarations */

extern char *library_directory;

#if defined(Solaris)
int gethostname(char *name, int namelen);
#endif

#if defined(SunOS4)
#include <string.h>
int system(char *string);
int gethostname(char *name, int namelen);
int readlink(char *path, char *buf, int bufsiz);
#endif

#ifndef MAXPATHLEN
# define MAXPATHLEN 1024
#endif

#define COPY_FLAG_OVERWRITE 0x1
#define COPY_FLAG_TIMESTAMP 0x2
#define COPY_FLAG_SYMLINK   0x4
#define COPY_FLAG_APPEND    0x8

#if defined(Win32)
#define DriveSelector(path) \
        (isalpha(path[0]) && path[1]==':' && \
         (path[2]=='/' || path[2]=='\\' || path[2]==(char)0))

#include <sys/cygwin.h>

#endif

char cwd[MAXPATHLEN+1];/* Should be private --- each thread may cd freely! */

bool_t using_windows(void)
{
#if defined(Win32)
  return !getenv("CIAOSCRIPT");
#else
  return FALSE;
#endif
}

bool_t prolog_using_windows(Arg)
     Argdecl;
{
  return using_windows();
}

bool_t expand_file_name(name, target)
     char *name;
     char *target;
{

#if !defined(__pwd_h) && !defined(_PWD_H) && !defined(__PWD_H__) && !defined(_PWD_H_)
  extern struct passwd *getpwnam PROTO((char *));
#endif

  CIAO_REGISTER char *src, *dest;
  char src_buff[MAXPATHLEN+1];

  if (!name[0]) {
    target[0] = (char)0;
    return TRUE;
  }

#if defined(Win32)
  src = name;
  dest = src_buff;
  while ((*dest = (*src == '\\') ? '/' : *src))
    ++src, ++dest;
#else
  strcpy(src_buff, name);
#endif

  /* contract // to / (non-initial in Win32) */
#if defined(Win32)
  src = dest = src_buff+1;
#else
  src = dest = src_buff;
#endif
  while ((*dest = *src)) {
    while (src[0] == '/' && src[1] == '/') ++src;
    ++dest ; ++src;
  }

  src = src_buff;
  dest = target;

  switch (*src) {
  case '$':        /* environment var */
    ++src;
  envvar:
    switch (*dest++ = *src++) {
    case 0:
    case '/':
      --src, --dest, dest[0] = (char)0;
      if (dest == target) {
        strcpy(target, library_directory);
        dest = target+strlen(target);
      } else {
        if (!(dest = getenv(target)))
          USAGE_FAULT("file name: undefined variable")
        target[0] = (char)0;
        strcpy(target, dest);
        dest = target+strlen(target);
      }
      goto st1;
    default:
      goto envvar;
    }
    break;
  case '~':        /* home directory */
    ++src;
  homedir:
    switch (*dest++ = *src++)
      {
      case 0:
      case '/':
        --src, --dest, dest[0] = (char)0;
        if (dest == target) {
          if (!(dest = getenv("HOME"))) {
            /* fprintf(stderr, "library_directory = %s\n", library_directory); */
            dest = library_directory;
          }
          strcpy(target, dest);
          dest = target+strlen(target);
        } else {
          struct passwd *pw;
          if (!(pw = getpwnam(target)))
            USAGE_FAULT("file name: no such user")
              strcpy(target, (char *)pw->pw_dir);
          dest = target+strlen(target);
        }
        goto st1;
      default:
        goto homedir;
      }
    break;
  case '/':        /* absolute path */
    src++;
    *dest++ = '/';
    break;
  default:
#if defined(Win32)
    if (DriveSelector(src)) {    /* c:/ */
      strcpy(dest, "/cygdrive/");
      dest[10] = tolower(src[0]);
      dest += 11;
      src += 2;
      goto st1;
    } else
#endif
      {
        strcpy(target, cwd);
        dest = target+strlen(target);
        if (dest[-1] != '/')
          *dest++ = '/';
      }
  }


 st0: /* prev char is '/' */
  switch (*dest++ = *src++) {
  case 0:
    if (dest-2 > target)
      dest[-2] = 0;
    goto end;
  case '/':
    goto st0;
  case '.':
    if (src[0] == '/' || src[0] == (char)0) {
      if (dest-2 >= target)
        dest -= 2;
    } else if (src[0] == '.' && (src[1] == '/' || src[1] == (char)0))	{
      if (dest-3 >= target) {
        dest -= 3;
        while (--dest, dest[0] != '/')
          ;
        src++;
      }
    }
  }

 st1: /* inside file name component */
  switch (*dest++ = *src++) {
  case 0:
    goto end;
  case '/':
    goto st0;
  default:
    goto st1;
  }

 end:
  if (target[0] == (char)0) /* root directory */
    target[0] = '/', target[1] = (char)0;
  return TRUE;
}

#if defined(FIX_PATHS) /* Renaming paths like /mounted/... */

struct ren_pair { char *from; char *to; };

static struct ren_pair rename_path_pairs[] = REN_PAIRS;

int fix_path(path)
     char *path;
{
  char *from, *p1, buf[MAXPATHLEN+1];
  struct ren_pair *rp;

  for (rp = rename_path_pairs; *(from = rp->from) ; rp++) {
    for (p1 = path ; *from && *p1 ; from++, p1++) {
      if (*from != *p1) {break;}; /* "path" does not start with "from" */
    }
    if (! *from) { /* "path" starts with "from" */
      strcpy(buf, p1);
      strcpy(path, rp->to);
      strcat(path, buf);
      return TRUE;
    }
  }

  return FALSE;
}
#endif


void compute_cwd()
{
  getcwd(cwd, MAXPATHLEN+1);

#if defined(FIX_PATHS)
  fix_path(cwd);
#endif
}


bool_t prolog_unix_cd(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:working_directory", 2);
  char pathBuf[MAXPATHLEN+1];
/*   struct stat statbuf; */
  Unify_constant(MakeString(cwd), X(0));
  DEREF(X(0), X(0));

  DEREF(X(1), X(1));
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }

  /* check type argument*/
  if (!IsAtom(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), X(1), 2);
  }
  /* check argument domain error */
  if (!expand_file_name(GetString(X(1)), pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(1), 2);
  }
  /* If there is another problem ...*/
  if (chdir(pathBuf)) {
    if (current_ferror_flag==atom_on) {
	switch (errno) {
	  case ENOENT: /* File does not exists */ 
	    BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(1), 2);
	    break ;
	  case EACCES: /* We dont have permissions in the directory */
	    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(1), 2);
	    break ;
	  default: /* Who knows */
	    BUILTIN_ERROR(SYSTEM_ERROR, X(1), 2);
	    break ;
	}
    } else {
      /* Silently fails */
      return FALSE;
    }
  }

  compute_cwd();
  return TRUE;
}

// This tries to execute 'command' in $SHELL.  If $SHELL is not set, then
// shell_available is set to FALSE, else it is set to TRUE.  If
// command == NULL no command is executed (the shell is just called).
// Otherwise, command is passed to $SHELL, and the return code of the
// execution is returned as the result of evaluating the function.  This is
// intended to be wrapper both for shell/0, shell/1, shell/2, and, maybe,
// system/1.

// Here we should give better errors!  However there is no leeway to return
// them within the ISO-defined exceptions.  Besides, when an exception
// involves a newly created process which is not able to accomplish its
// task, the (internally created) exception should force the process to
// finish as well.


int ciao_shell_start(const char *command)
{
  char *shellname =  getenv("SHELL");
  pid_t pid;
  int retcode;

  if (shellname == NULL) {  // This means an error if no SHELL
      MAJOR_FAULT("No SHELL environment variable defined");
  } else {
    if ((pid = vfork()) == 0) {  // Child
      if (command == NULL) // Just start the shell
        execlp(shellname, shellname, NULL);
      else // -c is standard to "execute this command"
        execlp(shellname, shellname, "-c", command, NULL);
      _EXIT("Execution of $SHELL failed");
    } else if (pid < 0) { // Could not fork
      MAJOR_FAULT("Could not start process for new shell $SHELL");
    } else {              // Parent
      waitpid(pid, &retcode, 0);
     }
  }
    return retcode;
}

bool_t prolog_unix_shell0(Arg)
     Argdecl;
{
  // ERR__FUNCTOR("system:shell", 0);
  int retcode;

  retcode = ciao_shell_start(NULL);
  return (retcode == 0);
}

bool_t prolog_unix_shell2(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:shell", 2);
  CIAO_REGISTER int system_result;

#if defined(USE_DYNAMIC_ATOM_SIZE)
  char *cbuf = (char *)checkalloc(2*MAXATOM+MAXPATHLEN+20);
#else
  char cbuf[2*MAXATOM+MAXPATHLEN+20];
#endif

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument*/
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  system_result = ciao_shell_start(GetString(X(0)));

#if defined(USE_DYNAMIC_ATOM_SIZE)
    checkdealloc((tagged_t *)cbuf, 2*MAXATOM+MAXPATHLEN+20);
#endif
    return cunify(Arg, MakeSmall(system_result), X(1));
}

extern char ** environ;

char *shell_system = "/bin/sh";

static int fixed_system(char *command) {
  int pid, status = 0;
  
  if (command == NULL)
    return 1;
  pid = vfork();
  if (pid == 0) {
    char *argv[4];
    argv[0] = "sh";
    argv[1] = "-c";
    argv[2] = command;
    argv[3] = NULL;
    execve(shell_system, argv, environ);
    _exit(127);
  }
  else if (pid < 0)
    status = -1;
  else {
    if (waitpid(pid, &status, 0) != pid)
      status = -1;
  }
  return status;
}

bool_t prolog_unix_system2(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:system", 2);
  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument*/
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  return cunify(Arg, MakeSmall(fixed_system(GetString(X(0)))), X(1));
}

/* Return the arguments with which the current prolog was invoked */


bool_t prolog_unix_argv(Arg)
     Argdecl;
{
  CIAO_REGISTER tagged_t list = atom_nil;
  CIAO_REGISTER char **p1 = prolog_argv;
  CIAO_REGISTER int i;

  for (i=prolog_argc; i>1;) {
    MakeLST(list, MakeString(p1[--i]), list);
  }
  return cunify(Arg, list, X(0));
}

/* //) ( (+
   bool_t prolog_unix_exit(Arg)
   Argdecl;
   {
   DEREF(X(0), X(0));

   exit(GetSmall(X(0)));
   return TRUE;
   }
*/

bool_t check_errno(Argdecl, int index)
{
  /* Note: This ERR__FUNCTOR is not related to an exported predicate,
     since prolog_constant_codes is called from other places. I have
     some ideas of refining error reporting without hindering
     performance (please, ask me before changing this code). -- JFMC
   */
  ERR__FUNCTOR("system:$throw_errno", 1);
  switch(errno) {
  case EISDIR: 
  case ENAMETOOLONG:
  case ENOTDIR: 
  case EFAULT:
    /* TODO: This is not a domain error error, leave it such as until
             we find a better solution */
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(index), index + 1);
    break;
  case EPERM:
  case EACCES:
  case EROFS:
    BUILTIN_ERROR(PERMISSION_ERROR(OPEN, SOURCE_SINK), X(index), index + 1);
    break;
  case ENOENT:
    BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(index), index + 1);
    break;
  case EMFILE:
  case ENFILE:
  case ENOMEM:
  case ELOOP:
  case ENOSPC:
    BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED), X(index), index + 1);
    break;
  case EEXIST:
    BUILTIN_ERROR(SYSTEM_ERROR, X(index), index + 1);
    break;
  default:
    BUILTIN_ERROR(SYSTEM_ERROR, X(index), index + 1);
    break;
  }
}

bool_t prolog_unix_mktemp(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:mktemp", 2);
  char template[STATICMAXATOM];
  int fildes;

  //  extern char *mktemp PROTO((char *));  // Not used any longer

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);

  /* check type argument*/
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  strcpy(template, GetString(X(0)));

  /* if mkstemp fails, give a system error */
  if ((fildes = mkstemp(template)) <  0) {
    return check_errno(Arg, 0);
  } else {
    close(fildes);   // Do not leave it open, since the stream is not seen
                     // at Prolog level
    return cunify(Arg, MakeString(template), X(1));
  }
}

bool_t prolog_unix_access(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:file_exists", 2);
  char pathBuf[MAXPATHLEN+1];
  int mode;

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument*/
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  DEREF(X(1), X(1));

  if (!TagIsSmall(X(1)) || (mode = GetSmall(X(1))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(1), 2, BYTE);

  if (!expand_file_name(GetString(X(0)), pathBuf))
    return FALSE;

  if (access(pathBuf, mode))
    {
/*       perror("% access in file_exists/2"); /\* --this must be quiet. *\/ */
      /*  MINOR_FAULT("access() failed");  */
      /* --MCL: no need to raise any exception */
      return FALSE;
    }
  return TRUE;
}

/* directory_files(+Path, FileList) */

bool_t prolog_directory_files(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:directory_files", 2);
  char pathBuf[MAXPATHLEN+1];
  DIR *dir;
  int gap;
  struct dirent *direntry;

  /* Using X(2) to build the result - DCG */

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check domain argument */
  if (!expand_file_name(GetString(X(0)), pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  
  /* Check for system errors */
  if (! (dir = opendir(pathBuf))) {
    if (current_ferror_flag==atom_on) {
      /* First, identifying the error type: */
      switch(errno) {
      case EACCES:
	BUILTIN_ERROR(PERMISSION_ERROR(OPEN, SOURCE_SINK), X(0), 1);
	break;
      case ENOENT:
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1);
	break;
      case ENOTDIR:
	BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
	break;
      case EMFILE:
      case ENFILE:
      case ENOMEM:
	BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED), X(0), 1);
	break;
      default:
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  } else {
    X(2) = atom_nil;
    gap = HeapDifference(w->global_top, Heap_End)-CONTPAD;
    while ((direntry = readdir(dir))) {
      if ((gap -= 2) < 0) {
	explicit_heap_overflow(Arg, CONTPAD+32, 3);
	gap += 32;
      }
      MakeLST(X(2), MakeString(direntry->d_name), X(2));
    }
    closedir(dir);
  }
  return cunify(Arg, X(2), X(1));
}

/* file_properties(+File, Type, Linkto, ModTime, Protection, Size)

   ModTime: the time (in seconds since 1, Jan, 1970), since file File
   (absolute path) was last modified.
*/

bool_t prolog_file_properties(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:file_properties", 6);
  struct stat statbuf;
  char pathBuf[MAXPATHLEN+1];
  char symlinkName[STATICMAXATOM+1];
  int len;

  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check argument domain error */
  if (!expand_file_name(GetString(X(0)), pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  DEREF(X(2), X(2));
  if (X(2) != atom_nil) { /* Link wanted */
    symlinkName[0] = (char) 0;
    if ((len=readlink(pathBuf, symlinkName, STATICMAXATOM)) > 0) {
      symlinkName[len] = (char) 0;
    }
    Unify_constant(MakeString(symlinkName), X(2));
  }

  DEREF(X(1), X(1));
  DEREF(X(3), X(3));
  DEREF(X(4), X(4));
  DEREF(X(5), X(5));
  if (   (X(1)!=atom_nil)
         || (X(3)!=atom_nil)
         || (X(4)!=atom_nil)
         || (X(5)!=atom_nil) ) {

    if (stat(pathBuf, &statbuf)) {
      if (current_ferror_flag==atom_on) {
	switch (errno) {
	  case ENOENT: /* File does not exists */ 
	    BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1);
	    break ;
	  case EACCES: /* We dont have permissions in the directory */
	    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	    break ;
	  default: /* Who knows */
	    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);		    
	    break ;
	  }
      } else {
	/* Silently fails */
	return FALSE;
      }
    }

    if (X(1)!=atom_nil) {
      Unify_constant(( S_ISREG(statbuf.st_mode) ? atom_regular
                       : S_ISDIR(statbuf.st_mode) ? atom_directory
                       : S_ISLNK(statbuf.st_mode) ? atom_symlink
                       : S_ISFIFO(statbuf.st_mode) ? atom_fifo
                       : S_ISSOCK(statbuf.st_mode) ? atom_socket
                       : atom_unknown), X(1));
    }

    if (X(3)!=atom_nil) {
      /* Cannot be Unify_constant because it is a large integer */
      if (!cunify(Arg, MakeInteger(Arg, statbuf.st_mtime), X(3))) {
	return FALSE;
      }
    }
    if (X(4)!=atom_nil) {
      Unify_constant(MakeSmall(statbuf.st_mode&0xfff), X(4));
    }
    if (X(5)!=atom_nil) {
      Unify_constant(MakeSmall(statbuf.st_size), X(5));
    }
  }

  return TRUE;
}

/* prolog_touch(+Path) */

bool_t prolog_touch(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:touch", 1);
  char file[MAXPATHLEN+1];
  int status;
  int fd = -1;
  int open_errno = 0;

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument */
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  if (!expand_file_name(GetString(X(0)), file))
    return FALSE;

  /* Try to open the file, create it if necessary. */
  fd = open(file, O_WRONLY | O_CREAT | O_NONBLOCK | O_NOCTTY,
	    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  if (fd == -1) open_errno = errno;

  if (fd != -1 && close(fd) < 0) {
    BUILTIN_ERROR(PERMISSION_ERROR(OPEN, SOURCE_SINK), X(0), 1);
  }

  /* If we have write access to the file, but do not own it, we will
     not be able to set the modification time arbitrarily. If we pass
     NULL, we just update it to the current time (which is allowed) */
  status = utime(file, NULL);

  if (status) {
    if (open_errno) {
      /* TODO: open_errno is not used */
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
    } else {
      /* TODO: errno is not used */
      BUILTIN_ERROR(PERMISSION_ERROR(MODIFY, SOURCE_SINK), X(0), 1);
    }
  }

  return TRUE;
}

bool_t prolog_unix_chmod(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:chmod", 2);
  char pathBuf[MAXPATHLEN+1];
/*   struct stat statbuf; */
  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check domain argument */
  if (!expand_file_name(GetString(X(0)), pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  DEREF(X(1), X(1));
  /* check instatiation error to the other argument*/
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }
  /* and check type argument again */
  if (!TagIsSmall(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(1), 2);
  }

  /* make call to chmod, if there is any proble, raise a system error */
  if (chmod(pathBuf, GetSmall(X(1)))) {
    if (current_ferror_flag==atom_on) {
	switch (errno) {
	case ENOENT: /* File does not exists */ 
	  BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(1), 2);
	  break ;
	case EACCES: /* We dont have permissions in the directory */
	  BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(1), 2);
	  break ;
	default: /* Who knows */
	  BUILTIN_ERROR(SYSTEM_ERROR, X(1), 2);
	  break ;
	}
    } else {
      return FALSE;
    }   
  }
 
  return TRUE;
}

bool_t prolog_unix_umask(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:umask", 2);
  int i;

  DEREF(X(1), X(1));
  /* check argument instantiation error */
  if (IsVar(X(1))) {
    if (X(1)==X(0)) {
      i = umask(0);
      (void)umask(i);
      return cunify(Arg, MakeSmall(i), X(0));
    } else
      BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  } else {
    /* check type argument*/
    if (!TagIsSmall(X(1)))
      BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(1), 2);
    return cunify(Arg, MakeSmall(umask(GetSmall(X(1)))), X(0));
  }
}




bool_t prolog_unix_delete(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:delete_file", 1);
  char pathBuf[MAXPATHLEN+1];

  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check argument domain error */
  if (!expand_file_name(GetString(X(0)), pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(1), 2);
  }
  /* Try to unlink, if anything go wrong, raise an error */
  if (unlink(pathBuf)) {
    if ( current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1); 
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break ;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  }
    
  return TRUE;
}


bool_t prolog_unix_rename(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:rename_file", 2);
  char
    orig_name[MAXPATHLEN+1],
    new_name[MAXPATHLEN+1];

  DEREF(X(0), X(0));
  /* check instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  DEREF(X(1), X(1));
  /* check instantiation error to the other argument */
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }
  /* check type the other argument*/
  if (!TagIsATM(X(1))) {
    ERROR_IN_ARG(X(1), 2, STRICT_ATOM);
  }
  /* check domain of the two arguments */
  if (!expand_file_name(GetString(X(0)), orig_name)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  if (!expand_file_name(GetString(X(1)), new_name)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(1), 2);
  }
  /* if anything fails, raise and exception */
  if (rename(orig_name, new_name)) {
    if ( current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(1), 2); 
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(1), 2);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(1), 2);
	break ;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  }

  return TRUE;
}


bool_t prolog_unix_mkdir(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:make_directory", 2);
  char dirname[MAXPATHLEN+1];
  int mode;

  DEREF(X(0), X(0));
  /* check instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check domain argument */
  if (!expand_file_name(GetString(X(0)), dirname)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  DEREF(X(1), X(1));
  /* check instantiation error */
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }
  /* check type argument*/
  if (!TagIsSmall(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(1), 2);
  }
  mode = GetSmall(X(1));
  
  /* call to mkdir, if there is a problem, raise a system error */
  if (mkdir(dirname, mode)) {
    if ( current_ferror_flag==atom_on) {
      switch (errno) {
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	break ;
      case ENOENT: /* Path does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1); 
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break ;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  }
       
  return TRUE;
}


bool_t prolog_unix_rmdir(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:delete_directory", 1);
  char dirname[MAXPATHLEN+1];
  /*   struct stat statbuf; */
  DEREF(X(0), X(0));
  /* check instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* Check domain error */
  if (!expand_file_name(GetString(X(0)), dirname)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  /* check that file exists */

  /* try to make rmdir, else, system_error */
  if (rmdir(dirname)) {
    if ( current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1); 
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break ;
      }
    } else {
      return FALSE;
    }
  }
  return TRUE;
}



/*
 *  current_host(?HostName).
 */
bool_t prolog_current_host(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:current_host", 1);
  char hostname[MAXHOSTNAMELEN*4];

  if (gethostname(hostname, sizeof(hostname)) < 0)
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

  if (!strchr(hostname, '.')) {
    struct hostent *host_entry;
    char **aliases;

    /* If the name is not qualified, then pass the name through the name
       server to try get it fully qualified */
    /* if null, its a system error */
    if ((host_entry = gethostbyname(hostname)) == NULL)
      BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
    strcpy(hostname, host_entry->h_name);

    /* If h_name is not qualified, try one of the aliases */

    if ((aliases=host_entry->h_aliases)) {
      while (!strchr(hostname, '.') && *aliases)
	strcpy(hostname, *aliases++);
      if (!strchr(hostname, '.'))
	strcpy(hostname, host_entry->h_name);
    }

#if HAS_NIS
    /* If still unqualified, then get the domain name explicitly.
       This code is NIS specific, and causes problems on some machines.
       Apollos don't have getdomainname, for example. */
    if (!strchr(hostname, '.')) {
      char domain[MAXHOSTNAMELEN*3];

      if (getdomainname(domain, sizeof(domain)) < 0)
	BUILTIN_ERROR(SYSTEM_ERROR, Arg, 1);
      strcat(hostname, ".");
      strcat(hostname, domain);
    }
#endif
    /*free(host_entry);*/
  }

  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(hostname), X(0));
}

/* internal_getenvstr(+Name, -Value) */

/* bool_t prolog_c_getenvstr(Arg) */
/*      Argdecl; */
/* { */
/*   char *s; */
/*   int i; */
/*   tagged_t cdr; */

/*   DEREF(X(0), X(0)); */
/*   DEREF(X(1), X(1)); */
/*   /\* check instantiation error *\/ */
/*   /\* */
/*   if (IsVar(X(0))) */
/*     BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1); */
/*   *\/ */
/*   /\* check type argument*\/ */
/*   /\* */
/*   if (!TagIsATM(X(0))) */
/*     ERROR_IN_ARG(X(0), 1, STRICT_ATOM); */
/*   *\/ */
/*   if ((s = getenv(GetString(X(0)))) == NULL) return FALSE; */

/*   s += (i = strlen(s)); */

/*   if (HeapDifference(w->global_top, Heap_End)<CONTPAD+(i<<1)) */
/*     explicit_heap_overflow(Arg, CONTPAD+(i<<1), 2); */

/*   cdr = atom_nil; */
/*   while (i>0) { */
/*     i--; */
/*     MakeLST(cdr, MakeSmall(*(--s)), cdr); */
/*   } */
/*   return cunify(Arg, cdr, X(1)); */
/* } */


/* setenvstr(+Name, +Value) */

#if defined(Solaris)
/* emulate setenv in terms of putenv (from rpm 2.0.9) */
int setenv(const char *name, const char *value, int overwrite)
{
  int len;
  if (!overwrite && getenv(name)) return 0;
  len = strlen(name) + strlen(value) + 2;
  if (len < 255) {
    char buf[256];
    strcpy(buf, name);
    strcat(buf, "=");
    strcat(buf, value);
    return putenv(buf);
  } else {
    char *buf = (char *)checkalloc(len);
    int ret;
    strcpy(buf, name);
    strcat(buf, "=");
    strcat(buf, value);
    ret = putenv(buf);
    checkdealloc((tagged_t *)buf, len);
    return ret;
  }
}
#endif


/* bool_t prolog_c_setenvstr(Arg) */
/*      Argdecl; */
/* { */
/*   char *variable,                          /\* The variable we want to set *\/ */
/*     *s;                                 /\* The value we want to assign *\/ */
/*   int i,                                                  /\* String index *\/ */
/*     len = 8;                                           /\* String length *\/ */
/*   tagged_t car;                          /\* Pointer to the head of the list *\/ */
/*   tagged_t value;                                   /\* To traverse the list *\/ */
/*   int carvalue;                          /\* Value of the head of the list *\/ */

/*   DEREF(X(0), X(0)); */
/*   DEREF(X(1), X(1)); */

/*   /\* Minimal check: variable name as atom, value as string (why this */
/*      difference?) *\/  */
/*   /\* */
/*   if (!TagIsATM(X(0))) */
/*     BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), X(0), 1); */

/*   if (!TagIsLST(X(1))) */
/*     BUILTIN_ERROR(TYPE_ERROR(CHARACTER_CODE_LIST), X(1), 2); */
/*   *\/ */
/*   variable = GetString(X(0)); */
/*   value = X(1); */
/*   s = (char *)malloc(len*sizeof(char)); */
/*   i = 0; */

/*   while (TagIsLST(value)){ */
/*     DerefCar(car, value); */
/*     if (!TagIsSmall(car) ||  */
/*         ((carvalue = GetSmall(car)) > 255) || */
/*         carvalue < 0){ */
/*       BUILTIN_ERROR(TYPE_ERROR(CHARACTER_CODE_LIST), X(1), 2); */
/*     } else s[i++] = (char)carvalue; */

/*     if (i == len){ /\* Length exceeded! *\/ */
/*       len = len * 2; */
/*       s = (char *)realloc(s, len); */
/*     }  */
/*     DerefCdr(value, value); */
/*   } */

/*   s[i] = '\0'; */
/*   setenv(variable, s, 1); */
/*   free(s); */
/*   return TRUE; */
/* } */

/* By Edison Mera: */

bool_t prolog_c_winpath(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
#if defined(Win32)
  {
    char *posixpath;
    char winpath[MAX_PATH + 1];
    posixpath = GetString(X(0));
    cygwin32_conv_to_full_win32_path(posixpath, winpath);
    return cunify(Arg, MakeString(winpath), X(1));
  }
#else
  return cunify(Arg, X(0), X(1));
#endif
}

bool_t prolog_c_winfile(Arg)
     Argdecl;
{
#if defined(Win32)
  {
    char *posixpath;
    char winpath[MAX_PATH + 1];
    posixpath = GetString(X(0));
    cygwin32_conv_to_win32_path(posixpath, winpath);
    return cunify(Arg, MakeString(winpath), X(1));
  }
#else
  return cunify(Arg, X(0), X(1));
#endif
}

bool_t prolog_c_posixpath(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
#if defined(Win32)
  {
    char *winpath;
    char posixpath[MAX_PATH + 1];
    winpath = GetString(X(0));
    cygwin32_conv_to_full_posix_path(winpath, posixpath);
    return cunify(Arg, MakeString(posixpath), X(1));
  }
#else
  return cunify(Arg, X(0), X(1));
#endif
}

bool_t prolog_c_posixfile(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
#if defined(Win32)
  {
    char *winpath;
    char posixpath[MAX_PATH + 1];
    winpath = GetString(X(0));
    cygwin32_conv_to_posix_path(winpath, posixpath);
    return cunify(Arg, MakeString(posixpath), X(1));
  }
#else
  return cunify(Arg, X(0), X(1));
#endif
}

bool_t prolog_c_errno(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeSmall(errno), X(0));
}

bool_t prolog_c_strerror(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(strerror(errno)), X(0));
}

#define BUF_MAX 65536

bool_t prolog_c_copy_file(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:copy_file", 2);
  char *source, *destination;
  int fd_source, fd_destination, flags;
  int copy_flag;
  ssize_t s;
  struct stat stat_buf;
  char buffer[BUF_MAX];
  errno = 0;
  DEREF(X(0), X(0));
  source = GetString(X(0));
  DEREF(X(1), X(1));
  destination = GetString(X(1));
  DEREF(X(2), X(2));
  copy_flag = GetInteger(X(2));
  if (copy_flag & COPY_FLAG_SYMLINK) {
    if (copy_flag & COPY_FLAG_OVERWRITE) {
      if(unlink(destination)==-1) {
	if (errno != ENOENT)
	  return check_errno(Arg, 1);
      }
#if defined(__CYGWIN32__) || defined(__CYGWIN__)
      if(access(destination, 0)) {
	if(unlink(destination)==-1) {
	  if (errno != ENOENT)
	    return check_errno(Arg, 1);
	}
      }
#endif
    }
    if (symlink(source, destination)==-1) {
      return check_errno(Arg, 1);
    }
  }
  else {
    fd_source = open(source, O_RDONLY);
    if(fd_source==-1) {
      /* First, identifying the error type: */
      return check_errno(Arg, 0);
    }

    if(copy_flag & COPY_FLAG_OVERWRITE) {
      flags = O_WRONLY|O_CREAT|O_TRUNC;
    }
    else if(copy_flag & COPY_FLAG_APPEND) {
      flags = O_WRONLY|O_CREAT|O_APPEND;
    }
    else {
      flags = O_WRONLY|O_CREAT|O_EXCL;
    }
    fstat(fd_source, &stat_buf);
    fd_destination = open(destination, flags, stat_buf.st_mode);
    if (fd_destination==-1) {
      /* Now we must close source */
      close(fd_source);
      /* Identifying the error type: */
      return check_errno(Arg, 1);
    }
    while((s=read(fd_source, buffer, BUF_MAX))!=0){
      if(s==-1){
	close(fd_source);
	close(fd_destination);
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
      }
      else
	{
	  if(write(fd_destination, buffer, s)==-1){
	    close(fd_source);
	    close(fd_destination);
	    BUILTIN_ERROR(SYSTEM_ERROR, X(1), 2);
	  }
	}
    }
    close(fd_source);
    close(fd_destination);
    if(copy_flag & COPY_FLAG_TIMESTAMP) {
      struct utimbuf buf;
      buf.actime  = stat_buf.st_atime;
      buf.modtime = stat_buf.st_mtime;
      utime(destination, &buf);
    }
  }
  return TRUE;
}

bool_t prolog_c_get_env(Arg)
     Argdecl;
{
  char *name, *value;
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));
  name = GetString(X(0));
  value = getenv(name);
  if(value==NULL)
    return FALSE;
  else
    return cunify(Arg, MakeString(value), X(1));
}

bool_t prolog_c_set_env(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:set_env", 2);
  char *name, *value;
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));
  name = GetString(X(0));
  value = GetString(X(1));
  if(setenv(name, value, 1)) {
    BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED), X(0), 1);
  } else {
    return TRUE;
  }
}

bool_t prolog_c_del_env(Arg)
     Argdecl;
{
  char *name;
  DEREF(X(0), X(0));
  name = GetString(X(0));
#if defined(Solaris)
  putenv(name);
#else
  unsetenv(name);
#endif
  return TRUE;
}

bool_t prolog_c_current_env(Arg)
     Argdecl;
{
  int n, index, r_val;
  char *name, *value;
  DEREF(X(0), X(0));
  index = GetInteger(X(0));
  name = environ[index];
  if(name!=NULL)
    {
      DEREF(X(1), X(1));
      DEREF(X(2), X(2));
      value = strchr(environ[index], '=');
      n = (int)(value-name);
      value++;
      name = memcpy(
		    (char *)checkalloc((int)(n+1)*sizeof(char)), 
		    name, (int)(n+1)*sizeof(char));
      name[n] = '\0';
      r_val = cunify(Arg, MakeString(name), X(1)) &
	cunify(Arg, MakeString(value), X(2));
      checkdealloc((tagged_t *)name, (int)(n+1)*sizeof(char));
      return r_val;
    }
  else
    {
      return FALSE; //Must fail when raise the final
    }
}


/*
  pause(+Seconds): make this process sleep for Seconds seconds
*/

bool_t prolog_pause(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:pause", 1);
  tagged_t x0;
  long time;

  DEREF(x0, X(0));
  /* check instantiation_error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument*/
  if (!TagIsSmall(x0))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(0), 1);
  time = GetSmall(x0);

  sleep(time);

  return TRUE;
}


/*
  get_pid(?PID): PID is unified with  the process identificator number
  of this process
*/

bool_t prolog_getpid(Arg)
     Argdecl;
{
  tagged_t x0;

  DEREF(x0, X(0));
  return cunify(Arg, x0, MakeSmall(getpid()));
}

bool_t prolog_getuid(Arg)
     Argdecl;
{
  tagged_t x0;

  DEREF(x0, X(0));
  return cunify(Arg, x0, MakeSmall(getuid()));
}

bool_t prolog_getgid(Arg)
     Argdecl;
{
  tagged_t x0;

  DEREF(x0, X(0));
  return cunify(Arg, x0, MakeSmall(getgid()));  
}

bool_t prolog_getgrnam(Arg)
     Argdecl;
{
  tagged_t x0;
  struct group * g;

  DEREF(x0, X(0));
  g = getgrgid(getgid());
  if (g == NULL) {
    return FALSE; 
  } else { 
    return cunify(Arg, x0, MakeString(g->gr_name)); 
  } 
}

bool_t prolog_getpwnam(Arg)
     Argdecl;
{
  tagged_t x0;
  struct passwd * p;

  DEREF(x0, X(0));
  p = getpwuid(getuid());
  return cunify(Arg, x0, MakeString(p->pw_name));

}


/* $find_file(+LibDir, +Path, +Opt, +Suffix, ?Found, -AbsPath, -AbsBase, -AbsDir)
 * string LibDir	a library in which to search for Path
 * string Path		a path, may be absolute or relative. If LibDir
 *			is specified then Path must be relative to LibDir.
 * string Opt           an optional suffix to Path, must precede Suffix, is
 *                      included in AbsBase
 * string Suffix        an optional suffix to Path, not included in AbsBase
 * atom   Found         true or fail
 * string AbsPath       the absolute pathname of Path
 * string AbsBase       the absolute pathname of Path, without Suffix
 * string AbsDir        the absolute pathname of the directory of Path
 *
 * Description: Try to find in LibDir, in this order:
 *   Path+Opt+Suffix
 *   Path+Suffix
 *   Path
 *   Path/Path+Opt+Suffix
 *   Path/Path+Suffix
 *   Path/Path
 * if any found, unify Found with true, and return in AbsPath, AbsBase and
 * AbsDir the appropriate values, else unify Found with false, and return in
 * AbsPath, AbsBase and AbsDir the values corresponding to the last option
 * (no Opt nor Suffix).
 */

#if !defined(S_ISDIR)                                 /* Notably, Solaris */
#  define S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#endif

bool_t prolog_find_file(Arg)
     Argdecl;
{
  char *libDir, *path, *opt, *suffix;
  char pathBuf[MAXPATHLEN+8];
  /* MAXATOM may change dinamically to make room for longer atoms */
  long relBufSize =2*MAXATOM+2;
  char *relBuf = (char *)checkalloc(relBufSize);
  CIAO_REGISTER char *bp;
  char *cp;
  struct stat file_status;
  time_t t_opt, t_pri;

  DEREF(X(0), X(0));
  libDir = GetString(X(0));
  DEREF(X(1), X(1));
  path = GetString(X(1));
  DEREF(X(2), X(2));
  opt = GetString(X(2));
  DEREF(X(3), X(3));
  suffix = GetString(X(3));

  if (path[0] == '/' || path[0] == '$' || path[0] == '~'
#if defined(Win32)
      || path[0] == '\\' || DriveSelector(path)
#endif
      ) {
    strcpy(relBuf, path);
  } else {
    strcpy(relBuf, libDir);
    if (relBuf[strlen(relBuf)-1]!='/')
      strcat(relBuf, "/");
    strcat(relBuf, path);
  }

  if (!expand_file_name(relBuf, pathBuf)){
    checkdealloc((tagged_t *)relBuf, relBufSize);
    return FALSE;
  }

#if defined(FIX_PATHS)
  fix_path(pathBuf);
#endif

  cp = pathBuf + strlen(pathBuf);

  t_opt = t_pri = 0;

 searchPath:

  if (*opt) {
    strcpy(cp, opt);
    bp = cp + strlen(cp);
    strcpy(bp, suffix);
    if(!access(pathBuf, F_OK)) {
      stat(pathBuf, &file_status);
      if (!S_ISDIR(file_status.st_mode))
        t_opt = file_status.st_mtime;    /* found path+opt+suffix */
    }
  }

  bp = cp;

  if (*suffix) {
    strcpy(bp, suffix);
    if(!access(pathBuf, F_OK)) {
      stat(pathBuf, &file_status);
      if (!S_ISDIR(file_status.st_mode))
        t_pri = file_status.st_mtime;    /* found path+suffix */
    }
  }

  if (t_pri > t_opt) { /* path+suffix exists, path+opt+suffix older|absent */
    Unify_constant(atom_true, X(4));
    goto giveVals;
  } else if (t_opt > 0) { /* newer path+opt+suffix exists */
    /* recreate opt+suffix */
    strcpy(cp, opt);
    bp = cp + strlen(cp);
    strcpy(bp, suffix);
    Unify_constant(atom_true, X(4));
    goto giveVals;
  }

  *bp = 0;

  if(!access(pathBuf, F_OK)){
    stat(pathBuf, &file_status);
    if (S_ISDIR(file_status.st_mode)) {    /* directory */
      while (*bp!='/') --bp;               /* duplicate dir name */
      *cp++ = *bp++ ;
      while (*bp!='/')
        *cp++ = *bp++ ;
      *cp = 0;
      goto searchPath;                     /* search inside */
    } else {
      Unify_constant(atom_true, X(4));      /* found path */
      if (*suffix && strcmp(bp -= strlen(suffix), suffix))
        /* does not end in suffix */
        bp = cp;
      goto giveVals;
    }
  }

  Unify_constant(atom_fail, X(4));

 giveVals:

  Unify_constant(MakeString(pathBuf), X(5));

  *bp = 0;

  Unify_constant(MakeString(pathBuf), X(6));

  while (*bp!='/')
    --bp;
  *bp = 0;

  Unify_constant(MakeString(pathBuf), X(7));

  checkdealloc((tagged_t *)relBuf, relBufSize);
  return TRUE;
}

bool_t prolog_path_is_absolute(Arg)
     Argdecl;
{
  char *path;

  DEREF(X(0), X(0));
  path = GetString(X(0));

  if (path[0] == '/' || path[0] == '$' || path[0] == '~'
#if defined(Win32)
      || path[0] == '\\' || DriveSelector(path)
#endif
      ) {
    return TRUE;
  } else {
    return FALSE;
  }
}

bool_t prolog_expand_file_name(Arg)
     Argdecl;
{
  char *path;
  /* MAXATOM may change dinamically to make room for longer atoms */
  long relBufSize =2*MAXATOM+2;
  char *relBuf = (char *)checkalloc(relBufSize);

  DEREF(X(0), X(0));
  path = GetString(X(0));

  if (!expand_file_name(path, relBuf)){
    checkdealloc((tagged_t *)relBuf, relBufSize);
    return FALSE;
  }

#if defined(FIX_PATHS)
  fix_path(relBuf);
#endif

  Unify_constant(MakeString(relBuf), X(1));
  checkdealloc((tagged_t *)relBuf, relBufSize);
  return TRUE;
}

extern char *emulator_architecture;

/*
 *  get_arch(?ArchName).
 */
bool_t prolog_getarch(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(emulator_architecture), X(0));
}


extern char *emulator_os;

/*
 *  get_os(?OsName).
 */
bool_t prolog_getos(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(emulator_os), X(0));
}

extern char *emulator_debug;

/*
 *  getdebug(?Debug).
 */
bool_t prolog_getdebug(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(emulator_debug), X(0));
}

extern char *emulator_location;

bool_t prolog_get_eng_location(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(emulator_location), X(0));
}

extern char *ciao_version;
extern char *ciao_patch;
extern char *ciao_commit_branch;
extern char *ciao_commit_id;
extern char *ciao_commit_date;
extern char *ciao_commit_desc;

/*
 *  $ciao_version(?Version, ?Patch, ?CommitBranch, ?CommitId, ?CommitDate, ?CommitDesc) 
 *    for current_prolog_flag(version, ?V).
 */
bool_t prolog_version(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));
  DEREF(X(2), X(2));
  DEREF(X(3), X(3));
  DEREF(X(4), X(4));
  DEREF(X(5), X(5));
  return (cunify(Arg, MakeString(ciao_version),  X(0)) &&
	  cunify(Arg, MakeString(ciao_patch),    X(1)) &&
	  cunify(Arg, MakeString(ciao_commit_branch), X(2)) &&
	  cunify(Arg, MakeString(ciao_commit_id), X(3)) &&
	  cunify(Arg, MakeString(ciao_commit_date), X(4)) &&
	  cunify(Arg, MakeString(ciao_commit_desc), X(5)));
}

extern char *ciao_suffix;
extern char *exec_suffix;
extern char *so_suffix;

bool_t prolog_get_ciao_ext(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(ciao_suffix), X(0));
}

bool_t prolog_get_exec_ext(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(exec_suffix), X(0));
}

bool_t prolog_get_so_ext(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(so_suffix), X(0));
}

bool_t prolog_wait(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:wait", 3);
  int retcode, status;

  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(0), 1);
  }

  retcode = waitpid(GetSmall(X(0)), &status, 0);

  return (cunify(Arg, X(1), MakeSmall(retcode)) &&
	  cunify(Arg, X(2), MakeSmall(status)));
}

/*
 *exec(+Command, +Args, -StdIn, -StdOut, -StdErr, +Background, -PID, -Errcode):
 * connect to an external process
 */

#define Read  0
#define Write 1

#define STDIN  0
#define STDOUT 1
#define STDERR 2


bool_t check_pipe(Arg, argno, stream_int)
     Argdecl;
     int argno;
     int stream_int[2];
{
  DEREF(X(argno), X(argno));
  if (X(argno) == atom_nil)
    return FALSE;
  else {
    pipe(stream_int);
    return TRUE;
  }
}

#define MAXARGS 100

bool_t prolog_exec(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:exec", 3);
  tagged_t head, list;
  char *command;
  bool_t wait_for_completion;

  bool_t
    unif_stdin,
    unif_stdout,
    unif_stderr;
  bool_t
    dup_stdin,
    dup_stdout,
    dup_stderr;
  int
    pipe_in[2],                                   /* Child standard input */
    pipe_out[2],                                 /* Child standard output */
    pipe_err[2];                                  /* Child standard error */
  stream_node_t
    *str_in = NULL,                /* Connection to child standard input  */
    *str_out = NULL,              /* Connection to child standard output  */
    *str_err = NULL;               /* Connection to child standard error  */

  int pid;
  int status = 0;
  int nargs  = 0;
  char **arguments;

  DEREF(X(0), X(0));
  if (!IsAtom(X(0))) {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), X(0), 1);
  }

  dup_stdin  = check_pipe(Arg, 2, pipe_in);
  dup_stdout = check_pipe(Arg, 3, pipe_out);
  dup_stderr = check_pipe(Arg, 4, pipe_err);

  command = GetString(X(0));

  arguments = (char **)checkalloc(MAXARGS*sizeof(char *));

  arguments[nargs++] = command;
  DEREF(list, X(1));      /* Arguments. Traverse list and fill vector in. */
  while(!IsVar(list) && TagIsLST(list)) {
    DEREF(head, CTagToCar(list));
    if (!TagIsATM(head)) {  /* We only allow atoms */
      checkdealloc((tagged_t *)arguments, MAXARGS*sizeof(char *));
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), head, 2);
    }
    arguments[nargs++] = GetString(head);
    list = CTagToCdr(list);
    DEREF(list, list);
  }
  arguments[nargs] = NULL;

 /* Make sure we had a real list */
  if (!(!IsVar(list) && TagIsATM(list) && (list == atom_nil))) {
    checkdealloc((tagged_t *)arguments, MAXARGS*sizeof(char *));
    BUILTIN_ERROR(TYPE_ERROR(LIST), X(1), 2);
  }


 /* Backgrounding or not. */

  DEREF(X(5), X(5));
  if ((X(5) == atom_true) ||
      (X(5) == atom_wait))
    wait_for_completion = FALSE;
  else if (X(5) == atom_false)
    wait_for_completion = TRUE;
  else {
    checkdealloc((tagged_t *)arguments, MAXARGS*sizeof(char *));
    ERROR_IN_ARG(X(5), 6, STRICT_ATOM);
  }
  /* Empty buffers before launching child */
  fflush(NULL);

  pid = vfork();

  if (pid == -1) {
    checkdealloc((tagged_t *)arguments, MAXARGS*sizeof(char *));
    SERIOUS_FAULT("exec/4 could not vfork() new process");
  } else
    if (pid == 0) {
      bool_t execstat;
      /* This is the code of the child */
      if (dup_stdin) {
	close(pipe_in[Write]);
	dup2(pipe_in[Read], STDIN);
      }
      if (dup_stdout) {
	close(pipe_out[Read]);
	dup2(pipe_out[Write], STDOUT);
      }
      if (dup_stderr) {
	close(pipe_err[Read]);
	dup2(pipe_err[Write], STDERR);
      }

 // If there is any error while exec'ing, we have to give an error and then
 // abort completely. Unfortunately, if we have redirected the standard
 // error output, the error message will be lost!
      execstat = execvp(command, arguments);
      checkdealloc((tagged_t *)arguments, MAXARGS*sizeof(char *));
      if (execstat < 0) {
        _EXIT("Execution of command failed!");   // Should never return!
      }
      return FALSE;
    } else {                                                    /* Parent */
      if (dup_stdin) {
        close(pipe_in[Read]);
        str_in  = new_stream(X(0), "w", fdopen(pipe_in[Write], "w"));
      }
      if (dup_stdout) {
        close(pipe_out[Write]);
        str_out = new_stream(X(0), "r", fdopen(pipe_out[Read], "r"));
      }
      if (dup_stderr) {
        close(pipe_err[Write]);
        str_err = new_stream(X(0), "r", fdopen(pipe_err[Read], "r"));
      }

      if (wait_for_completion)
        waitpid(pid, &status, 0);

      unif_stdin  = !dup_stdin ||cunify(Arg, ptr_to_stream(Arg, str_in),  X(2));
      unif_stdout = !dup_stdout||cunify(Arg, ptr_to_stream(Arg, str_out), X(3));
      unif_stderr = !dup_stderr||cunify(Arg, ptr_to_stream(Arg, str_err), X(4));

      return (
	      unif_stdin && unif_stdout && unif_stderr &&
	      cunify(Arg, MakeSmall(pid), X(6)) &&
	      cunify(Arg, MakeSmall(status), X(7))
	      );
    }
}


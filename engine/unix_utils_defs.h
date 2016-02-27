
BOOL expand_file_name(char *name, char *target);
void compute_cwd(void);
BOOL prolog_unix_cd(Argdecl);
BOOL prolog_unix_shell0(Argdecl);
BOOL prolog_unix_shell2(Argdecl);
BOOL prolog_unix_system2(Argdecl);
BOOL prolog_unix_argv(Argdecl);
BOOL prolog_unix_exit(Argdecl);
BOOL prolog_unix_mktemp(Argdecl);
BOOL prolog_unix_access(Argdecl);
BOOL prolog_directory_files(Argdecl);
BOOL prolog_file_properties(Argdecl);
BOOL prolog_unix_chmod(Argdecl);
BOOL prolog_unix_umask(Argdecl);
BOOL prolog_unix_delete(Argdecl);
BOOL prolog_unix_rename(Argdecl);
BOOL prolog_unix_mkdir(Argdecl);
BOOL prolog_unix_rmdir(Argdecl);
BOOL prolog_current_host(Argdecl);
BOOL prolog_getenvstr(Argdecl);
BOOL prolog_setenvstr(Argdecl);
BOOL prolog_pause(Argdecl);
BOOL prolog_getpid(Argdecl);
BOOL prolog_find_file(Argdecl);
BOOL prolog_getarch(Argdecl);
BOOL prolog_getos(Argdecl);
BOOL prolog_version(Argdecl);


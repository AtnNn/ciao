#if !defined(TIMING_DEFS)
#define TIMING_DEFS

/* This is defined here to let it can be changed to another functions */

# define TICK_FUNCTION usertick
# define GET_CLOCKFREQ(X) (X.userclockfreq)
# define BASE_RUNTICK (TICK_FUNCTION())

ENG_FLT usertime(void);
ENG_FLT walltime(void);
void init_statistics(void);
void reset_statistics(void);

extern ENG_LINT (*usertick)(void);
extern ENG_LINT (*systemtick)(void);
extern ENG_LINT (*walltick)(void);

bool_t prolog_runtime(Argdecl);
bool_t prolog_usertime(Argdecl);
bool_t prolog_systemtime(Argdecl);
bool_t prolog_walltime(Argdecl);

bool_t prolog_time(Argdecl);
bool_t prolog_datime(Argdecl);

bool_t prolog_walltick(Argdecl);
bool_t prolog_usertick(Argdecl);
bool_t prolog_systemtick(Argdecl);
bool_t prolog_runtick(Argdecl);

bool_t prolog_wallclockfreq(Argdecl);
bool_t prolog_userclockfreq(Argdecl);
bool_t prolog_systemclockfreq(Argdecl);
bool_t prolog_runclockfreq(Argdecl);

#endif /* TIMING_DEFS */

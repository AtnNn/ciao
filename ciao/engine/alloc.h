/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */




#define kB       1024
#define kCells   1024


#define STATICMAXATOM 1024     /* Avoid very long atoms inside the engine */

#define USE_DYNAMIC_ATOM_SIZE                               /* By default */

#if defined(USE_DYNAMIC_ATOM_SIZE) 
# define MAXATOM  Atom_Buffer_Length
#else
# define MAXATOM  STATICMAXATOM
#endif


#define CONTPAD 128		/* min. amount of heap at proceed */
#define CALLPAD (2*(MAXATOM) + CONTPAD) /* min. amount of heap at call */

#define STACKPAD (2*ARITYLIMIT + 16) /* min. amount of stack at allocate */

				/* min. amount of trail/choice at try */
#define CHOICEPAD (2*ARITYLIMIT)


#define ATMTABSIZE  (4*kCells)	/* size of global atom table  */
#define QLOADSIZE   (2*kCells)	/* plenty at present */

#if defined(TABLING)

# define GLOBALSTKSIZE  (4800*kCells-1) /* Was 6*kCells-1 (DCG) */
# define LOCALSTKSIZE   (3000*kCells-1)
# define CHOICESTKSIZE  (3000*kCells-1)
# define TRAILSTKSIZE   (3000*kCells-1)

#elif (defined(ANDPARALLEL) || defined(PARBACK))

# define GLOBALSTKSIZE   (600*kCells-1) /* Was 6*kCells-1 (DCG) */
# define LOCALSTKSIZE    (300*kCells-1)
# define CHOICESTKSIZE   (300*kCells-1)
# define TRAILSTKSIZE    (300*kCells-1)

#else

# define GLOBALSTKSIZE   (16*kCells-1) /* Was 6*kCells-1 (DCG) */
# define LOCALSTKSIZE    (4*kCells-1)
# define CHOICESTKSIZE   (4*kCells-1)
# define TRAILSTKSIZE    (4*kCells-1)

#endif

#define XREGBANKSIZE    ARITYLIMIT

/* The ...STKSIZE constants may be overridden by env. variables.
   This macro first looks for one, and if not found, uses the default. */
#define GETENV(VALUE,WORK,STRING,VAR) \
  if ((WORK = getenv(STRING))) \
    VALUE = atoi(WORK); \
  else \
    VALUE = VAR;


#if defined(DEBUG)

#define INC_MEM_PROG(size) \
    if (debug_mem) \
      fprintf(stderr, "Program memory increased by %ld bytes\n", \
             (long int)size);\
    mem_prog_count = mem_prog_count + (size)

#define DEC_MEM_PROG(size) \
    if (debug_mem) fprintf(stderr, "Program memory decreased by %ld bytes\n", \
             (long int)size);\
    mem_prog_count = mem_prog_count - (size)

#else

#define INC_MEM_PROG(size) mem_prog_count = mem_prog_count + (size);

#define DEC_MEM_PROG(size) mem_prog_count = mem_prog_count - (size);

#endif

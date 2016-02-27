/* Copyright (C) 1996, 1997, 1998, 1999, 2000 CLIP */

int buffered_input(FILE *stream);
BOOL pop_qlinfo(Argdecl);
BOOL prolog_qread(Argdecl);
BOOL push_qlinfo(Argdecl);
BOOL qread1(Argdecl, FILE *qfile, TAGGED *rungoal);
int is_a_script(FILE *file);
void expand_qload(void);
void reloc_counter(ENG_INT Label);
void reloc_emul_entry(int Li, ENG_INT Label);
void reloc_pointer(int Li, ENG_INT Label);
void skip_to_ctrl_l(FILE *file);

/* We want to use buffered reading of .po files --- it is much faster! */

#define BUFFERED_PO

#if defined(BUFFERED_PO)
# define GETC(f) buffered_input(f)
# if defined(DEBUG)
#  define UNGETC(chr, f) \
   if (!qlbuffidx)  fprintf(stderr, "Error UNGETting: buffer underfull!\n"); \
   else qlbuffidx--
# else
#  define UNGETC(chr, f) qlbuffidx-- 
# endif
#else
# define GETC(f) getc(f)
# define UNGETC(chr, f) ungetc(chr, f)
#endif


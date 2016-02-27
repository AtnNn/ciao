
ENG_FLT getdouble(FILE *f);
ENG_INT getlong(FILE *f);
tagged_t getlarge(Argdecl, FILE *f);
char *getstring(Argdecl, FILE *f);
int getshort(FILE *f);
void getbytecode(Argdecl, FILE *f, bcp_t insn_p, int length);

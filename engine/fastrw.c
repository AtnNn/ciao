#define FASTRW_MAX_VARS 1024

BOOL prolog_fast_read(Arg)
     Argdecl;
{
  struct stream_node *s;
  TAGGED vars[FASTRW_MAX_VARS];
  fast_read(Arg, Input_Stream_Ptr);
  return TRUE;
}

void fast_read(Arg, s)
     Argdecl;
     
{
  FILE *f = s->streamfile;

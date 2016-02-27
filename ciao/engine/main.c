/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* This is the main file for an executable.  It just calls the entry point,
   which can as well be called by any other executable to load and start a
   prolog engine. */

char emulatorlength_holder[] = "This emulator executable has a size of        ";

extern int start(int argc, char **argv);
extern void init_emulatorlength(char *);
/* extern char *emulatorlength; */

int main(int argc, char *argv[])
{
/*   emulatorlength = emulatorlength_holder; */
  init_emulatorlength(emulatorlength_holder);
  return start(argc, argv);
}

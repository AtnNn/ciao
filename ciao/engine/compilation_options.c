#include <stdio.h>

int main(int argc, char *argv[]) {

  int i;

  printf(":- module(_, _).\n\n");

  printf("c_compiler('%s').\n", argv[1]);
  printf("c_compiler_options([");
  for (i = 2; i < argc-2; i++) 
      printf("'%s', ", argv[i]);
  printf("'%s']).\n", argv[argc-2]);

  return 0;
}

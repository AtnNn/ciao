/* Utility for fixing the size constant of the CIAO engine */
/* UPM CLIP laboratory - by Oscar Portela Arjona */

#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>

int main(argc,argv)
      int argc;
      char *argv[];
{
  int j,i = 0;
  struct stat data;
  char size[8];
  char string[] = "This emulator executable has a size of N"; 
  FILE *fd;

  if (stat(argv[1],&data) == -1)
    printf("Usage: %s <file>\n",argv[0]);
  else if (data.st_size > 10000000)
    printf("Error: Size exceeding 7 digits\n");
  else if ((fd = fopen(argv[1],"r+b")) == NULL)
    printf("Error: Unable to open %s for reading and writing",argv[0]);
  else {
    sprintf(size,"%7i",data.st_size);
    for (j = getc(fd); j != EOF; j = getc(fd)) {
      if (j == string[i]) i++;
      else i = 0;
      if (string[i] == 'N') {
	fseek(fd,0,SEEK_CUR); /* Required for correct execution on SunOS */
	fwrite(size,1,7,fd);
	fflush(fd);           /* Required for correct execution on SunOS */
      } 
    }
    fclose(fd);
    exit(0);
  }
  exit(-1);
}

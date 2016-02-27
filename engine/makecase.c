/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include <stdio.h>

main()
{
  int c;
  char name[128], hexcode[128], format[128];
  
  printf("/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP. */\n\n");
  printf("/* DO NOT EDIT THIS FILE */\n");
  printf("/* it has been automatically generated by 'makecase'*/\n");
  for(;;)
    {
      c = getchar();
      switch(c)
	{
	case EOF:
	  exit(0);
	case '#':
	  switch(scanf("define %s %[xX0123456789ABCDEFabcdef] Format%s\n",name,hexcode,format))
	    {
	    case 2: /* no format */
	      printf("case %s: name = \"%s\"; format = ",hexcode,name);
	      printf("\"\"");
	      printf("; break;\n");
	      break;
	    case 3: /* with format */
	      printf("case %s: name = \"%s\"; format = %s; break;\n",hexcode,name,format+1);
	      /*print_within_parenthesis(format);*/
	      break;
	    }
	default:
	  break;
	}
    }
}

print_within_parenthesis(s)
     char *s;
{
  while(*s && *s++!='(')
    ;
  while(*s && *s!=')')
    putchar(*s++);
}

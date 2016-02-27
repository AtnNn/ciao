Java-to-Prolog interface Examples:

SIMPLE EXAMPLES
--------------
* example0.java

This example just starts the Prolog server to get all solutions of
a simple goal (append(X,Y,[a,b])), and prints them to standard
output. 

To start this example, type:

$ java -cp ../../:./ example1 ../../plserver

or, if you are using Windows:

  java -cp ..\..\;.\ example1 <home>\Win32\bin\ciaoengine -C -b ..\..\plserver.cpx 

where java must be on version 1.2 or higher, and <home> represents the
Ciao home directory.

Important: be sure plserver.pl is compiled using the following
command:

  ciaoc plserver.pl



* example1.java

This example is like example0, but instead of printing to standard
output, opens a simple Java window and prints there two solutions of
the same goal.

The example can be run using the same commands than in previous
example. 

N-QUEENS PROBLEM
----------------
queens.java

This example shows on the screen a chess board with the first solution of
the 8-queens problem, and allows the user change the size of the board, and
request the rest of solutions of the n-queens problem. 

To start this example, just type the following:

$ java -cp ../../:./ queens ../../plserver

or, if you are using Windows:

  java -cp ..\..\;.\ queens <home>\Win32\bin\ciaoengine -C -b ..\..\plserver.cpx

where java must be on version 1.2 or higher, and <home> represents the
Ciao home directory.

Important: be sure plserver.pl is compiled using the following
command:

  ciaoc plserver.pl




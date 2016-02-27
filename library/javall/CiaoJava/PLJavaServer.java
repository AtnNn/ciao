package CiaoJava;

import java.lang.*;
import java.io.*;
import java.applet.*;
import java.awt.*;

/**
 * This class implements the starting point of the java
 * object server. Starts the communication and
 * performs the basic server loop.
 *
 */
public class PLJavaServer {
  private static final String JAVA_QUIT = "$java_quit";

  public static void main(String argv[]) {

    PLConnection pl = null;
    try {
      pl = new PLConnection();
    } catch (Exception e) {
      System.err.println("Problems starting java server: " + e);
      System.exit(1);
    }
    PLInterpreter i = new PLInterpreter(pl);
    PLTerm command;

    try {
      while ((command = pl.fromProlog()) != new PLAtom(JAVA_QUIT))
	pl.toProlog(i.interpret(command));
    } catch (Exception e) {
      System.err.println("Error: " + e);
      System.exit(1);
    }

    System.exit(0);
  }
}








package CiaoJava;

import java.lang.*;
import java.io.*;
import java.applet.*;
import java.awt.*;

/**
 * This class implements the starting point of the Java
 * object server. Starts the communication and
 * performs the basic server loop. Includes the
 * <code>main</code> method to be executed at
 * server start up.
 *
 */
public class PLJavaServer {

    /**
     * Start up method. Starts the sockets and 
     * prepares the server to receive Prolog requests.
     * If there is any error creating sockets terminates
     * the Java process.
     */
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
      while ((command = pl.fromProlog()) != PLAtom.quit)
	pl.toProlog(i.interpret(command));
    } catch (Exception e) {
      System.err.println("Error: " + e);
      System.exit(1);
    }

    System.exit(0);
  }
}








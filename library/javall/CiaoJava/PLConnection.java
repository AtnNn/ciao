package CiaoJava;

import java.io.*;
import java.net.*;
import java.util.*;

/**
 * Starts and handles a connection to a prolog process
 * via sockets.
 * The PLConnection can be used in two ways, so the
 * <tt>CiaoJava</tt> interface can work as a java
 * object server (using the constructor with no arguments),
 * or as a connection to a prolog query server.
 * Working with a prolog server using the java side as a
 * client, the prolog goals can be launched using the <tt>launchGoal</tt>
 * method with a <tt>PLTerm</tt> object representing a goal
 * (those terms where the isRunnable() method returns true),
 * or creating and using <tt>PLGoal</tt> objects.
 */
public class PLConnection {

  /**
   * Private fields.
   */
  private Process plProc = null;
  private boolean allowThreads = false;

  private BufferedReader plIn;
  private PrintWriter plOut;
  private BufferedReader evIn;
  private PrintWriter evOut;

  private static final PLTerm DATA_SYNC = new PLAtom("data");
  private static final PLTerm EVENT_SYNC = new PLAtom("event");

  /**
   * Creates a PLConnection to use the java-to-prolog
   * interface. Starts the prolog server process and
   * connects to it.
   *
   * @param where command used to start the prolog server process.
   *
   * @exception IOException if there are I/O problems.
   * @exception PLException if there are problems regarding the prolog
   *                        process.
   */
  public PLConnection(String where) throws IOException, PLException {
    Runtime rt = Runtime.getRuntime();

    plProc = rt.exec(where);
    OutputStream pipeOut = plProc.getOutputStream();
    PrintStream out = new PrintStream(pipeOut);
    createSockets(out);
  }

  /**
   * Creates a PLConnection for the prolog-to-java
   * interface: waits for a prolog process that wants to
   * connect to it.
   */
  public PLConnection() throws IOException, PLException {

    createSockets(System.out);

  }

  /**
   * Asks the prolog server if it can work with threads.
   *
   * @return true if the prolog server can work with threads;
   *         false otherwise.
   *
   */
  public boolean allowThreads() {
    
    return this.allowThreads;

  }

  /**
   * This private method creates and synchronizes the sockets 
   * for communication with the prolog process.
   */
  private void createSockets(PrintStream out) 
    throws IOException, PLException {
    
    // Starting socket server.
    PLServerSocket ss = new PLServerSocket();

    // port number output.
    int port = ss.getPort();
    out.println(port + ".");
    out.flush();

    // Open data socket.
    Socket dataSocket = ss.openSocket();
    plIn = ss.getReader(dataSocket);
    plOut = ss.getWriter(dataSocket);

    // Synchronizing data socket.
    PLTerm dataSync = fromProlog();
    //if (!dataSync.equals(DATA_SYNC)) throw...
    toProlog(DATA_SYNC);

    // Starting event socket.
    Socket eventSocket = ss.openSocket();
    evIn = ss.getReader(eventSocket);
    evOut =  ss.getWriter(eventSocket);

    // Synchronizing event socket.
    PLTerm eventSync = fromProlog(evIn);
    //if (!eventSync.equals(EVENT_SYNC)) throw...
    toProlog(evOut, EVENT_SYNC);

  }

  /**
   * Goal launching. Evaluates the term received as a query and
   * sends it to prolog for evaluation.
   *
   * @param	goal	prolog term that will be evaluated as a prolog
   *              goal.
   */
  public PLGoal query(PLTerm term) throws PLException, IOException {

    PLGoal goal = new PLGoal(this,term);
    goal.query();
    return goal;

  }

  /**
   * Low level Java-to-prolog communication. This method sends 
   * prolog terms from java to prolog.
   *
   * @param term is an object representing a prolog term.
   */
  protected void toProlog(PLTerm term) {

    toProlog(plOut, term);

  }

  /**
   * Low level java-to-prolog event communication. This method sends
   * prolog terms to prolog through the event socket.
   *
   * @param term is an object representing a prolog term.
   */
  protected void toPrologEvent(PLTerm term) {

    toProlog(evOut, term);

  }

  /**
   * Low level Java-to-prolog communication. Private method that sends
   * prolog terms to prolog through a given socket.
   *
   * @param out   is the socket output stream to send the
   *              term through.
   * @param term  is an object representing a prolog term.
   */
  private void toProlog(PrintWriter out, PLTerm term) {

    out.print(term.fastWrite());
    out.flush();

  }

  /**
   * Prolog-to-java communication. This method listens at the prolog
   * socket to receive results from the prolog process as terms.
   *
   * @return Prolog term received from the socket.
   *
   * @exception IOException if the socket stream has been broken.
   */
  protected PLTerm fromProlog() throws IOException, PLException {

    return fromProlog(plIn);

  }

  /**
   * Low level Prolog-to-java communication. Private method that listens
   * at a given prolog socket to receive results from the prolog
   * process as terms.
   *
   * @param in socket input stream to receive the prolog data.
   *
   * @return Prolog term received from the socket.
   *
   * @exception IOException if the socket stream has been broken.
   */
  private PLTerm fromProlog(BufferedReader in) 
    throws IOException, PLException {
    
    return PLTerm.fastRead(in);

  }

}



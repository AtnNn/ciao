package CiaoJava;

import java.io.*;
import java.net.*;
import java.util.*;

/**
 * Starts and handles a connection to a Prolog process
 * via sockets.
 * The PLConnection can be used in two ways, so the
 * <code>CiaoJava</code> interface can work as a Java
 * object server (using the constructor with no arguments),
 * or as a connection to a Prolog query server.
 * Working with a Prolog server using the Java side as a
 * client, the Prolog goals can be launched using the <code>launchGoal</code>
 * method with a <code>PLTerm</code> object representing a goal
 * (those terms where the isRunnable() method returns true),
 * or creating and using <code>PLGoal</code> objects.
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
   * Creates a PLConnection to use the Java-to-Prolog
   * interface. Starts the Prolog server process and
   * connects to it creating the sockets.
   *
   * @param where command used to start the Prolog server process.
   *
   * @exception IOException if there are I/O problems.
   * @exception PLException if there are problems regarding the Prolog
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
   * Creates a PLConnection for the Prolog-to-Java
   * interface: waits for a Prolog process that wants to
   * connect to it.
   *
   * @exception IOException if there are I/O problems.
   * @exception PLException if there are problems regarding the Prolog
   *                        process.
   */
  public PLConnection() throws IOException, PLException {

    createSockets(System.out);

  }

  /**
   * Asks the Prolog server if it can work with threads.
   *
   * @return true if the Prolog server can work with threads;
   *         false otherwise.
   *
   */
  public boolean allowThreads() {
    
    return this.allowThreads;

  }

  /**
   * This private method creates and synchronizes the sockets 
   * for communication with the Prolog process.
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
   * sends it to Prolog for evaluation.
   *
   * @param	goal	Prolog term that will be evaluated as a Prolog
   *              goal.
   *
   * @return The <code>PLGoal</code> created to manage the goal.
   *
   * @exception IOException if there are I/O problems.
   * @exception PLException if there are problems regarding the Prolog
   *                        process.
   */
  public PLGoal query(PLTerm term) throws PLException, IOException {

    PLGoal goal = new PLGoal(this,term);
    goal.query();
    return goal;

  }

  /**
   * Low level Java-to-Prolog communication. This method sends 
   * Prolog terms from Java to Prolog. Transforms the term in
   * a serialized form (using the Prolog format) and sends the
   * result to Prolog through the data socket.
   *
   * @param term is an object representing a Prolog term.
   *
   */
  protected void toProlog(PLTerm term) {

    toProlog(plOut, term);

  }

  /**
   * Low level Java-to-Prolog event communication. This method sends
   * Prolog terms to Prolog through the event socket.
   *
   * @param term is an object representing a Prolog term.
   */
  protected void toPrologEvent(PLTerm term) {

    toProlog(evOut, term);

  }

  /**
   * Low level Java-to-Prolog communication. Private method that sends
   * Prolog terms to Prolog through a given socket.
   *
   * @param out   is the socket output stream to send the
   *              term through.
   * @param term  is an object representing a Prolog term.
   */
  private void toProlog(PrintWriter out, PLTerm term) {

    out.print(term.fastWrite());
    out.flush();

  }

  /**
   * Prolog-to-Java communication. This method listens at the Prolog
   * socket to receive results from the Prolog process as terms.
   *
   * @return Prolog term received from the socket.
   *
   * @exception IOException if the socket stream has been broken.
   * @exception PLException if there are problems regarding the Prolog
   *                        process.
   */
  protected PLTerm fromProlog() throws IOException, PLException {

    return fromProlog(plIn);

  }

  /**
   * Low level Prolog-to-Java communication. Private method that listens
   * at a given Prolog socket to receive results from the Prolog
   * process as terms.
   *
   * @param in socket input stream to receive the Prolog data.
   *
   * @return Prolog term received from the socket.
   *
   * @exception IOException if the socket stream has been broken.
   * @exception PLException if there are problems regarding the Prolog
   *                        process.
   */
  private PLTerm fromProlog(BufferedReader in) 
    throws IOException, PLException {
    
    return PLTerm.fastRead(in);

  }

}



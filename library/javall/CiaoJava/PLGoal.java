package CiaoJava;

import java.util.*;
import java.io.*;

/**
 * Class PLGoal
 * Represents a prolog goal to be evaluated on a given <tt>PLConnection</tt>.
 * Instances of this class should be generated with the <tt>launchGoal</tt>
 * method of the <tt>PLConnection</tt> class.
 */
public class PLGoal {
  protected static final String SOLUTION = "prolog_solution";

  private PLTerm originalGoal = null; /* Keeps a copy of the original
				       * goal term to rebind the
				       * variables included on every
				       * prolog solution.
				       */
  private PLTerm actualGoal = null;   /* Stores the actual binding
				       * of the variables contained
				       * in the goal. Is used to
				       * unbind them with the original
				       * binding (see originalGoal).
				       */
  private PLConnection prologSpace = null;
  private int queryId = NOT_LAUNCHED;

  private static final String LAUNCH_GOAL = "prolog_launch_query";
  private static final String NEXT_SOLUTION = "prolog_next_solution";
  private static final String TERMINATE_QUERY = "prolog_terminate_query";
  private static final String FAIL = "prolog_fail";
  private static final String EXCEPTION = "prolog_exception";
  private static final String LAUNCH_GOAL_ON_THREAD = 
    "prolog_launch_query_on_thread";
  private static final String NEXT_SOLUTION_ON_THREAD =
    "prolog_next_solution_on_thread";
  private static final String TERMINATE_QUERY_ON_THREAD =
    "prolog_terminate_query_on_thread";
  private static final String USE_MODULE = "prolog_use_module";

  // Query status (for queryId field).
  private static final int NOT_LAUNCHED = 0;
  private static final int TERMINATED = -1;
  private static final int FINISHED = -1;

  /**
   * Goal constructor. Creates a new goal on the <code>where</code> prolog
   * process, using the prolog term represented with <code>term</code>.
   *
   * @param		where	Prolog process on which the goal must
   *          			be evaluated.
   * @param		term	Prolog term that represents the goal that
   *          			will be evaluated.
   */
  public PLGoal(PLConnection where, PLTerm term) {

    prologSpace = where;
    this.actualGoal = term;
    this.originalGoal = term.copy();

  }

  /**
   * Goal constructor. Creates a new goal on the <code>where</code> prolog
   * process, using the prolog term represented with <code>termString</term>
   * string. This string must be a well formed prolog term; otherwise a
   * <code>PLException</code> will be thrown.
   *
   * @param		where	Prolog process on which the goal must
   *          			be evaluated.
   * @param		term	String containing the representation of a
   *                            well formed prolog term that represents
   *                            the goal that will be evaluated.
   */
  public PLGoal(PLConnection where, String term) 
    throws IOException, PLException {

    prologSpace = where;

    this.actualGoal = parseTerm(term);
    this.originalGoal = this.actualGoal.copy();

  }

  /**
   * Goal query.
   * Evaluates on the PLConnection associated object the
   * goal represented by this object. To obtain the solutions of this
   * goal, the {@link nextSolution()} method must be called, once for
   * each solution.
   *
   * @exception IOException, PLException if there is any problem 
   *            communicating with the prolog process.
   */
  public void query() throws IOException, PLException {

    if (queryId != NOT_LAUNCHED)
      throw new PLException("This query has been already launched.");

    if (actualGoal.isRunnable()) {
      PLTerm arg[] = {actualGoal};
      PLTerm command = null;
      if (prologSpace.allowThreads())
	  command = new PLStructure(LAUNCH_GOAL_ON_THREAD, 1, arg);
      else
	  command = new PLStructure(LAUNCH_GOAL, 1, arg);
      prologSpace.toProlog(command);
      PLTerm result = prologSpace.fromProlog();

      if (result.isSolution() && ((PLStructure)result).getArg(1).isException())
	throw PLException.translateException(((PLStructure)result).getArg(1));
      
      if (result.isException())
	throw PLException.translateException(result);

      if (result.isPrologFail())
	throw new PLException("Fail returned creating query");

      if (result.isQueryId()) {
	PLTerm id;
	id = ((PLStructure)result).getArg(0);
	queryId = ((PLInteger)id).getValue();
      }
      else
	throw new PLException("No Id received at query creation");
    }
    else
      throw new PLException("Invalid goal: " + actualGoal);
  }

  /**
   * Sends to prolog process a request for the next query solution.
   * Returns a prolog term that corresponds to the goal with the
   * prolog variables unified with the solution. Later use of this
   * prolog variable objects will refer the unification performed.
   * If there is no more solutions, all the variables of the goal
   * will be set to their original binding before calling this method.
   *
   * @return  the term that corresponds to the query, with the
   *          variables unified with the solution.
   * @return  <code>null</code> if there are no more solutions.
   *
   * @exception IOException if there are any error on the sockets.
   * @exception PLException if there are any error on the prolog
   *            process.
   */
  public PLTerm nextSolution() throws IOException, PLException {

    PLTerm result = null;
    if (queryId == NOT_LAUNCHED)
      throw new PLException("Query not launched");

    if (queryId == TERMINATED)
      throw new PLException("Query has been already terminated.");

    if (queryId == FINISHED)
      throw new PLException("Query is already finished.");

    PLTerm arg[] = {new PLInteger(queryId)};
    if (prologSpace.allowThreads())
	prologSpace.toProlog(new PLStructure(NEXT_SOLUTION_ON_THREAD, arg));
    else
	prologSpace.toProlog(new PLStructure(NEXT_SOLUTION, arg));
    result = prologSpace.fromProlog();

    if (result.isPrologFail()) {
      queryId = FINISHED;
      result = null;
    }
    else if (result.isSolution() && ((PLStructure)result).getArg(1).isException())
      throw PLException.translateException(((PLStructure)result).getArg(1));
    else if (result.isException())
      throw PLException.translateException(result);
    else {
      // result returns the second argument of the
      // term received. The first argument is the
      // query id.
      result = ((PLStructure)result).getArg(1);
      actualGoal.backtrack(originalGoal);
      actualGoal.unify(result);
    }

    return result;

  }

  /**
   * Terminates this prolog goal execution.
   *
   * @exception IOException if there are any error on the socket 
   *            communication
   * @exception PLException if there are any error on the prolog
   *            process
   *
   */
  public void terminate() throws IOException, PLException {
    
    if (queryId == NOT_LAUNCHED)
      throw new PLException("Query not launched");

    if (queryId == TERMINATED)
      throw new PLException("Query has been already terminated.");

    if (queryId == FINISHED)
      throw new PLException("Query is already finished.");

    terminate_();

  }

  /**
   * This method loads a module in the prolog process.
   *
   * @param module Prolog term that represents the name of the module
   *               to be loaded. Can be used the library(module) format.
   */
  public void useModule(PLTerm module) throws IOException, PLException {
    
      PLTerm arg[] = {module};
      PLTerm command = new PLStructure(USE_MODULE, 1, arg);
      prologSpace.toProlog(command);
      PLTerm result = prologSpace.fromProlog();

      if (result.isSolution() && ((PLStructure)result).getArg(1).isException())
	throw PLException.translateException(((PLStructure)result).getArg(1));

      if (result.isException())
	throw PLException.translateException(result);

      if (result.isPrologFail())
	throw new PLException("Fail returned using a prolog module");

      if (!result.isPrologSuccess())
	throw new PLException("No success returned using a prolog module");
      
  }

  /**
   * 
   * This method loads a module in the prolog process.
   *
   * @param module String that contains a prolog term with the name of
   *               the module to be loaded. Can be used the
   *               library(module) format.
   */
  public void useModule(String module) throws IOException, PLException {

      useModule(parseTerm(module));

  }

  /**
   * This private method implements the common tasks related to
   * goal termination.
   */
  private void terminate_() throws IOException, PLException {

    PLTerm result;

    PLTerm arg[] = {new PLInteger(queryId)};
    if (prologSpace.allowThreads())
	prologSpace.toProlog(new PLStructure(TERMINATE_QUERY_ON_THREAD, arg));
    else
	prologSpace.toProlog(new PLStructure(TERMINATE_QUERY, arg));
    result = prologSpace.fromProlog();
    queryId = TERMINATED;

    if (result.isSolution() && ((PLStructure)result).getArg(1).isException())
      throw PLException.translateException(result);
    else if (!result.isPrologSuccess())
      throw new PLException("Termination has not been accepted by the server");

  }

  /**
   * Destructor. Terminates the prolog goal and finalizes itself.
   */
  protected void finalize() throws IOException, PLException, Throwable {
    
    if ((queryId != NOT_LAUNCHED) &&
	(queryId != TERMINATED) &&
	(queryId != FINISHED)) {

      terminate_();

    }
    super.finalize();

  }

  /**
   * This method uses the prolog process to parse a prolog term received
   * as a string.
   *
   *  @param     termString <code>String</code> object that represents a well
   *                        formed prolog term.
   *  @return    the <code>PLTerm</code> that represents this prolog term.
   *  @exception <code>IOException</code> if the socket stream has been
   *             broken.
   *  @exception <code>PLException</code> if there is a problem parsing
   *             the term on the prolog side.
   **/
  private PLTerm parseTerm(String termString) 
    throws IOException, PLException {

    PLVariable v = new PLVariable();
    PLStructure p = new PLStructure("prolog_parse", 
				    new PLTerm[] {new PLString(termString),v});

    PLGoal g = new PLGoal(prologSpace, p);
    g.query();
    PLTerm r =  g.nextSolution();
    if (r == null)
      throw new PLException("null returned from prolog socket");

    g.terminate();
    return v.getBinding();

  }

  /**
   * String representation of a prolog goal.
   */
  public String toString() {
    
    return "goal{" + this.originalGoal.toString() + ","
      + queryId + "}";

  }
}



package CiaoJava;

import java.io.PrintWriter;

/**
 * Prolog atom representation. This class is used to represent
 * prolog atoms and empty lists (The empty list [] is represented
 * in prolog and in the fast_read/fast_write format as an atom).
 * This class does not represent numeric atoms; use instead
 * PLInteger and PLFloat classes.
 */
public class PLAtom extends PLTerm {
  protected String Value;

  /**
   * Atom constructor. Creates a new atom object with its name given
   * as argument.
   *
   * @param name Name of the prolog atom.
   */
  public PLAtom(String name) {

    Type = PLTerm.ATOM;
    Value = name;

  }

  /**
   * String representation of a prolog atom.
   */
  public String toString() {

    return new String(Value);

  }

  /**
   * Gets the java representation of the atom as an object.
   *
   * @param i <code>PLInterpreter</code> object representing a prolog
   *          to java interpreter. Although is not used in this class,
   *          is included to implement the same method of class
   *          <code>PLTerm</code>.
   *
   * @return a java <code>Object</code> with the name of this prolog atom.
   */
  public Object javaRepr(PLInterpreter i) {
    
    return (Object)Value;

  }

  /**
   * Gets the name of this atom as a string. Returns a copy of the
   * string name instead of the name itself.
   */
  public String getName() {
    
    return new String(Value);

  }

  /**
   * Execution test on prolog objects. Implements the abstract method
   * of class <code>PLTerm</code>.
   * 
   * @return true if the related prolog term can be evaluated as a goal;
   *         false otherwise.
   */
  public boolean isRunnable() {

    return true;

  }

  /**
   * comparison between prolog terms.
   *
   * @param term The <code>PLTerm</code> object to compare with this object.
   *
   * @return true if the term received as argument is similar to this
   *         atom: is a <code>PLAtom</code> object and has the same
   *         atom name;
   *         false otherwise.
   */
  public boolean equals(PLTerm term) {

    if ((Type == term.Type) && Value.equals(((PLAtom)term).Value))
      return true;
    else
      return false;

  }

  /**
   * Makes a full copy of this <code>PLAtom</code> object.
   */
  public PLTerm copy() {
    
    return (PLTerm)(new PLAtom(this.getName()));

  }

  /**
   * Returns the number of cells used in the internal prolog
   * representation.
   * This function is only needed for $fast format generation in
   * prolog $fast_read version 'a'.
   */
  protected int numberOfCells() {
    return 0;
  }

  /**
   * this method is under development. Must implement the launching
   * of a prolog goal with no arguments.
   */
  public void launchGoal(PrintWriter out) throws PLGoalException {

  }

}

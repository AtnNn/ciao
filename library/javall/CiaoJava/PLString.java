package CiaoJava;

import java.util.Vector;

/**
 * java representation of a prolog string.
 */
public class PLString extends PLTerm {
  private String Value;

  /**
   * Given a java string, creates a java <code>PLString</code>
   * object.
   *
   * @param s String that will contain the prolog string.
   */
  public PLString(String s) {

    Type = PLTerm.STRING;
    Value = s;

  }

  /**
   * String representation of a java PLString object.
   *
   * @return a java string with a copy of the prolog string. 
   */
  public String toString() {

    return new String(Value);

  }

  /**
   * Gets the value of a prolog string object.
   *
   * @return the value of this prolog string object.
   */
  public String getValue() {

    return Value;

  }

  /**
   * Java representation of a PLString. 
   *
   * @param i <code>PLInterpreter</code> object used to 
   *          obtain the java representation. Included
   *          here only for compatibility purposes.
   *
   * @return a java object representation of this prolog string.
   */
  public Object javaRepr(PLInterpreter i) {

    return new String(Value);

  }

  /**
   * Execution test on prolog objects. Returns true if the
   * related prolog term can be evaluated.
   *
   * @return Always <code>false</code>.
   */
  public boolean isRunnable() {

    return false;

  }

  /**
   * Compares the PLString object with the PLTerm given as
   * argument. Implementation of the method inherited from
   * PLTerm.
   *
   * @param t Prolog term to be compared to.
   *
   * @return <code>true</code> if this prolog string is equal
   *         to the term received as argument;
   *         <code>false</code> otherwise.
   */
  public boolean equals(PLTerm t) {

    if (Type == t.Type && Value.equals(((PLString)t).getValue()))
      return true;
    else
      return false;
  }

  /**
   * Makes a full copy of this <code>PLString</code>
   * object. Recursively clones the elements of this term.
   *
   * @return a copy of this prolog term.
   */
  public PLTerm copy() {

    return (PLTerm)(new PLString(Value));

  }
    

  /**
   * Returns the number of characters of this <code>PLString</code>.
   *
   * @return The number of characters of this prolog string.
   */
  public int length() {

    return Value.length();

  }

  /**
   * Returns the number of cells needed to represent
   * this PLString in the prolog heap. Only used for
   * $fast_read format version 'a'.
   *
   * @return 0 (no cells used).
   */
  protected int numberOfCells() {

    return 0;

  }
}


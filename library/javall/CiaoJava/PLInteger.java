package CiaoJava;

/**
 * Prolog integer representation.
 * Warning: this implementation can only work with prolog integers
 * that fit in java Integer type.
 */
public class PLInteger extends PLTerm {
  /**
   * Value of the prolog integer.
   */
  Integer Value;

  /**
   * Integer constructor with its value given as argument.
   *
   * @param v Initial value of this <code>PLInteger</code> object.
   *        This argument must be the largest representation of an
   *        integral number to manage prolog integers.
   */
  public PLInteger(int v) {

    Type = PLTerm.INTEGER;
    Value = new Integer(v);

    }

  /**
   * String representation.
   */
  public String toString() {

    return Value.toString();

    }

  /**
   * Gets the java representation of this prolog integer as an object.
   * The object returned must be a java <code>Integer</code> object.
   *
   * @param i <code>PLInterpreter</code> object to interpret the
   *          this prolog term (included for compatibility with
   *          the abstract declaration in <code>PLTerm</code>).
   *
   * @return  An <code>Object</code> representing the prolog integer.
   *          This object will be a java <code>Integer</code> object.
   */
  public Object javaRepr(PLInterpreter i) {

    return (Object)Value;

    }

  /**
   * Gets the value of the integer object.
   */
  public int getValue() {

    return Value.intValue();

    }

  /**
   * Execution test on prolog objects. Returns true if the
   * related prolog term can be evaluated.
   */
  public boolean isRunnable() {

    return false;
  
  }

  /**
   * comparison between prolog terms.
   *
   * @param t Prolog term to compare to.
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (this.getValue() == ((PLInteger)t).getValue()))
      return true;
    else
      return false;

  }

  /**
   * Makes a full copy of this <code>PLInteger</code> object.
   */
  public PLTerm copy() {
    
    return (PLTerm)(new PLInteger(this.getValue()));

  }

  /**
   * Number of cells used in the internal prolog representation.
   * This function is only needed for $fast format generation in
   * prolog $fast_read version 'a'.
   * This will NOT work if the internal prolog representation of
   * this integer needs a structure (large integer representation).
   */
  protected int numberOfCells() {
    return 0;
  }
}






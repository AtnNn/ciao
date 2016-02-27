package CiaoJava;

/**
 * This class represents the prolog floats. Internally the prolog floats are
 * stored as double, to allow the conversion in the java side.
 */
public class PLFloat extends PLTerm {
  /**
   * Value of this prolog float. Must be the largest representation
   * of a float point number in java, so can be implemented prolog floats.
   */
  Double Value;

  /**
   * Creates a new <code>PLFloat</code> object
   * with the given float value.
   *
   * @param v <code>double</code> value that will contain the new object.
   */
  public PLFloat(double v) {

    Type = PLTerm.FLOAT;
    Value = new Double(v);

  }

  /**
   * String representation of a prolog float. Uses the representation 
   * of the toString method of the Double java class.
   *
   * @return The string representation of the prolog float.
   */
  public String toString() {

    return Value.toString();

  }

  /**
   * Java representation of a prolog float. Returns the java 
   * <code>Double</code> object that contains the prolog float.
   *
   * @return an <code>Object</code> instance that contains a 
   *         <code>Double</code> object.
   *
   */
  public Object javaRepr(PLInterpreter i) {

    return (Object)Value;
  
  }

  /**
   * Returns the value of this prolog float.
   */
  public double getValue() {

    return Value.doubleValue();

  }
  
  /**
   * Execution test on prolog objects. Returns true if the
   * related prolog term can be evaluated.
   */
  public boolean isRunnable() {

    return false;

  }

  /**
   * Comparison between prolog terms. 
   *
   * @param t Prolog term to compare to.
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (Value == ((PLFloat)t).Value))
      return true;
    else
      return false;

  }

  /**
   * Makes a full copy of this <code>PLFloat</code> object.
   */
  public PLTerm copy() {
    
    return (PLTerm)(new PLFloat(this.getValue()));

  }

  /**
   * Not implemented.
   */
  protected int numberOfCells() {
    // @@@@@ Debe retornar el numero correcto de celdas del heap.
    return 0;
  }
}




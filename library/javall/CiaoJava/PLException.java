package CiaoJava;

/**
 * This class implements the exceptions that can be thrown
 * using the CiaoJava package.
 */
public class PLException extends Exception {

  /**
   * Creates a new <code/>PLException</code>
   * with no description.
   */
  public PLException() {

    super();

  }

  /**
   * Creates a new <code/>PLException</code>
   * with the description given as argument.
   *
   * @param s String that contains the description of the
   *          exception.
   */
  public PLException(String s) {

    super(s);

  }

  /**
   * Translated a prolog exception represented as a 
   * term in a java PLException.
   *
   * @param prologException <code>PLTerm</code> object that represents
   *                        the prolog exception.
   */
  public static PLException translateException(PLTerm prologException) {

    return new PLException(prologException.toString());

  }
}






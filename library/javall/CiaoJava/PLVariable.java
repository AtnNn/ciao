package CiaoJava;

/**
 * Prolog variables representation
 */
public class PLVariable extends PLTerm {
  int VarNumber;
  PLTerm Binding = null;
  private final int SINGLE_REF = -1;
  private static int lastRef = -1;

  /**
   * Creates a new prolog variable
   * and references it to a free variable number.
   */
  public PLVariable() {

    Type = PLTerm.VARIABLE;
    if (lastRef < 0) {
      VarNumber = 0;
      lastRef = 0;
    } else
      VarNumber = ++lastRef;

  }

  /**
   * Creates a new prolog variable with a given
   * variable number.
   *
   * @param number variable number.
   */
  public PLVariable(int number) {

    Type = PLTerm.VARIABLE;
    VarNumber = number;

    if (lastRef < number)
      lastRef = number;

  }

  /**
   * free variable test. Returns true if this prolog variable is
   * unbound.
   */
  public boolean isFree() {

    return (Binding == null);

  }

  /**
   * Returns the binding of this prolog variable. If this variable
   * is free, returns null.
   */
  public PLTerm getBinding() {

    return Binding;

  }

  /**
   * Returns the internal variable number.
   */
  public int getNumber() {

    return VarNumber;

  }

  /**
   * Variable binding. Binds the prolog variable represented
   * by this object to a prolog term. If the variable is
   * already bound, the binding is replaced with this one.
   *
   * @param term Term to bind to.
   */
  public void bind(PLTerm term) {
    
    Binding = term;

  }

  /**
   * Variable unbinding. Uninstantiates this prolog variable.
   */
  public void unbind() {

    Binding = null;

  }

  /**
   * Returns the string representation of this prolog variable
   * If the variable is bound to a prolog term, this term is
   * also represented enclosed between brackets.
   *
   * @return The string representation of this variable.
   */
  public String toString() {
    String ret;

    if (VarNumber == SINGLE_REF)
      ret = "_";
    else
      ret = "_" + String.valueOf(VarNumber);

    if (!isFree())
      ret = ret + "{" + Binding.toString() + "}";

    return ret;
  }

  /**
   * Java representation of a variable: just itself. 
   */
  public Object javaRepr(PLInterpreter i) {

    return (Object)this;

  }

  /**
   * Execution test on prolog objects.
   *
   * @return <code>true</code> if the
   *         related prolog term can be evaluated;
   *         <code>false</code> otherwise.
   */
  public boolean isRunnable() {
  	return false;
  }

  /**
   * comparison between prolog terms. 
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (VarNumber == ((PLVariable)t).VarNumber))
      return true;
    else
      return false;
  }

  /**
   * Makes a full copy of this <code>PLVariable</code> prolog variable
   * object. Recursively clones the term bound by this variable.
   */
  public PLTerm copy() {

    PLVariable v = new PLVariable();
    if (!this.isFree()) {
      PLTerm content = this.Binding.copy();
      v.bind(content);
    }
    return (PLTerm)v;

  }

  /**
   * Term unification. Unifies this prolog variable with the term
   * received as argument. If this is a free variable, binds it
   * to the term received as argument. If not, tries to unify
   * the binding with the term received.
   *
   * @param term Term to unify with.
   *
   * @return true if the unification is successful: false otherwise.
   */
  public boolean unify(PLTerm term) {

    if (Binding == null) {
      Binding = term;
      return true;
    }
    else
      return Binding.unify(term);

  }

  /**
   * Undo the unification made on this variable using as pattern
   * the term received as argument.
   */
  public void backtrack(PLTerm term) throws PLException {

    if (term.isVariable())
      this.bind(((PLVariable)term).getBinding());
    else
      throw new PLException("Object cannot be backtracked" + this.toString());

  }

  /**
   *
   */
  protected int numberOfCells() {
    return 0;
  }
}







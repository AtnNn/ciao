package CiaoJava;

import java.util.Vector;

/**
 * java representation of a prolog list.
 */
public class PLList extends PLTerm {
  private PLTerm Head;
  private PLTerm Tail;
  private final int START_CAPACITY = 16;
  private final int INCREMENT = 16;

  /**
   * Given a head and a tail, creates a java PLList object.
   * The tail term must be nil or another <code>PLList</code>.
   *
   * @param h First element of the list. Can be any prolog term.
   * @param t Rest of the list. Must be nil (if the list contains
   *          just one element), or another <code>PLList</code> object.
   */
  public PLList(PLTerm h, PLTerm t) {
    Type = PLTerm.LIST;
    if ((t.equals(PLTerm.nil)) || (t.Type == Type)) {
      Head = h;
      Tail = t;
      }
    else {
      System.err.println("Error: wrong tail type (" + t.Type + ")");
      System.exit(1);
      }
  }

  /**
   * Given a java list, creates a java PLList object
   * containing the objects included in the java list.
   * Is important to realize that the array argument must contain
   * at least one element. An empty list is implemented in the
   * java representation as an atom with name "[]".
   *
   * @param list Java list that contains the elements that must be
   *             included in the prolog list.
   */
  public PLList(PLTerm list[]) {
    Type = PLTerm.LIST;

    if (list.length > 1) {
      PLTerm tail[] = new PLTerm[list.length-1];
      System.arraycopy(list, 1, tail, 0, list.length-1);
      Head = list[0];
      Tail = new PLList(tail);
    }
    else if (list.length == 1) {
      Head = list[0];
      Tail = PLTerm.nil;
    }
    else
      System.err.println("nil creation as list failure");
  }

  /**
   * Given a java string, creates a java PLList object
   * containing the characters included in the java string.
   * Is important to realize that the string argument must contain
   * at least one character. An empty list is implemented in the
   * java representation as an atom with name "[]".
   *
   * @param s Java string that contains the characters that must be
   *          included in the prolog list as elements.
   */
  public PLList(String s) {
    Type = PLTerm.LIST;

    if (s.length() > 1) {
      Head = new PLInteger(s.getBytes()[0]);
      Tail = new PLList(s.substring(1));
    }
    else if (s.length() == 1) {
      Head = new PLInteger(s.getBytes()[0]);
      Tail = PLTerm.nil;
    }
    else
      System.err.println("nil creation as list failure");
  }

  /**
   * String representation of a java PLList object.
   *
   * @return The string representation of the prolog list.
   */
  public String toString() {
    PLList t;
    String s = "[";

    t = this;
    while (t.getTail().Type == Type) {
      s = s + t.getHead().toString() + ", ";
      t = (PLList)t.getTail();
    }
    
    return s + t.getHead().toString() + "]";
  }

  /**
   * Java representation of a PLList. The java representation
   * of a prolog list is an object array.
   *
   * @param i <code>PLInterpreter</code> object used to do
   *          the interpretation. Included here only for
   *          compatibility purposes with the <code>PLTerm</code>
   *          abstract class.
   *
   * @return  a java object that represents the prolog list.
   *          This java representation is built by an
   *          <code>Object</code> array.
   */
  public Object javaRepr(PLInterpreter i) {
    Vector v = new Vector(START_CAPACITY, INCREMENT);
    Object[] a;
    PLTerm t;

    t = this;
    do {
      v.addElement(((PLList)t).getHead().javaRepr(i));
      t = ((PLList)t).getTail();
    } while (t.Type == Type);
    a = new Object[v.size()];
    v.copyInto(a);
    return (Object)a;
  }

  /**
   * Execution test on prolog objects. Returns true if the
   * related prolog term can be evaluated. Implements the
   * abstract method declared in the <code>PLTerm</code> class.
   *
   * @return always <code>false</code>. 
   */
  public boolean isRunnable() {
  	return false;
  }

  /**
   * Gets the head of a PLList object.
   *
   * @return the first element of the prolog list.
   */
  public PLTerm getHead() {
    return Head;
  }

  /**
   * Gets the tail of a PLList object. The object returned may be
   * nil or another list.
   *
   * @return the prolog list result of removing the first element
   *         of this list.
   */
  public PLTerm getTail() {
    return Tail;
  }

  /**
   * Sets the tail of a PLList object, removing the previous tail.
   * Important: this method does not conform prolog list handling
   * and must be used very carefully.
   *
   * @param l <code>PLList</code> object that represents the new
   *          tail.
   */
  protected void setTail(PLList l) {
    Tail = l;
  }

  /**
   * Adds a term at the tail of a PLList object.
   *
   * @param term Prolog term to be added at the end of this list.
   */
  public void add(PLTerm term) {
    PLList mytail = this;

    while (mytail.getTail().Type == PLTerm.LIST)
      mytail = (PLList)mytail.getTail();
    mytail.setTail(new PLList(term,PLTerm.nil));
  }

  /**
   * Compares the PLList object with the PLTerm given as
   * argument. Implementation of the method inherited from
   * PLTerm.
   *
   * @param t Prolog term to be compared to.
   *
   * @return  <code>true</code> if this term is equal to
   *          the term received as argument;
   *          <code>false</code> otherwise.
   */
  public boolean equals(PLTerm t) {

    if (Type == t.Type) {
      if (Head.equals(((PLList)t).Head) && Tail.equals(((PLList)t).Tail))
        return true;
      else
        return false;
    }
    else
      return false;
  }

  /**
   * Makes a full copy of this <code>PLList</code> prolog list
   * object. Recursively clones the elements of this term.
   *
   * @return a <code>PLTerm</code> object that is a full
   *         copy of this list. All the elements of this list
   *         are copied in turn.
   */
  public PLTerm copy() {

    PLTerm head = this.getHead().copy();
    PLTerm tail = this.getTail().copy();
    
    PLList l = new PLList(head, tail);
    return (PLTerm)l;

  }
    
  /**
   * Term unification. Unifies this prolog list with the term
   * received as argument. This method overrides the one 
   * inherited from PLTerm.
   * 
   * <p><bold>Important:</bold> The unification is 'two sided':
   * the variables found in the term received as argument could
   * be bound in order to unify the complete terms. In the same
   * way, the variables found in this list could be bound to
   * unify both terms.</p>
   *
   * @param term Term to unify with.
   * @return     <code>true</code> if the unification is successful;
   *             <code>false</code> otherwise.
   */
  public boolean unify(PLTerm term) {

    if (term.isVariable()) {
      if (((PLVariable)term).isFree()) {
	((PLVariable)term).bind(this);
	return true;
      }
      else
	return this.unify(((PLVariable)term).getBinding());
    }
    else if (Type == term.Type) {
      PLList l = (PLList)term;
      return (Head.unify(l.getHead()) && Tail.unify(l.getTail()));
    }

    return false;

  }

  /* Undo the unification made on this list using as pattern
   * the term received as argument.
   *
   * @param term Prolog term to be used as pattern for 
   *             backtracking.
   */
  public void backtrack(PLTerm term) throws PLException {

    if (Type == term.Type) {
      PLList l = (PLList)term;

      this.getHead().backtrack(l.getHead());
      this.getTail().backtrack(l.getTail());
    }
    else
      throw new PLException("Object cannot be backtracked" + this.toString());

  }

  /**
   * Returns the number of elements of this <code>PLList</code>.
   *
   * @return The number of elements of this prolog list.
   */
  public int length() {
    PLList t;
    int len = 1;

    t = this;
    while (t.getTail().Type == Type) {
      len++;
      t = (PLList)t.getTail();
    }
    
    return len;
  }

  /**
   * Returns the number of cells needed to represent
   * this PLList in the prolog heap. Only used to
   * build the fast_write representation in 'a' version.
   *
   * @return the number of cells needed.
   */
  protected int numberOfCells() {
    PLList t;
    int num = 2;

    t = this;
    while (t.getTail().Type == Type) {
      num += t.getHead().numberOfCells() + 1;
      t = (PLList)t.getTail();
    }
       
    return num;
  }
}


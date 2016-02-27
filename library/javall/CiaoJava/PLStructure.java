package CiaoJava;

import java.io.PrintWriter;

/**
 * Prolog structure representation.
 */
public class PLStructure extends PLTerm {
  String Name;
  int Arity;
  PLTerm Args[];

  /**
   * Creates a new <code>PLStructure</code> object
   * with the functor, arity and arguments received as parameters.
   *
   * @param name  Functor name.
   * @param arity Structure arity.
   * @param arg   Array of prolog arguments.
   *
   */
  public PLStructure(String name, int arity, PLTerm arg[]) {

    Type = PLTerm.STRUCTURE;
    Name = name;
    Arity = arity;
    Args = new PLTerm[arity];
    for (int i = 0; i < arity; i++)
      Args[i] = arg[i];

  }

  /**
   * Creates a new <code>PLStructure</code> object
   * with the functor and arguments received as parameters.
   *
   * @param name  Functor name.
   * @param arg   Array of prolog arguments.
   *
   */
  public PLStructure(String name, PLTerm arg[]) {

    Type = PLTerm.STRUCTURE;
    Name = name;
    Arity = arg.length;
    Args = new PLTerm[Arity];
    for (int i = 0; i < Arity; i++)
      Args[i] = arg[i];

  }

  /**
   * String representation of a prolog structure.
   *
   * @return a java string that represents the contents of this
   *         prolog structure.
   */
  public String toString() {

    String s = Name + "(";

    for (int i = 0; i < Arity-1; i++)
      s += Args[i].toString() + ", ";
    return s + Args[Arity-1].toString() + ")";

  }

  /**
   * Returns the functor of this prolog structure.
   *
   * @return the string that contains the functor of this
   *         prolog structure.
   */
  public String getFunctor() {

    return Name;

  }

  /**
   * Returns the arity of this prolog structure.
   *
   * @return the number of elements of this structure.
   */
  public int getArity() {

    return Arity;

  }

  /**
   * Returns an array of prolog terms containing the arguments
   * of this structure.
   *
   * @return a java array of <code>PLTerm</code> objects that
   *         contains the arguments of this structure.
   */
  public PLTerm[] getArgs() {

    return Args;

  }

  /**
   * Returns the argument number <code>argNumber</code>, received
   * as argument.
   *
   * @param argNumber Position of the argument to be returned.
   *
   * @return the prolog term included in the position <code>argNumber</code>
   *         of the argument list.
   */
  public PLTerm getArg(int argNumber) {

    if (argNumber < Args.length)
      return Args[argNumber];
    else
      return null;

  }

  /**
   * java representation of a structure. If this structure
   * refers to a java object in the object table of the
   * interpreter received as argument, then this java object
   * is returned. Otherwise, this <code>PLStructure</code>
   * object is returned itself.
   *
   * @param i <code>PLInterpreter</code> object used to
   *          build the java representation.
   *
   * @return a java object with the java representation
   *         of this prolog structure.
   */
  public Object javaRepr(PLInterpreter i) {

    if (Name.equals(JAVA_OBJECT) && Arity == 1)
      return i.getObject((Integer)Args[0].javaRepr(i));
    else if (PLInterpreter.isInterpretable(this))
      return (i.interpret(this)).javaRepr(i);
    //    else if (isJavaType())
    //      return getJavaObject();
    else
      return (Object)this;
  }

//   /**
//    * Java type test on prolog objects. Returns true if the
//    * related prolog term can be evaluated as a java-type
//    * prolog declaration, that is, a structure with one argument
//    * and specific functor used to inform to java about the type
//    * of the argument received from prolog.
//    *
//    * @return <code>true</code> if the term represented by this
//    *         prolog structure can be evaluated as a java-type
//    *         specification;
//    *         <code>false</code> otherwise.
//    */
//   private boolean isJavaType() {
    
//     if (Arity == 1)
//       if (Name.equals(JAVA_INTEGER) ||
// 	  Name.equals(JAVA_SHORT) ||
// 	  Name.equals(JAVA_BYTE) ||
// 	  Name.equals(JAVA_LONG) ||
// 	  Name.equals(JAVA_FLOAT) ||
// 	  Name.equals(JAVA_DOUBLE) ||
// 	  Name.equals(JAVA_BOOLEAN) ||
// 	  Name.equals(JAVA_CHARACTER))
// 	return true;
    
//     return false;

//   }

//   /**
//    * Given a java-type prolog specification, returns the java
//    * object that represents it.
//    *
//    * @return An <code>Object</code> instance with the java 
//    *         representation of the java-type specification;
//    *         <code>null</code> if there is no possible 
//    *         java representation (when isJavaType returns 
//    *         <code>false</code>).
//    */
//   private Object getJavaObject() {
    
//     try {
//       if (Arity == 1)
// 	if (Name.equals(JAVA_INTEGER))
// 	  return (Object)(new Integer(Args[0].javaRepr())).intValue();
// 	else if (Name.equals(JAVA_SHORT))
// 	  return (Object)(new Short(Args[0].javaRepr())).shortValue();
// 	else if (Name.equals(JAVA_BYTE))
// 	  return (Object)(new Byte(Args[0].javaRepr())).byteValue();
// 	else if (Name.equals(JAVA_LONG))
// 	  return (Object)(new Long(Args[0].javaRepr())).longValue();
// 	else if (Name.equals(JAVA_FLOAT))
// 	  return (Object)(new Float(Args[0].javaRepr())).floatValue();
// 	else if (Name.equals(JAVA_DOUBLE))
// 	  return (Object)(new Double(Args[0].javaRepr())).doubleValue();
// 	else if (Name.equals(JAVA_BOOLEAN)) {
// 	  String s = (String)(Args[0].javaRepr());
// 	  return (Object)(new Boolean(s.equals(JAVA_BOOLEAN_YES)));
// 	}
// 	else if (Name.equals(JAVA_CHARACTER))
// 	  //@@@@@ ojojo: todavia no hay manera de implementar esto...
// 	  //	  return (Object)(new Character(Args[0].javaRepr())).charValue();
// 	  ;
//     } catch (Exception e) {
//       return null;
//     }
    
//     return null;

//   }

  /**
   * Execution test on prolog objects. Returns true if the
   * related prolog term can be evaluated.
   *
   * @return Always <code>true</code> (every prolog structure
   *         can be used to represent a prolog goal).
   */
  public boolean isRunnable() {
  	return true;
  }

  /** 
   * comparison between prolog terms. 
   *
   * @param t Prolog term to be compared to.
   *
   * @return <code>true</code> if this structure is equal
   *         to the prolog term received as argument;
   *         <code>false</code> otherwise.
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (Arity == ((PLStructure)t).Arity)) {
      for (int i = 0; i < Arity; i++)
        if (!Args[i].equals(((PLStructure)t).Args[i]))
          return false;
      return true;
    }
    else
      return false;
  }

  /**
   * Makes a full copy of this <code>PLStructure</code> prolog structure
   * object. Recursively clones the arguments of this term.
   *
   * @return a <code>PLTerm</code> object that contains a full copy 
   *         of this prolog structure; that is, no argument is shared
   *         between this object and the prolog term returned.
   *
   */
  public PLTerm copy() {

    PLTerm argCopy[] = new PLTerm[this.Arity];
    for (int i = 0; i < this.Arity; i++)
      argCopy[i] = this.Args[i].copy();

    PLStructure s = new PLStructure(this.Name, this.Arity, argCopy);
    return (PLTerm)s;

  }
    
  /**
   * Term unification. Unifies this prolog structure with the term
   * received as argument. This method overrides the one 
   * inherited from PLTerm.
   * 
   * <p><bold>Important:</bold> The unification is 'two sided':
   * the variables found in the term received as argument could
   * be bound in order to unify the complete terms. In the same
   * way, the variables found in this structure could be bound to
   * unify both terms.</p>
   *
   * @param term Term to unify with.
   * @return true if the unification is successful: false otherwise.
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
    else
      if ((Type == term.Type) && (Arity == ((PLStructure)term).Arity)) {
	PLStructure s = (PLStructure)term;
      
	for (int i = 0; i < Arity; i++)
	  if (!Args[i].unify(s.Args[i]))
	    return false;
	return true;
      }

    return false;
  }

  /* Undo the unification made on this Structure using as pattern
   * the term received as argument.
   *
   * @param term Prolog term used as pattern for the
   *             backtracking.
   */
  public void backtrack(PLTerm term) throws PLException {

    if ((Type == term.Type) && (Arity == ((PLStructure)term).Arity)) {
      PLStructure s = (PLStructure)term;
      
      for (int i = 0; i < Arity; i++)
	this.Args[i].backtrack(s.Args[i]);
    }
    else
      throw new PLException("Object cannot be backtracked" + this.toString());

  }

  protected int numberOfCells() {
    int num = 2 * (Arity + 1);

    for (int i = 0; i < Arity; i++)
      num += Args[i].numberOfCells();
       
    return num;
  }

  /**
   * Goal launching. Evaluates this structure as a goal and prints
   * it to the eventStream to be launched by prolog. Before launching, 
   * calculates the values of arguments with the interpreter given, if they
   * must be constructed by java.
   *
   * @param interpreter <code>PLInterpreter</code> object used to interpret
   *                    the arguments of this structure before launching
   *                    to the prolog process.
   * @param pl          <code>PLConnection</code> object that represents
   *                    the connection to the prolog process.
   **/
  public void launchGoal(PLInterpreter interpreter,
                         PLConnection pl) {

    PLTerm args[] = new PLTerm[Arity];
    for (int i = 0; i < Arity; i++)
      if (PLInterpreter.isInterpretable(Args[i]))
        args[i] = interpreter.interpret(Args[i]);
      else
        args[i] = Args[i];

    PLStructure pred = new PLStructure(Name, Arity, args);

    pl.toPrologEvent(this);
  }

  /**
   * Goal launching. Evaluates this structure as a goal and prints
   * it to the eventStream to be launched by prolog. Before launching, 
   *
   * @param pl          <code>PLConnection</code> object that represents
   *                    the connection to the prolog process.
   */
  public void launchGoal(PLConnection pl) {

    pl.toPrologEvent(this);

  }

}

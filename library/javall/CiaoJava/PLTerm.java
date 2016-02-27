package CiaoJava;

import java.util.*;
import java.io.*;

/**
 * java representation of prolog terms.
 * Abstract class for representing generic prolog
 * terms.
 * The rest of the term representation classes are
 * childs of this one.
 */
public abstract class PLTerm extends Object {

  /*
   * Term types.
   */
  public int Type;
  static final int INTEGER   = 1;
  static final int FLOAT     = 2;
  static final int ATOM      = 3;
  static final int LIST      = 4;
  static final int STRUCTURE = 5;
  static final int VARIABLE  = 6;
  static final int STRING    = 7;

  /*
   * Other constants.
   */
  private static final String INTERPRETER_ERROR = "No";
  private static final String INTERPRETER_SUCCESS = "Yes";
  public static final PLTerm nil = new PLAtom("[]");
  public static final PLTerm success = new PLAtom(INTERPRETER_SUCCESS);
  public static final PLTerm fail = new PLAtom(INTERPRETER_ERROR);
  static final String JAVA_OBJECT = "$java_object";
  static final String PROLOG_EXCEPTION = "prolog_exception";
  static final String PROLOG_FAIL = "prolog_fail";
  static final String PROLOG_SUCCESS = "prolog_success";
  static final String PROLOG_QUERY_ID = "prolog_query_id";

  /*
   * fastRead/fastWrite fields constants.
   */
  private static String AtomTable[] = {};
  private static int VarNumber = 0;
  private static int VarCounter = 0;

  private static final int STRING_BUFFER_SIZE = 20;
  private static final int MAX_READ_BUFFER = 512;

  /*
   * Constants for fast_read/fast_write version 'a'.
   */
  public static final char VERSION_A = 'a';
  private static final char PFX_LIST_WITH_NIL_TAIL = 'l';
  private static final char PFX_LIST_WITH_NONIL_TAIL = 'L';
  private static final char PFX_SHORT_INT = 'i';
  private static final char PFX_LONG_INT = 'I';
  private static final char PFX_FLOAT = 'f';
  private static final char PFX_MULTI_REF_VAR = 'V';
  private static final char PFX_SINGLE_REF_VAR = 'v';
  private static final char PFX_ATOM = 'a';
  private static final char PFX_MULTI_REF_ATOM = 'A';
  private static final char PFX_STRUCT = 's';
  private static final char PFX_MULTI_REF_STRUCT = 'S';

  /*
   * Constants for fast_read/fast_write version 'C'.
   */
  public static final char VERSION_C = 'C';
  private static final char PFXC_LIST = '[';
  private static final char PFXC_STRING = '\"';
  private static final char PFXC_INTEGER = 'I';
  private static final char PFXC_FLOAT = 'F';
  private static final char PFXC_VARIABLE = '_';
  private static final char PFXC_ATOM = 'A';
  private static final char PFXC_STRUCT = 'S';
  private static final char PFXC_NIL = ']';

  private static final char currentVersion = VERSION_C;

  /****************************************************/
  /* Abstract or overlapped methods                   */
  /****************************************************/

  /**
   * String representation of the prolog term. 
   */
  abstract public String toString();

  /**
   * java representation of the prolog term.
   * Is used to obtain the equivalent java object.
   * 
   * @param i <code>PLInterpreter</code> object used
   *          to interpret the prolog representation
   *          of java objects.
   *
   * @return  a java object representation of the prolog
   *          term.
   */
  abstract public Object javaRepr(PLInterpreter i);

  /**
   * Execution test for prolog objects.
   *
   * @return <code>true</code> if this prolog term can
   *         be evaluated in the prolog side as a goal;
   *         <code>false</code> otherwise.
   */
  abstract public boolean isRunnable();

  /**
   * Comparison between prolog terms.
   *
   * @param t Term to be compared to.
   *
   * @return <code>true</code> if this prolog term is
   *         equal to the term received as argument;
   *         <code>false</code> otherwise.
   */
  abstract public boolean equals(PLTerm t);

  /**
   * Number of cells on the prolog WAM heap for this term.
   */
  abstract protected int numberOfCells();

  /**
   * Duplication of prolog terms.
   * Performs an in depth term duplication, so that will be
   * no shared elements between this term and the copy
   * returned.
   *
   * @return a copy of this prolog term.
   */
  abstract public PLTerm copy();

  /**
   * Execution of a prolog goal. This method is combined
   * with <tt>nextSolution</tt> to obtain the set of
   * solutions given by a query on a goal.
   * Only are executed those terms where <tt>isRunnable</tt>
   * returns <tt>true</tt>.
   *
   * @param out	socket descriptor for sending the
   *            goal description to prolog.
   *
   * @return    prolog goal identifier, if the goal is accepted
   *            as a valid goal in the prolog side;
   *            prolog Exception term, if a problem has been
   *            found translating the term to a prolog goal.
   *
   * @exception PLGoalException if this term cannot be a prolog goal.
   */
  public void launchGoal(PrintWriter out) throws PLGoalException {

    throw new PLGoalException("This term cannot be a prolog goal: " 
			      + this.toString());

  }

  /**
   * Term unification. Unifies this prolog term with the term
   * received as argument. 
   * 
   * <p><bold>Important:</bold> The unification is 'two sided':
   * the variables found in the term received as argument could
   * be bound in order to unify the complete terms. In the same
   * way, the variables found in this term could be bound to
   * unify both terms.</p>
   *
   * @param term Term to unify with.
   *
   * @return true if the unification is successful: false otherwise.
   */
  public boolean unify(PLTerm term) {
    // This is the most general code.
    // For PLList, PLStructure and PLVariable there are specific code,
    // that overrides this method.

    if (term.isVariable()) {
      if (((PLVariable)term).isFree()) {
	((PLVariable)term).bind(this);
	return true;
      }
      else
	return this.unify(((PLVariable)term).getBinding());
    }
    else
      return this.equals(term);

  }

  /**
   * Undo the unification made on this <code>PLTerm</code> object
   * using as pattern the term received as argument.
   * This is the most general code.
   * For PLList, PLStructure and PLVariable there are specific code 
   * that overrides this method.
   *
   * @param term Term to be used as pattern.
   *
   * @exception PLException if this term cannot be backtracked.
   */
  public void backtrack(PLTerm term) throws PLException {

    if (!this.equals(term))
      throw new PLException("Object cannot be backtracked: " 
			    + term.toString()
			    + " against " + this.toString());

  }

  /**
   * Variable test.
   *
   * @return <code>true</code> if this prolog term is a variable;
   *         <code>false</code> otherwise.
   */
  public boolean isVariable() {
    return (Type == VARIABLE);
  }

  /**
   * Structure test. 
   *
   * @return <code>true</code> if this prolog term is a structure;
   *         <code>false</code> otherwise.
   */
  public boolean isStructure() {
    return (Type == STRUCTURE);
  }

  /**
   * List test.
   *
   * @return <code>true</code> if this prolog term is a list;
   *         <code>false</code> otherwise.
   */
  public boolean isList() {
    return (Type == LIST);
  }

  /**
   * String test. 
   *
   * @return <code>true</code> if this prolog term is a string;
   *         <code>false</code> otherwise.
   */
  public boolean isString() {
    return (Type == STRING);
  }

  /**
   * Nil test.
   *
   * @return <code>true</code> if this prolog term is nil;
   *         <code>false</code> otherwise.
   */
  public boolean isNil() {
    return (equals(nil));
  }

  /**
   * Prolog exception test.
   *
   * @return <code>true</code> if the term represents a
   *         prolog exception;
   *         <code>false</code> otherwise.
   */
  protected boolean isException() {

    if (Type == STRUCTURE &&
    	((PLStructure)this).getFunctor().equals(PROLOG_EXCEPTION))
      return true;
    else
      return false;
    
  }

  /**
   * Prolog fail test.
   *
   * @return <code>true</code> if the term represents the valid
   *         prolog atom used to indicate goal fail;
   *         <code>false</code> otherwise.
   */
  protected boolean isPrologFail() {

    if (this.equals(new PLAtom(PROLOG_FAIL)))
      return true;
    else
      return false;

  }

  /**
   * Prolog success test.
   *
   * @return <code>true</code> if the term represents the valid
   *         prolog atom used to indicate prolog success;
   *         <code>false</code> otherwise.
   */
  protected boolean isPrologSuccess() {

    if (this.equals(new PLAtom(PROLOG_SUCCESS)))
      return true;
    else
      return false;

  }

  /**
   * Prolog query id test.
   *
   * @return <code>true</code> if the term represents the valid
   *         prolog structure used to indicate a query id;
   *         <code>false</code> otherwise.
   */
  protected boolean isQueryId() {

    if (Type == STRUCTURE &&
    	((PLStructure)this).getFunctor().equals(PROLOG_QUERY_ID) &&
	((PLStructure)this).getArity() == 1)
      return true;
    else
      return false;

  }

  /**
   * Prolog solution test.
   *
   * @return <code>true</code> if the term represents the valid
   *         prolog structure used to indicate a query solution;
   *         <code>false</code> otherwise.
   */
  protected boolean isSolution() {

    if (Type == STRUCTURE &&
    	((PLStructure)this).getFunctor().equals(PLGoal.SOLUTION) &&
	((PLStructure)this).getArity() == 2)
      return true;
    else
      return false;

  }

  /**
   * Conversion from prolog low level format to a java object.
   * Receives a prolog term in low level format from a stream reader,
   * and builds a <code>PLTerm</code> representation of the term
   * received.
   *
   * @param in Reader from which the term will be read.
   *
   * @return a prolog term object with the term read.
   *
   * @exception PLException if raises any error reading the term.
   */
  protected static PLTerm fastRead(BufferedReader in) throws PLException {

    String at[] = {};
    AtomTable = at;
    VarNumber = 0;
    VarCounter = 0;

    // Version.
    char v = getChar(in);
    if (v != currentVersion)
      throw new PLException("Wrong fast_write version");

    // Prefix.
    getPrefix(in);

    // Term itself.
    return getTerm(in);

  }

  /**
   * Gets the prefix of a prolog low level fast format term.
   *
   * @param in Reader from which the term will be read.
   */
  private static void getPrefix(BufferedReader in) throws PLException {
    int NumberOfAtoms;
    int NumberOfCells;

    switch (currentVersion) {
    case VERSION_A:
      /* Number of variables */
      VarNumber = getInt(in);
      
      /* Atom table */
      AtomTable = new String[getInt(in)];
      for (int i = 0; i < AtomTable.length; i++)
        AtomTable[i] = getString(in);
      
      /* Number of cells */
      /* just skipped */
      NumberOfCells = getInt(in);
      break;
    case VERSION_C:
      // null prefix.
      break;
    }
  }

  /**
   * Gets an integer from the buffer until '\0' is found.
   *
   * @param  in Reader from which the term will be read.
   *
   * @return    The integer read.
   *
   * @exception <code>PLException</code> if the integer
   *            received is not a valid java integer.
   */
  private static int getInt(BufferedReader in) throws PLException {
    int value = 0;
    String number = "";
    char c;

    while ((c = getChar(in)) != '\0')
      number = number + String.valueOf(c);

    try {
      value = Integer.valueOf(number).intValue();
    }
    catch (NumberFormatException e) {
      throw new PLException("Cannot parse int (" + number + ")");
    }
    return value;
  }

  /**
   * Gets a float from the buffer until '\0' is found.
   *
   * @param in  Reader from which the term will be read.
   *
   * @return    The float read. 
   *
   * @exception <code>PLException</code> if the float received
   *            is not a valid java float.
   */
  private static float getFloat(BufferedReader in) throws PLException {
    float value = 0;
    String number = "";
    char c;

    while ((c = getChar(in)) != '\0')
      number = number + String.valueOf(c);

    try {
      value = Float.valueOf(number).floatValue();
    }
    catch (NumberFormatException e) {
      throw new PLException("Cannot parse float (" + number + ")");
    }
    return value;
  }

  /**
   * Gets one char from the reader received as argument.
   *
   * @param in  Reader from which the term will be read.
   *
   * @return    The character read. 
   *
   * @exception <code>PLException</code> if the <code>BufferedReader</code>
   *            does not allow more read operations.
   */
  private static char getChar(BufferedReader in) throws PLException {

    char c;

    try {
      c = (char)in.read();
    } catch  (IOException e) {
      throw new PLException("Cannot read from the input socket");
    }

    return c;

  }

  /**
   * Gets a substring from the buffer until '\0' is found.
   *
   * @param in  Reader from which the term will be read.
   *
   * @return    The string read. 
   *
   * @exception <code>PLException</code> if the <code>BufferedReader</code>
   *            does not allow more read operations, and '\0'
   *            is missing.
   */
  private static String getString(BufferedReader in) throws PLException {

    String Subs = "";
    char c;

    while ((c = getChar(in)) != '\0') {
      Subs += c;
    }

    return Subs;
  }

  /**
   * gets a term from the buffer in 'fast' format.
   *
   * @param in  Reader from which the term will be read.
   *
   * @return    The term read. 
   *
   * @exception <code>PLException</code> if there is any problem
   *            during the term parsing.
   */
  private static PLTerm getTerm(BufferedReader in) throws PLException {
    String name;
    int arity;
    PLTerm args[];
    PLList list;

    switch (currentVersion) {
    case VERSION_A:
      switch(getChar(in)) {
      case PFX_LIST_WITH_NIL_TAIL:
        arity = getInt(in);      // arity must be > 0!
        list = new PLList(getTerm(in),PLTerm.nil);
        for (int i = 1; i < arity; i++)
          list.add(getTerm(in));
        return list;
        
      case PFX_LIST_WITH_NONIL_TAIL:
        arity = getInt(in);      // arity must be > 0!
        list = new PLList(getTerm(in),PLTerm.nil);
        for (int i = 1; i < arity; i++)
          list.add(getTerm(in));
        list.add(getTerm(in));
        return list;
        
      case PFX_SHORT_INT:
        return new PLInteger(getInt(in));
        
      case PFX_LONG_INT:
        return new PLInteger(getInt(in));
        
      case PFX_FLOAT:
        return new PLFloat(getFloat(in));
        
      case PFX_MULTI_REF_VAR:
        return new PLVariable(getInt(in));
        
      case PFX_SINGLE_REF_VAR:
        return new PLVariable();
        
      case PFX_ATOM:
        return new PLAtom(getString(in));
        
      case PFX_MULTI_REF_ATOM:
        return new PLAtom(AtomTable[getInt(in)]);
        
      case PFX_STRUCT:
        name = getString(in);
        arity = getInt(in);
        args = new PLTerm[arity];
        for (int i = arity - 1; i >= 0; i--)
          args[i] = getTerm(in);
        return new PLStructure(name, arity, args);
        
      case PFX_MULTI_REF_STRUCT:
        name = AtomTable[getInt(in)];
        arity = getInt(in);
        args = new PLTerm[arity];
        for (int i = arity - 1; i >= 0; i--)
          args[i] = getTerm(in);
        return new PLStructure(name, arity, args);
      }
      break;

    case VERSION_C:
      switch(getChar(in)) {
      case PFXC_LIST:
        PLTerm head = getTerm(in);
        PLTerm tail = getTerm(in);
        return new PLList(head,tail);

      case PFXC_STRING:
	PLString s = new PLString(getString(in));
	getTerm(in);  // nil terminator is read here.
	return s;
        
      case PFXC_INTEGER:
        return new PLInteger(getInt(in));
        
      case PFXC_FLOAT:
        return new PLFloat(getFloat(in));
        
      case PFXC_VARIABLE:
        return new PLVariable(getInt(in));
        
      case PFXC_ATOM:
        return new PLAtom(getString(in));
        
      case PFXC_STRUCT:
        name = getString(in);
        arity = getChar(in);
        args = new PLTerm[arity];
        for (int i = 0; i < arity; i++)
          args[i] = getTerm(in);
        return new PLStructure(name, arity, args);

      case PFXC_NIL:
        return new PLAtom("[]");

      }
      break;
    }
    return null;
  }

  /**
   * Translation from a java object to prolog low level representation.
   *
   * @return a string that contains the low level representation of
   *         this prolog term.
   */
  public String fastWrite() {

    StringBuffer s = new StringBuffer(STRING_BUFFER_SIZE);

    int NumberOfAtoms;

    // Prefix.
    s.append(genPrefix(this));

    // Term itself.
    s.append(genTerm(this));

    return s.toString();
  }

  /**
   * Prefix generation in $fast format.
   *
   * @param t Prolog term for which build a prefix in
   *          low level format.
   *
   * @return  a <code>StringBuffer</code> object that
   *          contains the prefix.
   */
  private StringBuffer genPrefix(PLTerm t) {
    StringBuffer s = new StringBuffer(STRING_BUFFER_SIZE);

    // Version.
    s.append(currentVersion);

    switch(currentVersion) {
    case VERSION_A:
      // Number of variables.
      s.append("0\0");
      
      // Atom table.
      s.append("0\0");
      
      // Number of cells.
      int n = t.numberOfCells();
      s.append(n);
      s.append("\0");
      break;
    }
    return s;
  }

  /**
   * Term generation in $fast format.
   *
   * @param t Prolog term to translate to low level format
   *
   * @return  a <code>StringBuffer</code> object that
   *          contains the low level term representation.
   */
  private StringBuffer genTerm(PLTerm t) {
    StringBuffer s = new StringBuffer(10);

    switch(currentVersion) {
    case VERSION_A:
      switch(t.Type) {
      case PLTerm.ATOM:
        s.append(PFX_ATOM);
        s.append(((PLAtom)t).Value + "\0");
        break;
        
      case PLTerm.INTEGER:
        s.append(PFX_LONG_INT);
        s.append(((PLInteger)t).Value + "\0");
        break;

      case PLTerm.FLOAT:
        s.append(PFX_FLOAT);
        s.append(((PLFloat)t).Value + "\0");
        break;
        
        //case PLTerm.NIL:
        //s.append(PFX_LIST_WITH_NIL_TAIL);
        //s.append("0\0");
        //break;
        
      case PLTerm.LIST:
        s.append(PFX_LIST_WITH_NIL_TAIL);
        s.append(((PLList)t).length());
        s.append('\0');
        PLList l = (PLList)t;
        while (l.getTail().Type == PLTerm.LIST)
          s.append(genTerm(l.getHead()));
        break;
        
      case PLTerm.STRUCTURE:
        PLTerm[] args = ((PLStructure)t).Args;
        s.append(PFX_STRUCT);
        s.append(((PLStructure)t).Name);
        s.append('\0');
        s.append(args.length);
        s.append('\0');
        for (int i = 0; i<args.length; i++)
          s.append(genTerm(args[i]));
        break;
        
      case PLTerm.VARIABLE:
        // @@@@@
        // OOJOJOJOJOJO: ESTO NO ESTA MUY BIEN.
        // DEBE PODER IDENTIFICAR VARIABLES MULTIRREFERENCIADAS.
        s.append(PFX_SINGLE_REF_VAR);
      }
      break;

    case VERSION_C:
      switch(t.Type) {
      case PLTerm.ATOM:
        if (t.equals(PLTerm.nil))
          s.append(PFXC_NIL);
        else {
          s.append(PFXC_ATOM);
          s.append(((PLAtom)t).Value + "\0");
        }
        break;
        
      case PLTerm.INTEGER:
        s.append(PFXC_INTEGER);
        s.append(((PLInteger)t).Value + "\0");
        break;

      case PLTerm.FLOAT:
        s.append(PFXC_FLOAT);
        s.append(((PLFloat)t).Value + "\0");
        break;
        
      case PLTerm.LIST:
        s.append(PFXC_LIST);
        s.append(genTerm(((PLList)t).getHead()));
        s.append(genTerm(((PLList)t).getTail()));
        break;
        
      case PLTerm.STRUCTURE:
        PLTerm[] args = ((PLStructure)t).Args;
        s.append(PFXC_STRUCT);
        s.append(((PLStructure)t).Name);
        s.append('\0');
        byte strArity[] = {(new Byte(new Integer(args.length).byteValue())).byteValue()};
        s.append(new String(strArity));
        for (int i = 0; i<args.length; i++)
          s.append(genTerm(args[i]));
        break;
        
      case PLTerm.VARIABLE:

	if (((PLVariable)t).isFree()) {
	  s.append(PFXC_VARIABLE);
	  s.append(((PLVariable)t).getNumber() + "\0");
	}
	else
	  s.append(genTerm(((PLVariable)t).getBinding()));
	break;

      case PLTerm.STRING:
        s.append(PFXC_STRING);
	s.append(((PLString)t).getValue() + "\0");
        s.append(PFXC_NIL);
        break;
        
      }
      break;
    }

    return s;
  }
}



package CiaoJava;

import java.awt.*;
import java.lang.reflect.*;
import java.io.*;
import java.util.*;

/**
 * class PLInterpreter
 * Interprets the terms received via $fast_write
 * and solves the requests:
 *  $java_create_object       object creation
 *  $java_delete_object       object deletion
 *  $java_get_value           field get value
 *  $java_set_value           field set value
 *  $java_invoke_method       method invocation
 *  $java_add_listener        predicate registering to object events
 *  $java_remove_listener     predicate removing from object events
 */
class PLInterpreter {
  private Hashtable objTable;
  private int objKey = 0;
  private static final int STARTING_CAPACITY = 16;
  private static final float FACTOR = 0.2f;
  //  private PLActionListener javaActionListener;
  private PLEventListener eventListener;

  // Functor names accepted from prolog.
  private static final String CREATE_OBJECT = "$java_create_object";
  private static final String DELETE_OBJECT = "$java_delete_object";
  private static final String GET_VALUE = "$java_get_value";
  private static final String SET_VALUE = "$java_set_value";
  private static final String INVOKE_METHOD = "$java_invoke_method";
  private static final String ADD_LISTENER = "$java_add_listener";
  private static final String REMOVE_LISTENER = "$java_remove_listener";
  private static final String QUIT = "$quit";
  private static final String WAIT_FOR_EVENTS = "$java_wait_for_events";

  /**
   * Creates a new interpreter, with the table of objects
   * managed by prolog given as argument.
   *
   * @param pl Object that represents the connection to the prolog process.
   */
  public PLInterpreter(PLConnection pl) {
    objTable = new Hashtable(STARTING_CAPACITY, FACTOR);
    //    javaActionListener = new PLActionListener();
    eventListener = new PLEventListener(pl, this);
  }

  /** 
   * Interprets a string received from prolog (in $fast format)
   * and runs the related tasks. Possible tasks are:
   *  $java_create_object(+NAME, +ARGUMENTS, -OBJECT)
   *  $java_delete_object(+OBJECT)
   *  $java_get_value(+OBJECT, +FIELD, -VALUE)
   *  $java_set_value(+OBJECT, +FIELD, +VALUE)
   *  $java_invoke_method(+OBJECT, +METHOD, +ARGUMENTS, -RESULT)
   *  $java_add_listener(+OBJECT, +EVENT, +PREDICATE, -RESULT)
   *  $java_remove_listener(+OBJECT, +EVENT, +PREDICATE, -RESULT)
   *  $quit
   *
   * @param t Prolog term to be interpreted. Are interpreted only the
   *          prolog terms related above.
   *
   * @return the term to be sent back to Prolog, corresponding
   *         to the returning value. If there is no return value,
   *         success atom is returned. If an error occurs, the fail
   *         atom is returned.
   */
  public PLTerm interpret(PLTerm t) {
    // Only for debug
    System.err.println(t.toString());

    switch (t.Type) {
    case PLTerm.STRUCTURE:
      PLStructure st = (PLStructure)t;
      if (st.Name.equals(CREATE_OBJECT)) {
        return createObject(st);
      } else if(st.Name.equals(DELETE_OBJECT)) {
        return deleteObject(st);
      } else if(st.Name.equals(GET_VALUE)) {
        return getValue(st);
      } else if(st.Name.equals(SET_VALUE)) {
        return setValue(st);
      } else if(st.Name.equals(INVOKE_METHOD)) {
        return invokeMethod(st);
      } else if(st.Name.equals(ADD_LISTENER)) {
        return addListener(st);
      } else if(st.Name.equals(REMOVE_LISTENER)) {
        return removeListener(st);
      } else {
        System.err.println("error: unexpected structure received");
        return PLAtom.fail;
      }

    case PLTerm.ATOM:
      PLAtom at = (PLAtom)t;
      if (at.Value.equals(QUIT)) {
        return new PLAtom(QUIT);
      } else {
        System.err.println("error: unexpected atom received");
        return PLAtom.fail;
      }
    default:
      System.err.println("error: unexpected term received");
      return PLAtom.fail;
    }
  }

  /**
   * Gets the value of a java object field.
   * Evaluates the '$java_get_value' prolog term, given as argument.
   *
   * @param st Prolog structure that contains the data needed for this
   *           operation: object reference and field name.
   *
   * @return   The prolog representation of the field value requested,
   *           if the command succeeds;
   *           the prolog representation of fail if the command
   *           does not succeed.
   */
  private PLTerm getValue(PLStructure st) {
    // Input arguments test.
    if (st.Args.length != 2) {
      System.err.println("error($java_get_value): number of arguments");
      return PLAtom.fail;
    }          
    if ((st.Args[0].Type != PLTerm.STRUCTURE) &&
        (st.Args[0].Type != PLTerm.ATOM)) {
      System.err.println("error($java_get_value): object reference");
      return PLAtom.fail;
    }
    if (st.Args[1].Type != PLTerm.ATOM) {
      System.err.println("error($java_get_value): Field name reference");
      return PLAtom.fail;
    }

    Class cl = null;
    Object obj = null;
    if (st.Args[0].Type == PLTerm.ATOM) {
      // Static field. Java object will be null, and the 
      // first argument is the class name.
      try {
        cl = Class.forName(st.Args[0].toString());
      } catch(Exception e) {
        System.err.println("error($java_get_value): class not found");
        return PLAtom.fail;
      }
    }
    else {
      // Instance field.
      PLStructure str = (PLStructure)st.Args[0];
      if (!str.Name.equals(PLTerm.JAVA_OBJECT) || str.Arity != 1) {
        System.err.println("error($java_get_value): object structure");
        return PLAtom.fail;
      }
      obj = str.javaRepr(this);
      if (obj == null) {
        System.err.println("error($java_get_value): Object not found");
        return PLAtom.fail;
      }
      cl = obj.getClass();
    }
    
    // Field value return
    try {
      Field fl = cl.getField((String)st.Args[1].javaRepr(this));
      return prologRepr(fl.get(obj));
    } catch (Exception e) {
      System.err.println("error($java_get_value): Field value");
      return PLAtom.fail;
    }
  }

  /**
   * Sets the value of a java object field.
   * Evaluates the '$java_set_value' prolog term, given as argument.
   *
   * @param st Prolog structure that contains the data needed for this
   *           operation: object reference, field name and field value.
   *
   * @return   the prolog representation of success if the command
   *           succeeds;
   *           the prolog representation of fail if the command
   *           does not succeed.
   */
  private PLTerm setValue(PLStructure st) {
    // Input arguments test.
    if (st.Args.length != 3) {
      System.err.println("error($java_set_value): number of arguments");
      return PLAtom.fail;
    }          
    if ((st.Args[0].Type != PLTerm.STRUCTURE)) {
          System.err.println("error($java_set_value): object reference");
          return PLAtom.fail;
    }
    PLStructure str = (PLStructure)st.Args[0];
    if (!str.Name.equals(PLTerm.JAVA_OBJECT) || str.Arity != 1) {
      System.err.println("error($java_set_value): object structure");
      return PLAtom.fail;
        }
    if (st.Args[1].Type != PLTerm.ATOM) {
      System.err.println("error($java_set_value): Field name reference");
      return PLAtom.fail;
        }
    Object obj = str.javaRepr(this);
    if (obj == null) {
      System.err.println("error($java_set_value): Object not found");
      return PLAtom.fail;
    }
    Object val = st.Args[2].javaRepr(this);
    
    // Field value change.
    try {
      Class cl = obj.getClass();
      Field fl = cl.getField((String)st.Args[1].javaRepr(this));
      fl.set(obj,val);
      return PLAtom.success;
    } catch (Exception e) {
      System.err.println("error($java_get_value): Field value");
      return PLAtom.fail;
    }
  }

  /**
   * Deletes a java object and removes it from the object table.
   * Evaluates the '$java_delete_object' prolog term, given as argument. 
   *
   * @param st Prolog structure that contains the data needed for this
   *           operation: object reference.
   *
   * @return   the prolog representation of success if the command
   *           succeeds;
   *           the prolog representation of fail if the command
   *           does not succeed.
   */
  private PLTerm deleteObject(PLStructure st) {
    // Input arguments test.
    if (st.Args.length != 1) {
      System.err.println("error($java_delete_object): number of arguments");
      return PLAtom.fail;
    }          
    if ((st.Args[0].Type != PLTerm.STRUCTURE)) {
      System.err.println("error($java_delete_object): object reference");
      return PLAtom.fail;
    }
    PLStructure str = (PLStructure)st.Args[0];
    if (!str.Name.equals(PLTerm.JAVA_OBJECT) || str.Arity != 1) {
      System.err.println("error($java_delete_object): object structure");
      return PLAtom.fail;
    }
    
    // Object deletion.
    Integer obj = (Integer)str.Args[0].javaRepr(this);
    Object value = null;
    try {
      value = objTable.remove(obj);
    } catch (Exception e) {
      System.err.println("error($java_delete_object): object not found");
      return PLAtom.fail;
    }
    if (value == null) {
      System.err.println("error($java_delete_object): object not found");
      return PLAtom.fail;
    } else
      return PLAtom.success;
  }

  /**
   * Creates a java object and adds it to the object table.
   * Evaluates the '$java_create_object' prolog term, given as argument.
   *
   * @param st Prolog structure that contains the data needed for this
   *           operation: class name and constructor argument list.
   *
   * @return   the prolog representation of the java object if the command
   *           succeeds;
   *           the prolog representation of fail if the command
   *           does not succeed.
   */
  private PLTerm createObject(PLStructure st) {
    // Input arguments test.
    if (st.Args.length != 2) {
      System.err.println("error($java_create_object): number of arguments");
      return PLAtom.fail;
    }          
    if (st.Args[0].Type != PLTerm.ATOM) {
      System.err.println("error($java_create_object): object name");
      return PLAtom.fail;
    }
    if (!st.Args[1].isList() && !st.Args[1].isNil() && !st.Args[1].isString()){
      System.err.println("error($java_create_object): no list argument");
      return PLAtom.fail;
    }
    
    // Class creation.
    Class cl = null;
    try {
      cl = Class.forName(st.Args[0].toString());
    } catch(Exception e) {
      System.err.println("error($java_create_object): class not found");
      return PLAtom.fail;
    }
    
    // Object creation & insertion in the object table.
    Object newobj = null;

    try {
      if (st.Args[1].Type == PLTerm.LIST) {
        PLList arg = (PLList)st.Args[1];
        Object[] objarg = (Object[])arg.javaRepr(this);
        Class[] clsarg = new Class[objarg.length];
        for (int i = 0; i < clsarg.length; i++)
          clsarg[i] = objarg[i].getClass();
        Constructor cn = getConstructor(cl, (Class[])clsarg);
        newobj = cn.newInstance(objarg);
        objTable.put(new Integer(newobj.hashCode()),newobj);

      } else if (st.Args[1].isString()) {
	// If the argument list is a string, then it must be
	// translated to an array of bytes (currently integers).
	byte[] arg = ((PLString)st.Args[1]).toString().getBytes();
	Object[] objarg = new Object[arg.length];
	Class[] clsarg = new Class[objarg.length];
	for (int i = 0; i < arg.length; i++) {
	  objarg[i] = (Object)(new Integer(arg[i]));
	  clsarg[i] = Integer.TYPE;
	}
        Constructor cn = getConstructor(cl, (Class[])clsarg);
        newobj = cn.newInstance(objarg);
        objTable.put(new Integer(newobj.hashCode()),newobj);

      } else {
        Object objarg[] = {};
        Class clsarg[] = {};
        Constructor cn = getConstructor(cl, (Class[])(clsarg));
        newobj = cn.newInstance(objarg);
        objTable.put(new Integer(newobj.hashCode()),newobj);
      }

    } catch (Exception e) {
      System.err.println("error($java_create_object): creating object");
      return PLAtom.fail;
    }
    
    return java_object(newobj.hashCode());
  }

  /**
   * Invokes a java object method.
   * Evaluates the '$java_invoke_method' prolog term, given as argument.
   *
   * @param st Prolog structure that contains the data needed for this
   *           operation: object reference, method name and argument list.
   *
   * @return   the prolog representation of the value returned by the java
   *           method call if the command succeeds;
   *           the prolog representation of success if the method
   *           invocation does not return a value;
   *           the prolog representation of fail if the command
   *           does not succeed.
   */
  private PLTerm invokeMethod(PLStructure st) {
    // Input arguments test.
    if (st.Args.length != 3) {
      System.err.println("error($java_invoke_method): number of arguments");
      return PLAtom.fail;
    }
    if ((st.Args[0].Type != PLTerm.STRUCTURE) &&
        (st.Args[0].Type != PLTerm.ATOM)) {
      System.err.println("error($java_invoke_method): object/class reference");
      return PLAtom.fail;
    }
    if (st.Args[1].Type != PLTerm.ATOM) {
      System.err.println("error($java_invoke_method): method name");
      return PLAtom.fail;
    }
    if (!st.Args[2].isList() && !st.Args[2].isNil() && !st.Args[2].isString()){
      System.err.println("error($java_invoke_method): no list argument");
      return PLAtom.fail;
    }

    Class cl = null;
    Object obj = null;
    if (st.Args[0].Type == PLTerm.ATOM) {
      // Static method invocation. Java object will be null, and the 
      // first argument is the class name.
      try {
        cl = Class.forName(st.Args[0].toString());
      } catch(Exception e) {
        System.err.println("error($java_invoke_method): class not found");
        return PLAtom.fail;
      }
    }
    else {
      // Instance method invocation.
      PLStructure str = (PLStructure)st.Args[0];
      if (!str.Name.equals(PLTerm.JAVA_OBJECT) || str.Arity != 1) {
        System.err.println("error($java_invoke_method): object structure");
        return PLAtom.fail;
      }
      obj = str.javaRepr(this);
      if (obj == null) {
        System.err.println("error($java_invoke_method): java object not found");
        return PLAtom.fail;
      }
      cl = obj.getClass();
    }
    
    // Method invocation.
    try {
      String mtName = (String)st.Args[1].javaRepr(this);
      Object result = null;

      if (st.Args[2].isList()) {
        PLList arg = (PLList)st.Args[2];
        Object[] objarg = (Object[])arg.javaRepr(this);
        Class clsarg[] = new Class[objarg.length];
        for (int i = 0; i < clsarg.length; i++)
          clsarg[i] = objarg[i].getClass();
        Method mt = getMethod(cl, mtName, clsarg);
        result = invoke(mt, obj, objarg);

      } else if (st.Args[2].isString()) {
	// If the argument list is a string, then it must be
	// translated to an array of bytes (currently integers).
	byte[] arg = ((PLString)st.Args[2]).toString().getBytes();
	Object[] objarg = new Object[arg.length];
	Class[] clsarg = new Class[objarg.length];
	for (int i = 0; i < arg.length; i++) {
	  objarg[i] = (Object)(new Integer(arg[i]));
	  clsarg[i] = Integer.TYPE;
	}
        Method mt = getMethod(cl, mtName, clsarg);
	result = invoke(mt, obj, objarg);

      } else {
        Class clsarg[] = {};
        Method mt = cl.getMethod(mtName, clsarg);
        result = mt.invoke(obj, new Object[0]);
      }

      if (result == null)
        return PLAtom.success;
      else
        return prologRepr(result);

    } catch(Exception e) {
      System.err.println("error($java_invoke_method): method not found");
      return PLAtom.fail;
    }
  }

  /**
   * Adds the java object event to the listener predicate.
   * Evaluates the '$java_add_listener' prolog term, given
   * as argument.
   *
   * @param st Prolog structure that contains the data needed for this
   *           operation: object reference, event class name and 
   *           exception handler goal.
   *
   * @return   the prolog representation of success if the command
   *           succeeds;
   *           the prolog representation of fail if the command
   *           does not succeed.
   */
  private PLTerm addListener(PLStructure st) {
    // Input arguments test.
    if (st.Args.length != 3) {
      System.err.println("error($java_add_listener): number of arguments");
      return PLAtom.fail;
    }
    if ((st.Args[0].Type != PLTerm.STRUCTURE)) {
      System.err.println("error($java_add_listener): object reference");
      return PLAtom.fail;
    }
    PLStructure str = (PLStructure)st.Args[0];
    if (!str.Name.equals(PLTerm.JAVA_OBJECT) || str.Arity != 1) {
      System.err.println("error($java_add_listener): object structure");
      return PLAtom.fail;
    }
    Object obj = str.javaRepr(this);
    if (obj == null) {
      System.err.println("error($java_add_listener): java object not found");
      return PLAtom.fail;
    }
    if (st.Args[1].Type != PLTerm.ATOM) {
      System.err.println("error($java_add_listener): event class");
      return PLAtom.fail;
    }

    Class event = null;
    try {
      event = Class.forName(st.Args[1].toString());
    } catch(Exception e) {
      System.err.println("error($java_add_listener): event class not found");
      return PLAtom.fail;
    }

    // Assign listener and listen.
    eventListener.addListener(obj, event, st.Args[2]);
    return PLAtom.success;
  }

  /**
   * Removes the java object event from the listener predicate.
   * Evaluates the '$java_remove_listener' prolog term, given
   * as argument.
   *
   * @param st Prolog structure that contains the data needed for this
   *           operation: object reference, event class name and 
   *           exception handler goal.
   *
   * @return   the prolog representation of success if the command
   *           succeeds;
   *           the prolog representation of fail if the command
   *           does not succeed.
   */
  private PLTerm removeListener(PLStructure st) {
    // Input arguments test.
    if (st.Args.length != 3) {
      System.err.println("error($java_remove_listener): number of arguments");
      return PLAtom.fail;
    }
    if ((st.Args[0].Type != PLTerm.STRUCTURE)) {
      System.err.println("error($java_remove_listener): object reference");
      return PLAtom.fail;
    }
    PLStructure str = (PLStructure)st.Args[0];
    if (!str.Name.equals(PLTerm.JAVA_OBJECT) || str.Arity != 1) {
      System.err.println("error($java_remove_listener): object structure");
      return PLAtom.fail;
    }
    Object obj = str.javaRepr(this);
    if (obj == null) {
      System.err.println("error($java_remove_listener): java object not found");
      return PLAtom.fail;
    }
    if (st.Args[1].Type != PLTerm.ATOM) {
      System.err.println("error($java_remove_listener): event class");
      return PLAtom.fail;
    }

    Class event = null;
    try {
      event = Class.forName(st.Args[1].toString());
    } catch(Exception e) {
      System.err.println("error($java_remove_listener): event class not found");
      return PLAtom.fail;
    }

    // Assign listener and listen.
    eventListener.removeListener(obj, event, st.Args[2]);
    return PLAtom.success;
  }

  /** 
   * Gets the prolog representation of the object argument.
   *
   * @param v java object from which a prolog representation will
   *          be obtained.
   *
   * @return  the prolog term that represents the java object
   *          received as argument.
   */
  private PLTerm prologRepr(Object v) {

    if (v.getClass().isArray()){
      // Is an array: returns a list of terms.

      Object[] va = (Object [])v;
      PLTerm list = PLTerm.nil;
      if (va.length > 0) {
        list = new PLList(prologRepr(va[0]),PLTerm.nil);
        for (int i = 1; i < va.length; i++)
          ((PLList)list).add(prologRepr(va[i]));
      }
      return list;
    }

    if (v instanceof Integer)
      return new PLInteger(((Integer)v).intValue());
    
    if (v instanceof Short)
      return new PLInteger(((Short)v).intValue());

    if (v instanceof Long)
      return new PLInteger(((Long)v).intValue());

    if (v instanceof Byte)
      return new PLInteger(((Byte)v).intValue());

    if (v instanceof String)
      return new PLString(((String)v).toString());

    if (v instanceof Character)
      return new PLString(((Character)v).toString());

    if (v instanceof Float)
      return new PLFloat(((Float)v).floatValue());

    if (v instanceof Double)
      return new PLFloat(((Double)v).doubleValue());

    if (v instanceof Boolean)
      return new PLString(((Boolean)v).toString());
  
    if ((v instanceof PLTerm) ||
        (v instanceof PLAtom) ||
        (v instanceof PLInteger) ||
        (v instanceof PLFloat) ||
        (v instanceof PLList) ||
        (v instanceof PLStructure) ||
        (v instanceof PLVariable))
      return (PLTerm)v;

    // The argument v is a java object. Must be returned
    // its prolog representation.
    objTable.put(new Integer(v.hashCode()),v);
    return java_object(v.hashCode());
    
  }

  /** 
   * Prolog representation of a java object in the object table.
   *
   * @param i hash code in the object table.
   *
   * @return  a new <code>PLTerm</code> object that references 
   *          the object referenced by the hash code.
   */
  private PLTerm java_object(int i) {
    PLTerm arg[] = {new PLInteger(i)};

    return new PLStructure(PLTerm.JAVA_OBJECT, 1, arg);

  }

//   /** 
//    * Gets the method of the class cl that matches the
//    * parameter type object array. Considers superclass
//    * and interface matching recursively on the argument
//    * list.
//    *
//    * @param cl     <code>Class</code> object that represents the
//    *               object class.
//    * @param mtName Method name.
//    * @param clsarg Array of <code>Class</code> objects that 
//    *               represents the method argument list.
//    *
//    * @return       the <code>Method</code> object that represents
//    *               the method found. If there is no method in the
//    *               inheritance tree, returns null.
//    */
//   private Method getMethod(Class cl, String mtName, Class[] clsarg) {
//     return getSuperclassMethod(cl, mtName, clsarg, 0);
//   }

//   /**
//    * Recursive method search. See the previous getMethod method.
//    *
//    * @param cl     <code>Class</code> object that represents the
//    *               object class.
//    * @param mtName Method name.
//    * @param clsarg Array of <code>Class</code> objects that 
//    *               represents the method argument list.
//    * @param pos    Element of argument list being tested.
//    *               Used for recursion.
//    *
//    * @return       the <code>Method</code> object that represents
//    *               the method found. If there is no method in the
//    *               inheritance tree, returns null.
//    */
//   private Method getSuperclassMethod(Class cl, String mtName, 
//                                      Class[] clsarg, int pos) {
//     boolean found = false;
//     Method mt = null;
//     Class oldClass = clsarg[pos];
//     Class intf[];

//     do {
//       found = false;
//       if (pos + 1 < clsarg.length) {
//         // Searching the rest of the array of parameter types.
//         mt = getSuperclassMethod(cl, mtName, clsarg, pos + 1);
//         if (mt != null)
//           found = true;
//       }
//       else {
//         // Last parameter in the array: Trying to get the method object.
//         try {
//           found = true;
//           mt = cl.getMethod(mtName, clsarg);
//         } catch (Exception e) {
//           found = false;
//         }
//       }

//       clsarg[pos] = clsarg[pos].getSuperclass();
//     } while (clsarg[pos] != null && !found);


//     // Searching the interface arrays that implements
//     // the class and its ancestors.
//     Class ancClass = oldClass;
//     do {
//       intf = ancClass.getInterfaces();
//       for (int i = 0; i < intf.length && !found; i++) {
//         clsarg[pos] = intf[i];
//         mt = getSuperclassMethod(cl, mtName, clsarg, pos);
//         if (mt != null)
//           found = true;
//       }
//       ancClass = ancClass.getSuperclass();
//     } while (ancClass != null && !found);

//     // Restore the old value of the array component.
//     clsarg[pos] = oldClass;

//     if (found)
//       return mt;
//     else
//       return null;
//   }

  /** 
   * Gets the method of the class cl that matches the
   * parameter type object array. Considers superclass
   * and interface matching using the <code>isAssignableFrom()</code>
   * method (of the <code>Class</code> class).
   *
   * The main concern about searching the method to be launched for
   * a given object, method name, and parameter types is to choose 
   * from method candidates that are compatible with the parameter types.
   * In most of cases will be no possibility of discussion, only
   * one method fits the number and parameter types.
   * The problem raises when given the parameter type array, several
   * methods could be called, mainly because the type correspondence
   * between java and prolog is not perfect.
   *
   * This method uses the concept of 'distance' to select the best
   * fit for a given parameter array. 
   *
   * @param cl     <code>Class</code> object that represents the
   *               object class.
   * @param mtName Method name.
   * @param clsarg Array of <code>Class</code> objects that 
   *               represents the method argument list.
   *
   * @return       the <code>Method</code> object that represents
   *               the method found. If there is no method in the
   *               inheritance tree, returns null.
   */
  private Method getMethod(Class cl, String mtName, Class[] clsarg) {

    Method mt = null;
    int minDist = Integer.MAX_VALUE; // minimum distance.

    /*
     * First try with a perfect fit: the parameter types correspond
     * exactly with the method formal parameters.
     */
    try {
      mt = cl.getMethod(mtName, clsarg);
      return mt;
    } catch (Exception e) {}

    /*
     * There is no perfect fit; from the method list, the candidates
     * are chosen and determined their distance. The method with
     * minimum distance is selected.
     */
    Method mts[] = cl.getMethods();
    for (int i = 0; i < mts.length; i++) 
      if (mts[i].getName().equals(mtName)) {
	Class clsfor[] = mts[i].getParameterTypes();
	if (clsfor.length == clsarg.length) {
	  int distance = 0;
	  boolean compatible = true;
	  for (int j = 0; j < clsarg.length; j++)
	    if (!clsfor[j].isAssignableFrom(clsarg[j])) {
	      compatible = false;
	      break;
	    }
	    else
	      /*
	       * Distance Calculation
	       */
	      distance += getDistance(clsfor[j], clsarg[j]);

	  if (compatible && distance < minDist) {
	    minDist = distance;
	    mt = mts[i];
	  }
	}
      }

    if (minDist < Integer.MAX_VALUE)
      return mt;
    else
      return null;
  }

  /** 
   * Gets the constructor of the class cl that matches the
   * parameter type object array. Considers superclass
   * and interface matching of the parameters using the
   * <code>isAssignableFrom()</code> method (of the <code>Class</code> class).
   * The algorithm is exactly the same as the used in getMethod.
   * See this method to get detailed information.
   *
   * @param cl     <code>Class</code> object that represents the
   *               object class.
   *
   * @param clsarg Array of <code>Class</code> objects that 
   *               represents the constructor argument list.
   *
   * @return       the <code>Constructor</code> object that represents
   *               the constructor found. If there is no constructor in the
   *               inheritance tree, returns null.
   */
  private Constructor getConstructor(Class cl, Class[] clsarg) {
    Constructor cn = null;
    int minDist = Integer.MAX_VALUE; // minimum distance.

    /*
     * First try with a perfect fit: the parameter types corresponds
     * exactly with the constructor formal parameters.
     */
    try {
      cn = cl.getConstructor(clsarg);
      return cn;
    } catch (Exception e) {}

    /*
     * There is no perfect fit; from the constructor list, the candidates
     * are chosen and determined their distance. The constructor with
     * minimum distance is selected.
     */
    Constructor cns[] = cl.getConstructors();
    for (int i = 0; i < cns.length; i++) {
      Class clsfor[] = cns[i].getParameterTypes();
      if (clsfor.length == clsarg.length) {
	  int distance = 0;
	  boolean compatible = true;
	  for (int j = 0; j < clsarg.length; j++)
	    if (!clsfor[j].isAssignableFrom(clsarg[j])) {
	      compatible = false;
	      break;
	    }
	    else
	      /*
	       * Distance Calculation
	       */
	      distance += getDistance(clsfor[j], clsarg[j]);

	  if (compatible && distance < minDist) {
	    minDist = distance;
	    cn = cns[i];
	  }
	}
      }

    if (minDist < Integer.MAX_VALUE)
      return cn;
    else
      return null;
  }

  /**
   * Gets a measure of the 'distance' between two assignable
   * classes. In order to choose the nearest fit from a list
   * of java types to a list of methods or constructors, is
   * needed to use an heuristic based on the distance between
   * types.
   *
   * Is supossed that the java classes received as arguments are
   * compatible using the <code>isAssignableFrom</code> method of the
   * <code>Class</code> class.
   *
   * Is supossed that the <code>assignedFrom</code> parameter will not be
   * primitive.
   *
   * @param assignedTo    <code>Class</code> object that represents the
   *                      java type to be assigned.
   *
   * @param assignedFrom  <code>Class</code> object that represents the
   *                      java type from which will be made the
   *                      assignment.
   *
   * @return              an <code>int</code> number representing the
   *                      'distance' between the classes received as
   *                      argument.
   */
  private int getDistance(Class assignedTo, Class assignedFrom) {

    int distance;

    /*
     * If any of the classes is null, the distance is infinite
     * (represented as the maximum int.
     */
    if (assignedTo == null ||
	assignedFrom == null)
      return Integer.MAX_VALUE;

    /*
     * If both classes are equal, or the assignedTo class is primitive
     * and the assignedFrom class is the corresponding class,
     * the distance is 0.
     */
    if (assignedTo.equals(assignedFrom))
      return 0;

    Class newAssignedTo = null;
    try {
      Class Int = Class.forName("java.lang.Integer");
      Class Bln = Class.forName("java.lang.Boolean");
      Class Chr = Class.forName("java.lang.Character");
      Class Byt = Class.forName("java.lang.Byte");
      Class Sht = Class.forName("java.lang.Short");
      Class Lng = Class.forName("java.lang.Long");
      Class Flt = Class.forName("java.lang.Float");
      Class Dbl = Class.forName("java.lang.Double");

      if (assignedTo.isPrimitive()) {
	if (assignedTo.equals(Boolean.TYPE))
	  newAssignedTo = Bln;
	else if (assignedTo.equals(Character.TYPE))
	  newAssignedTo = Chr;
	else if (assignedTo.equals(Byte.TYPE))
	  newAssignedTo = Byt;
	else if (assignedTo.equals(Short.TYPE))
	  newAssignedTo = Sht;
	else if (assignedTo.equals(Integer.TYPE))
	  newAssignedTo = Int;
	else if (assignedTo.equals(Long.TYPE))
	  newAssignedTo = Lng;
	else if (assignedTo.equals(Float.TYPE))
	  newAssignedTo = Flt;
	else if (assignedTo.equals(Double.TYPE))
	  newAssignedTo = Dbl;

	return getDistance(newAssignedTo, assignedFrom);
      }

      /*
       * Several specific types derived from primitive types are
       * treated separatedly, because some types can be contained
       * in other types, but cannot be identified as superclasses
       * nor superinterfaces.
       */
      if ((assignedTo.equals(Dbl) ||
	   assignedTo.equals(Flt)) &&
	  (assignedFrom.equals(Dbl) ||
	   assignedFrom.equals(Flt)))
	return 1;

      if ((assignedTo.equals(Lng) ||
	   assignedTo.equals(Int) ||
	   assignedTo.equals(Sht) ||
	   assignedTo.equals(Byt)) &&
	  (assignedFrom.equals(Lng) ||
	   assignedFrom.equals(Int) ||
	   assignedFrom.equals(Sht) ||
	   assignedFrom.equals(Byt)))
	return 1;

    } catch (ClassNotFoundException e) {}

    /*
     * If the assignedFrom class is a superinterface or superclass of
     * the assignedTo class, the distance is the minimum number of nodes
     * between both classes in the inheritance tree.
     * A breadth-first search is used.
     */
    distance = getDistance(assignedTo.getSuperclass(), assignedFrom);
    for (int i = 0 ; i < assignedTo.getInterfaces().length ; i++) {
      int dist = getDistance(assignedTo.getInterfaces()[i], assignedFrom);
      if (dist < distance)
	distance = dist;
    }
    return distance + 1;

  }

    /**
     * Invokes the method received as first argument on the object 
     * received as second argument, with the arguments of the third
     * argument. The method <code>invoke</code> of the class
     * <code>Method</code> cannot be used directly because the argument
     * types could not correspond exactly to the method ones. A kind
     * of conversion must be done with the primitive java types.
     *
     * @param mt  <code>Method</code> to be invoked.
     * @param obj Object on which the method will be invoked.
     * @param arg List of argument values to invoke the method.
     *
     * @exception <code>Exception</code> if there is any problem invoking
     *            the method.
     **/
    private Object invoke(Method mt, Object obj, Object arg[])
	throws Exception {
System.err.println("::" + mt);
	/*
	 * Conversion of the arguments.
	 */
	for (int i = 0; i < arg.length ; i++)
	    if (mt.getParameterTypes()[i].isPrimitive())
		arg[i] = translateArg(arg[i], mt.getParameterTypes()[i]);

	/*
	 * Method invocation.
	 */
for (int i = 0; i < arg.length ; i++)
System.err.println("arg[]=" + arg[i]);

	return mt.invoke(obj, arg);
    }

    /**
     * Argument translation for method/constructor invocation. Translates
     * one argument making the type casting needed by the method/constructor.
     *
     * @param arg  Argument value received for method/constructor invocation.
     * @param type Primitive type needed by the method/constructor.
     *
     * @return The argument value with the proper type, if there is any;
     *         <code>null</code> if there is no possible conversion.
     **/
    private Object translateArg(Object arg, Class type) {

	Object trArg = null;

	/*
	 * target type must be primitive.
	 */
	if (!type.isPrimitive())
	    return null;

	/*
	 * Argument value must be a number object representation.
	 */
	if (arg instanceof Number) {
	    if (type.equals(Byte.TYPE))
		trArg = (Object)(new Byte(((Number)arg).byteValue()));
	    else if (type.equals(Short.TYPE))
		trArg = (Object)(new Short(((Number)arg).shortValue()));
	    else if (type.equals(Integer.TYPE))
		trArg = (Object)(new Integer(((Number)arg).intValue()));
	    else if (type.equals(Long.TYPE))
		trArg = (Object)(new Long(((Number)arg).longValue()));
	    else if (type.equals(Float.TYPE))
		trArg = (Object)(new Float(((Number)arg).floatValue()));
	    else if (type.equals(Double.TYPE))
		trArg = (Object)(new Double(((Number)arg).doubleValue()));
	} else
	    trArg = arg;

	return trArg;
    }	

  /** 
   * Returns true if the term received as argument
   * can be interpreted by the prolog term interpreter.
   *
   * @param t Term to be tested for interpretation.
   *
   * @return  <code>true</code> if the prolog term received
   *          as argument can be interpreted by the prolog
   *          command interpreter (method interpret above).
   */
  public static boolean isInterpretable(PLTerm t) {
    switch (t.Type) {
    case PLTerm.STRUCTURE:
      PLStructure st = (PLStructure)t;
      if (st.Name.equals(CREATE_OBJECT) ||
          st.Name.equals(DELETE_OBJECT) ||
          st.Name.equals(GET_VALUE) ||
          st.Name.equals(SET_VALUE) ||
          st.Name.equals(INVOKE_METHOD) ||
          st.Name.equals(ADD_LISTENER) ||
          st.Name.equals(REMOVE_LISTENER))
        return true;
      else
        return false;

    case PLTerm.ATOM:
      PLAtom at = (PLAtom)t;
      if (at.Value.equals(QUIT))
        return true;
      else
        return false;
    default:
      return false;
    }
  }

  /**
   * Gets the object from the object table given the hash code.
   *
   * @param hashCode Hash code that references an object
   *                 in the java object table.
   *
   * @return         a java object referenced by the hash code.
   */
  public Object getObject(Integer hashCode) {
    return objTable.get(hashCode);    
  }
}




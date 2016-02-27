:- module(javart, [ 
        java_create_object/2,
        machine_name/1,
	java_constructor/1,
	java_object/1,
        java_delete_object/1, 
        java_invoke_method/2, 
	java_method/1,
        java_get_value/2,
        java_set_value/2,
	java_field/1,
        java_connection/0,
	java_remote_connection/1,
        java_disconnection/0,
        java_add_listener/3,
	java_event/1,
	prolog_goal/1,
        java_remove_listener/3
	],

	[assertions,regtypes,isomodes]).

:- comment(title,"Low-level Prolog to Java interface").

:- comment(author,"Jes@'{u}s Correas").

:- comment(module,"

This module defines the low level Prolog to Java interface. This interface
allows a Prolog program to start a Java process, and create Java objects,
invoke methods, set/get attributes (fields), and handle Java events.

Although the Java side interface is explained in Javadoc format, the
general interface structure is detailed here.

@section{Low-Level Prolog to Java Interface Structure}
@cindex{Low-Level Prolog to Java Interface Structure}
This low-level prolog to java interface is made up of two parts: a Prolog
part and a Java part. The Prolog part receives requests from a Prolog
program and sends them to the Java part through a socket. The Java part
receives requests from the socket and performs the actions included in the
requests.

If an event is thrown in the java side, an asynchronous message must be
sent away to the prolog side, in order to launch a prolog goal to handle
the event. This asynchronous communication is made by the means of
a second socket. The nature of this communication needs the use of threads
both in java and prolog: one thread to deal with the 'sequential program
flow,' and other thread (may be several) to do the job of event handling.

In the java side the threads are automatically created by the context of
the objects we use: adding an event listener implies automatically that
there will be a thread ready to launch it when the event raises.  The
prolog side is different: there must be a thread in the low-level interface
that listens to the asynchronous socket to launch the goals requested.

@subsection{Prolog side}
@cindex{Low-Level Prolog to Java Interface Structure. Prolog side}
The prolog side receives the actions to do in the
java side from the user program, and sends them to the java side through the socket connection.
When the action is done in the java side, the result is returned to the user
program, or the action fails if any problem in the java side is found.

In order to send and receive prolog terms and java object references using a socket,
this layer must transform this elements in a serialized representation,
just like the java serialization package does. This transformation is done in
our implementation using the @tt{fast\_read/1} and @tt{fast\_write/1} predicates
included in Ciao, so any prolog element can be translated to and from a
list of bytes.

Prolog data representation of java elements is very simple in this
low-level interface. Java primitive types such as integers and characters
are translated into Prolog terms, and even some Java objects are translated
that way (e. g. Java strings). Java objects are represented in Prolog as
compound terms with a reference to identify the corresponding Java
object. Data conversion is made automatically when the interface is used,
so the Prolog user programs do not have to deal with the complexity of this
tasks. 

@subsection{Java side}
@cindex{Low-Level Prolog to Java Interface Structure. Java side}
The java side of this layer is more complex than the prolog side. The tasks
this part have to deal to are the following:

@begin{itemize}
  
@item Wait for requests from the prolog side.

@item Translate the prolog terms received in a 'serialized' form in a more
         useful java representation.

@item Interpret the requests received from the prolog side.

@item Handle the set of objects created by or derived from the requests
        received from de prolog side.

@item Handle the events raised in the java side, and launch the listeners
        added in the prolog side.

@item Handle the exceptions raised in the java side, and send to the prolog
side.

@end{itemize}

In the implementation of the java side, two items must be carefully
designed: the handling of java objects, and the representation of prolog
data structures. The last item is specially important because all the
interactions between prolog and java are made using prolog structures, an
easy way to standardize the different data management of both
languages. Even the requests themselves are encapsulated using prolog
structures.  The overload of this encapsulation is not significant in terms
of socket traffic, due to the optimal implementation of the prolog serialized term.

The java side must handle the objects created from the prolog side
dinamically, and these objects must be accessed as fast as possible from
the set of objects. The java API provides a powerful implementation of Hash
tables that achieves all the requirements of our implementation.

On the other hand, the java representation of prolog terms is made using
the inheritance of java classes. In the java side exists a representation
of a generic prolog term, implemented as an abstract class in
java. Variables, atoms, compound terms, lists, and numeric terms are
classes in the java side which inherit from the term class. Java objects can
be seen also under the prolog representation as compound terms, where the
single argument corresponds to the Hash key of the actual java object in
the Hash table referred to before. This behaviour makes the handling of mixed
java and prolog elements easy. Prolog goals are represented in the java
side as objects which contain a prolog compound term with the term
representing the goal. This case will be seen more in depth next, when the java to
prolog is explained. 

@section{Java event handling from Prolog}
@cindex{Java event handling from Prolog}
Java event handling is based on a delegation model since version
1.1.x. This approach to event handling is very powerful and elegant, but a
user program cannot handle all the events that can raise on a given object:
for each kind of event, a listener must be implemented and added
specifically. However, the Java 2 API includes a special listener
(@tt{AWTEventListener}) that can manage the internal java event queue.

The prolog to java interface has been designed to emulate the java event
handler, and is also based on event objects and listeners. The low level
prolog to java interface implements its own event manager, to handle those
events that have prolog listeners associated to the object that raises the
event. From the prolog side can be added listeners to objects for specific
events. The java side includes a list of goals to launch from the object
and event type.

Due to the events nature, the event handler must work in a separate thread
to manage the events asynchronously. The java side has its own mechanisms
to work this way. The prolog side must be implemented specially for event
handling using threads. The communication between java and prolog is also
asynchronous, and a additional socket stream is used to avoid interferences
with the main socket stream. The event stream will work in this
implementation only in one way: from java to prolog. If an event handler
needs to send back requests to java, it will use the main socket stream, just
like the requests sent directly from a prolog program.

The internal process of register a Prolog event handler to a Java event is
shown in the next figure:

@image{ip2jbn-events-pl-reg}

When an event raises, the low-level Prolog to Java interface have to send to the Prolog user program the goal to evaluate. Graphically, the complete process  takes the tasks involved in the following figure:

@image{ip2jbn-events-pl-fire}


@section{Java exception handling from Prolog}
@cindex{Java exception handling from Prolog}
Java exception handling is very similar to the peer prolog handling:
it includes some specific statements to trap exceptions from user code. In the
java side, the exceptions can be originated from an incorrect request, or can
be originated in the code called from the request. Both exception types will be
sent to prolog using the main socket stream, leaving the prolog
program manage the exception. However, the first kind of exceptions are
prefixed, so the user program can distinguish from the
second type of exceptions.

In order to handle exceptions properly using the prolog to java and java to 
prolog interfaces simultaneously, in both sides of the interface will be
filtered those exceptions coming from their own side: this avoids an
endless loop of exceptions bouncing from one side to another.

").

%%:- comment(doinclude,java_stream/3).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(read),[read/2]).
:- use_module(library(write),[write/1]).
:- use_module(javasock).
:- use_module(library(system)).

% 
% /home/clip/Systems/ciao/lib/engine/basic_props.pl
%% -----------------------------------------------------------------------
%% REGTYPES
%% -----------------------------------------------------------------------

:- regtype machine_name(X) # "@var{X} is the network name of a machine.".

machine_name(X) :- atm(X).

:- regtype java_object(X) # "@var{X} is a java object (a structure with functor
	'$java_object', and argument an integer given by the java side).".

java_object('$java_object'(X)) :- int(X).

:- regtype java_field(X) # "@var{X} is a java field (structure on which the functor
	name is the field name, and the single argument is the field value).".

java_field(X) :- struct(X).

:- regtype java_method(X) # "@var{X} is a java method (structure with
	functor as method name, and arguments as method ones, plus a result
	argument. This result argument is unified with the atom 'Yes' if
	the java method returns void).".

java_method(X) :- struct(X).

:- regtype java_constructor(X) # "@var{X} is a java constructor (structure
	with functor as constructor full name, and
	arguments as constructor arguments).".

java_constructor(X) :- struct(X).

:- regtype java_event(X) # "@var{X} is a java event represented as an atom
	with the full event constructor name (e.g.,
	'java.awt.event.ActionListener').".

java_event(X) :- atm(X).

:- regtype prolog_goal(X) # "@var{X} is a prolog predicate. Prolog term
	that represents the goal that must be invoked when the event
	raises on the object. The predicate arguments can be java objects,
	or even the result of java methods. These java objects will be
	evaluated when the event raises (instead of when the listener is
	added). The arguments that represent java objects must be
	instantiated to already created objects. The variables will be kept
	uninstantiated when the event raises and the predicate is
	called.".

prolog_goal(_).

%% -----------------------------------------------------------------------
:- pred java_connection/0 # "Starts the Java server on the local machine,
	connects it through the data and event sockets, and starts the event
	handling thread.".
%% -----------------------------------------------------------------------

java_connection :-
	(java_stream(_,_,_) ->
        format(user_error,
             '{ERROR: the connection with java is already active}',[]),
        fail
	;true), 
	 !,
	compound_classpath(Classpath),
	append("java -classpath ", Classpath, CommandS1),
        append(CommandS1," CiaoJava.PLJavaServer", CommandS2),
        name(Command, CommandS2),
%        write(Command),
%        nl,
        popen(Command,read,Stream),
	current_host(Node),
	socket_connection(Node,Stream),
	!,
        eng_call(prolog_listener, create, create).

%% -----------------------------------------------------------------------
:- pred java_remote_connection(+machine_name) # "Starts the Java server in
	machine_name, connects it through the data and event sockets, and
	starts the event handling thread.".
%% -----------------------------------------------------------------------

java_remote_connection(Node):-
	(java_stream(_,_,_) ->
        format(user_error,
             '{ERROR: the connection with java is already active}',[]),
        fail
	;true), 
	 !,
        name(Node,NodeS),
	compound_classpath(Classpath),
	append("rsh -n ",NodeS,CommandS0),
	append(" java -classpath ", Classpath, CommandS1),
        append(CommandS1," java_daemon", CommandS2),
	append(CommandS0,CommandS2,CommandS3),
        name(Command, CommandS3),
        write(Command),
        nl,
        popen(Command,read,Stream),
	socket_connection(Node,Stream),
	!,
	eng_call(prolog_listener, create, create).

%% -----------------------------------------------------------------------
:- pred compound_classpath(-path)
	:: atom # "Compounds a string with the classpath needed by java to
	run the java server.".
%% -----------------------------------------------------------------------

compound_classpath(Classpath) :-
        absolute_file_name(library('javall/javart'),AbsFileName),
        name(AbsFileName,AbsFileNameS),
        append(Classpath,"/javart.pl",AbsFileNameS).

%% -----------------------------------------------------------------------
:- pred java_create_object(+java_constructor,-java_object) 
	:: java_constructor * java_object # "New java object creation. The
	constructor must be a compound term as defined by its type, with
	the full class name as functor (e.g., 'java.lang.String'), and the
	parameters passed to the constructor as arguments of the
	structure.".
%% -----------------------------------------------------------------------

java_create_object(Constructor, Object):-
	(nonvar(Constructor) -> true),
	(var(Object) ->  true),
	Constructor =.. [Name | ArgList],
	java_fast_write(data, '$java_create_object'(Name,ArgList)),
	java_fast_read(data, Object),
	!,
	check_error(Object).

%% -----------------------------------------------------------------------
:- pred java_delete_object(+java_object) 
	:: java_object # "Java object deletion. It removes the object given
	as argument from the Java object table.".
%% -----------------------------------------------------------------------

java_delete_object(Object) :-
	(nonvar(Object) -> true),
	java_fast_write(data, '$java_delete_object'(Object)),
	java_fast_read(data, T),
	!,
	check_error(T).

%% -----------------------------------------------------------------------
:- pred java_get_value(+java_object,+java_field)
	:: java_object * java_field # "Gets the value of a field. Given a
	Java object as first argument, it instantiates the variable given as second
	argument. This field must be
	uninstantiated in the java_field functor, or this predicate will fail.".
%% -----------------------------------------------------------------------

java_get_value(Object,Field) :-
        (nonvar(Object) -> true),
        Field =.. [Name, Value],
        java_fast_write(data, '$java_get_value'(Object,Name)),
        java_fast_read(data, Value),
	!,
        check_error(Value).

%% -----------------------------------------------------------------------
:- pred java_set_value(+java_object,+java_field) 
	:: java_object * java_field # "Sets the value of a Java object
	field. Given a Java object reference, it assigns the value included in
	the java_field compound term. The field value in the java_field
	structure must be instantiated.".
%% -----------------------------------------------------------------------

java_set_value(Object,Field) :-
        (nonvar(Object) -> true),
        Field =.. [Name, Value],
        java_fast_write(data, '$java_set_value'(Object,Name,Value)),
        java_fast_read(data, T),
	!,
        check_error(T).

%% -----------------------------------------------------------------------
:- pred java_invoke_method(+java_object,+java_method) 
	:: java_object * java_method # "Invokes a java method on an
	object. Given a Java object reference, invokes the method
	represented with the second argument. ".
%% -----------------------------------------------------------------------

java_invoke_method(Object,Method) :-
        (nonvar(Object) -> true),
        (nonvar(Method) -> true),
        Method =.. [Name | ArgList],
        append(Args,[Result],ArgList),
        java_fast_write(data, '$java_invoke_method'(Object,Name,Args)),
        java_fast_read(data, Result),
	!,
	check_error(Result).

%% -----------------------------------------------------------------------
:- pred java_add_listener(+java_object, +java_event, +prolog_goal) 
	:: java_object * java_event * prolog_goal # "Adds a listener to an
	event on an object. Given a Java object reference, it registers the
	goal received as third argument to be launched when the Java event
	raises.".
%% -----------------------------------------------------------------------

:- meta_predicate java_add_listener(?,?,goal).

java_add_listener(Object, Event, Predicate) :-
        (nonvar(Object) -> true),
        (nonvar(Event) -> true),
        (nonvar(Predicate) -> true),
        java_fast_write(data, '$java_add_listener'(Object, Event, Predicate)),
        java_fast_read(data, T),
        check_error(T).

%% -----------------------------------------------------------------------
:- pred java_remove_listener(+java_object, +java_event, +prolog_goal) 
	:: java_object * java_event * prolog_goal # "It removes a listener
	from an object event queue. Given a Java object reference, goal
	registered for the given event is removed.".
%% -----------------------------------------------------------------------

java_remove_listener(Object, Event, Predicate) :-
        (nonvar(Object) -> true),
        (nonvar(Event) -> true),
        (nonvar(Predicate) -> true),
        java_fast_write(data, '$java_remove_listener'(Object, Event, Predicate)),
        java_fast_read(data, T),
        check_error(T).

%% -----------------------------------------------------------------------
:- pred prolog_listener/0 # "Prolog event listener. Started in a separated
	thread, listens at the event socket for events received from
	java. This predicate launches the Prolog goal received from the
	socket.".
%% -----------------------------------------------------------------------

prolog_listener :-
        java_fast_read(event, Predicate),
        check_error(Predicate),
        eng_call(Predicate, create, create),
        prolog_listener.


%% -----------------------------------------------------------------------
:- pred java_disconnection/0
        # "Closes the connection with the java process.".
%% -----------------------------------------------------------------------

java_disconnection :-
	eng_killothers,
        socket_disconnection.

%% -----------------------------------------------------------------------
:- pred check_error(+term)
	:: term
	# "Checks if a term received from java is the 'fail' term (this
	may be changed if the 'No' atom is used).".
%% -----------------------------------------------------------------------

check_error('No') :- 
        !,
        fail.

check_error(_) :- 
	!.



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
	prolog_predicate/1,
        java_remove_listener/3
	],

	[assertions,regtypes,isomodes]).

:- comment(title,"Low-level Prolog to Java interface").

:- comment(author,"Jes@'{u}s Correas").

:- comment(module,"This module defines a low level Prolog to Java interface.").

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

:- regtype machine_name(X) # "@var{X} is the Internet name of a machine.".

machine_name(X) :- atm(X).

:- regtype java_object(X) # "@var{X} is a java object (structure with functor
	'$java_object', and argument an integer given by java side).".

java_object('$java_object'(X)) :- int(X).

:- regtype java_field(X) # "@var{X} is a java field (structure with functor as
	field name, and argument as value).".

java_field(X) :- struct(X).

:- regtype java_method(X) # "@var{X} is a java method (structure with
	functor as method name, and arguments as method ones, plus a result
	argument. This result argument is unified with the atom 'Yes' if
	the java method returns void).".

java_method(X) :- struct(X).

:- regtype java_constructor(X) # "@var{X} is a java constructor (structure
	with functor as constructor name (full package specification), and
	arguments as constructor ones).".

java_constructor(X) :- struct(X).

:- regtype java_event(X) # "@var{X} is a java event represented as an atom
	with the basic event constructor name (e.g.,
	'java.awt.event.ActionListener').".

java_event(X) :- atm(X).

:- regtype prolog_predicate(X) # "@var{X} is a prolog predicate. Prolog term
	that represents the predicate that must be invoked when the event
	raises on the object. The predicate arguments can be java objects,
	or even the result of java methods. These java objects will be
	evaluated when the event raises (instead of when the listener is
	added). The arguments that represent java objects must be
	instantiated to already created objects. The variables will be kept
	uninstantiated when the event raises and the predicate will be
	called.".

prolog_predicate(_).

%% -----------------------------------------------------------------------
:- pred java_connection/0
	# "Starts java server and connects it through data and event sockets.".
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
        write(Command),
        nl,
        popen(Command,read,Stream),
	current_host(Node),
	socket_connection(Node,Stream),
	!,
        eng_call(prolog_listener, create, create).

%% -----------------------------------------------------------------------
:- pred java_remote_connection(+machine_name) # "Starts java server in 
	machine_name and connects it through data and event sockets.".
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
	:: atom
        # "Compounds a string with the classpath needed by java to run the java
	  server.".
%% -----------------------------------------------------------------------

compound_classpath(Classpath) :-
        absolute_file_name(library('javall/javart'),AbsFileName),
        name(AbsFileName,AbsFileNameS),
        append(Classpath,"/javart.pl",AbsFileNameS).

%% -----------------------------------------------------------------------
:- pred java_create_object(+java_constructor,-java_object) 
	:: java_constructor * java_object 
        # "New java object creation. The constructor must be
    	built by the full class name as main functor, and the parameters
    	passed to the constructor as arguments of the structure.".
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
	:: java_object
        # "Java objects deletion.".
%% -----------------------------------------------------------------------

java_delete_object(Object) :-
	    (nonvar(Object) -> true),
	    java_fast_write(data, '$java_delete_object'(Object)),
	    java_fast_read(data, T),
	    !,
	    check_error(T).

%% -----------------------------------------------------------------------
:- pred java_get_value(+java_object,+java_field)  
	:: java_object * java_field 
        # "Get the value of a field. The field value in the
	Java_field functor must be uninstantiated.".
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
	:: java_object * java_field 
        # "Set the value of a field. The field value in the
	Java_field functor must be instantiated.".
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
	:: java_object * java_method
        # "Invokes a java method on an object.".
%% -----------------------------------------------------------------------

java_invoke_method(Object,Method) :-
nl,display('java_invoke_method('),display(Object),display(','),display(Method),
display(')'),nl,
        (nonvar(Object) -> true),
        (nonvar(Method) -> true),
        Method =.. [Name | ArgList],
        append(Args,[Result],ArgList),
        java_fast_write(data, '$java_invoke_method'(Object,Name,Args)),
        java_fast_read(data, Result),
	!,
	check_error(Result).

%% -----------------------------------------------------------------------
:- pred java_add_listener(+java_object, +java_event, +prolog_predicate) 
	:: java_object * java_event * prolog_predicate
        # "Adds a listener to an event on an object.".
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
:- pred java_remove_listener(+java_object, +java_event, +prolog_predicate) 
	:: java_object * java_event * prolog_predicate
        # "Removes a listener from an object event queue.".
%% -----------------------------------------------------------------------

java_remove_listener(Object, Event, Predicate) :-
        (nonvar(Object) -> true),
        (nonvar(Event) -> true),
        (nonvar(Predicate) -> true),
        java_fast_write(data, '$java_remove_listener'(Object, Event, Predicate)),
        java_fast_read(data, T),
        check_error(T).

%% -----------------------------------------------------------------------
:- pred prolog_listener/0 # "Prolog event listener. Daemon that listens
	at the event socket for events received from java. This predicate
	must be started in a separate thread when the connection be started.".
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
	eng_killothers,    %% OJOJO: this should be replaced by
	                     %% killing just the listener thread.
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

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:









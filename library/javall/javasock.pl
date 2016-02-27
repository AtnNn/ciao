:- module(javasock, [
	start_socket_interface/1,
	stop_socket_interface/0,
	join_socket_interface/0,
	java_query/2,
	java_response/2,
	prolog_query/2,
	prolog_response/2,
	java_stream/3,
	java_debug/1
	],
	[assertions,regtypes,isomodes]).

:- comment(title,"Low-level Prolog to Java socket connection").

:- comment(author,"Jes@'{u}s Correas").

:- comment(module,"
@cindex{Socket implementation}
This module defines a low level socket interface, to be used by javart and
jtopl. Includes all the code related directly to the handling of
sockets. This library
should not be used by any user program, because is a very low-level
connection to Java. Use @lib{javart} (Prolog to Java low-level interface)
or @lib{jtopl} (Java to Prolog interface) libraries instead.

").

:- use_module(library(fastrw), [fast_read/1, fast_write/1]).
:- use_module(library(read),[read/2]).
:- use_module(library(sockets)).
:- use_module(library(dynamic)). 
:- use_module(library(format)). 
:- use_module(library(concurrency)). 
:- use_module(library('javall/jtopl')).

:- regtype machine_name(?Name) # "@var{Name} is a valid host name.".
machine_name(X) :- atm(X).

:- pred java_stream(pjStream, jpStream, Address)
	:: struct * struct * machine_name # "Stores the identifiers
        of the streams used. A fact is asserted when the connection 
        to the Java process is established. It Contains 
        prolog-to-java and java-to-prolog streams,
	and the network	address where the Java process is running.".
:- data java_stream/3.

:- pred java_threads(pjIn,pjOut,jpIn,jpOut,plServer)
	:: int * int * int * int * int # "Stores the threads used to
        handle the sockets and the goal server.".
:- data java_threads/5.

%% -----------------------------------------------------------------------
%% MESSAGE QUEUES
%% Dynamic predicates used for communication between user threads and 
%% socket handling threads.
%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------
:- pred java_query(threadId,query)
	:: atm * term # "Data predicate containing the queries to be sent
        to Java. First argument is the Prolog thread Id, and second
        argument is the query to send to Java.".
%% -----------------------------------------------------------------------
:- concurrent java_query/2.

%% -----------------------------------------------------------------------
:- pred java_response(id,response)
	:: atm * term # "Data predicate that stores the responses to
        requests received from Java. First argument corresponds to
        the Prolog thread Id; second argument corresponds to the
        response itself.".
%% -----------------------------------------------------------------------
:- concurrent java_response/2.

%% -----------------------------------------------------------------------
:- pred prolog_query(id, query)
	:: int * term # "Data predicate that keeps a queue of the queries
        requested to Prolog side from Java side.".
%% -----------------------------------------------------------------------
:- concurrent prolog_query/2.

%% -----------------------------------------------------------------------
:- pred prolog_response(id, response)
	:: int * term # "Data predicate that keeps a queue of the responses
        to queries requested to Prolog side from Java side.".
%% -----------------------------------------------------------------------
:- concurrent prolog_response/2.

%% -----------------------------------------------------------------------
:- pred start_socket_interface(+Address) 
	:: term
        # "Given an address in format 'node:port', creates the sockets
        to connect to the java process, and starts the threads needed
        to handle the connection.". 
%% -----------------------------------------------------------------------

start_socket_interface(Node:SocketId):-
        java_client(Node:SocketId),
	!,
	eng_call(pj_socket_reader, create, create, PjIn),
	eng_call(pj_socket_writer, create, create, PjOut),
	eng_call(jp_socket_reader, create, create, JpIn),
	eng_call(jp_socket_writer, create, create, JpOut),
	eng_call(shell_s,create,create,PS),
	set_fact(java_threads(PjIn,PjOut,JpIn,JpOut,PS)).

%% -----------------------------------------------------------------------
:- pred stop_socket_interface # "Closes the sockets to disconnect from 
        the java process, and waits until the threads that handle the
        connection terminate.".
%% -----------------------------------------------------------------------
stop_socket_interface :-
	assertz_fact(java_query(0,'$terminate')),
	assertz_fact(prolog_response(0,'$terminate')),
	join_socket_interface,
	retract_fact(java_threads(PjIn,PjOut,JpIn,JpOut,PlServer)),
	eng_release(PjIn),
	eng_release(PjOut),
	eng_release(JpIn),
	eng_release(JpOut),
	eng_release(PlServer),
        retract_fact(java_stream(DataStream,EventStream,_)),
        close(DataStream),
        close(EventStream),
	retractall_fact(java_query(_,_)),
	retractall_fact(java_response(_,_)),
	retractall_fact(prolog_query(_,_)),
	retractall_fact(prolog_response(_,_)),
	!.

%% -----------------------------------------------------------------------
:- pred join_socket_interface # "Waits until the threads that handle the
        connection terminate.".
%% -----------------------------------------------------------------------
join_socket_interface:-
	java_threads(PjIn,PjOut,JpIn,JpOut,PlServer),
	eng_wait(PjIn),
	eng_wait(PjOut),
	eng_wait(JpIn),
	eng_wait(JpOut),
	eng_wait(PlServer),
	!.

join_socket_interface.

%% -----------------------------------------------------------------------
:- pred pj_socket_reader/0 # "Predicate that runs in a separate thread
	reading from the prolog-to-java socket. If receives a disconnect
        or terminate request, just finish the thread.".
%% -----------------------------------------------------------------------
pj_socket_reader :-
	repeat,
	  java_fast_read(pj,pj(Id,R)),
	  java_debug('pj_socket_reader: pj'(Id,R)),
	  (termination_check(Id,R) -> 
	   assertz_fact(java_query(Id,R)),
	   true
	  ;
	   assertz_fact(java_response(Id,R)),
	   fail
	  ),
	  !.

%% -----------------------------------------------------------------------
:- pred pj_socket_writer/0 # "Predicate that runs in a separate thread
	writing to the prolog-to-java socket. If receives a disconnect
        or terminate request, just finish the thread.".
%% -----------------------------------------------------------------------
pj_socket_writer :-
	retract_fact(java_query(Id,Q)),
	java_debug('pj_socket_writer: pj'(Id,Q)),
	java_fast_write(pj,pj(Id,Q)),
	(termination_check(Id,Q) -> 
	 true
	;
	 fail
	),
	!.

%% -----------------------------------------------------------------------
:- pred jp_socket_reader/0 # "Predicate that runs in a separate thread
	reading from the java-to-prolog socket. If receives a disconnect
        or terminate request, sends the termination message to the peer
        thread (that handles jp socket writing), and to the Prolog goal
        handler. Finally, terminates itself.
        IMPORTANT: a special query internal_use_module/1 is asserted
        from javart in prolog_query/2 predicate.".
%% -----------------------------------------------------------------------
jp_socket_reader :-
        repeat,
          java_fast_read(jp,jp(Id,Q)),
	  java_debug('jp_socket_reader: jp'(Id,Q)),
	  (termination_check(Id,Q) -> 
	   assertz_fact(prolog_query(0,Q)),
	   assertz_fact(prolog_response(0,Q))
	  ;
	   assertz_fact(prolog_query(Id,Q)),
	   fail
	  ),
	  !.

%% -----------------------------------------------------------------------
:- pred jp_socket_writer/0 # "Predicate that runs in a separate thread
	writing to the java-to-prolog socket. If receives a disconnect
        or terminate request, just finish the thread.".
%% -----------------------------------------------------------------------
jp_socket_writer :-
	retract_fact(prolog_response(Id,R)),
	java_debug('jp_socket_writer:jp'(Id,R)),
	java_fast_write(jp,jp(Id,R)),
	(termination_check(Id,R) -> 
	 true
	;
	 fail
	),
	!.

%% -----------------------------------------------------------------------
:- pred termination_check(+Term,+Term)
	:: term * term # "Checks if the termination atom is received.".
%% -----------------------------------------------------------------------

termination_check(0,'$terminate').
termination_check(0,'$disconnect').

%% -----------------------------------------------------------------------
:- pred java_client(+Address) 
	:: term # "Opens a connection at an address, and asserts the
        @tt{java_stream} corresponding fact.".
%% -----------------------------------------------------------------------

java_client(Address) :-
        open_client(Address,DataStream,EventStream),
	set_fact(java_stream(DataStream,EventStream,Address)).

%% -----------------------------------------------------------------------
:- pred open_client(+Address, -Stream, -Stream)
	:: term * stream * stream
        # "Given an address (@tt{Host:Port} format), creates and synchronizes
	  the sockets to the java process.".
%% -----------------------------------------------------------------------

open_client(Host:Port,PJStream,JPStream) :-
	open_pj(Host, Port, PJStream),
	open_jp(Host, Port, JPStream).

open_pj(Host, Port, PJStream) :-
        connect_to_socket(Host, Port, PJStream),
	java_fast_write0(PJStream,pj(0,data)),
        java_fast_read0(PJStream, pj(0,data)).

open_jp(Host, Port, JPStream) :-
        connect_to_socket(Host, Port, JPStream),
	java_fast_write0(JPStream,jp(0,event)),
        java_fast_read0(JPStream, jp(0,event)).

%% -----------------------------------------------------------------------
:- pred java_fast_write(+Type, +Term) 
	:: atom * term # " It writes on the given stream type the term
        received as second argument. This is the basic predicate used
        to send data to the Java side. The first argument
	reflects the socket (prolog-to-java or java-to-prolog).
        The second argument is the term to be sent to the Java side.".
%% -----------------------------------------------------------------------
java_fast_write(pj,T) :-
        current_fact(java_stream(PJStream,_,_)),
        java_fast_write0(PJStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_write(jp,T) :-
        current_fact(java_stream(_,JPStream,_)),
        java_fast_write0(JPStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_write0(Stream,T) :-
        (current_stream(_N,socket,Stream) ->
	 current_output(CU),
	 set_output(Stream),
	 fast_write(T),
	 flush_output(Stream),
	 set_output(CU)
	;
	 format(user_error,
	 '{ERROR: the connection with java has been shut down!}',[]),
	 fail
        ).

%% -----------------------------------------------------------------------
:- pred java_fast_read(+Type, -Term) :: atom * term # "It reads from the
	given stream type one term and unifies it with the term received as
	second argument. This is the basic predicate used to receive data
	from the Java side. The first argument reflects the socket type
	(prolog-to-java or java-to-prolog). The second argument is
        unified with the data received from the socket.".
%% -----------------------------------------------------------------------

java_fast_read(pj, T) :-
        current_fact(java_stream(PJStream,_,_)),
        java_fast_read0(PJStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_read(jp, T) :-
        current_fact(java_stream(_,JPStream,_)),
        java_fast_read0(JPStream,T),
        !.  %% Avoid choicepoints (none should have been pushed---just in case)

java_fast_read0(Stream,T) :-
        (current_stream(_N,socket,Stream) ->
	 current_input(CU),
	 set_input(Stream),
	 fast_read(T),
	 set_input(CU)
        ;
	 format(user_error,
	 '{ERROR: the connection with java has been shut down!}',[]),
	 fail
        ).

%%------------------------------------------------------------------
%% ONLY FOR DEBUGGING
%%------------------------------------------------------------------

:- data debugging/0.
% Comment/uncomment next line to set debugging off/on.
debugging.

java_debug(T) :-
	debugging,
	open('javasock.log',append,S),
	display(S,T),nl(S),
	close(S),
	!.

java_debug(_) :- !.


%%------------------------------------------------------------------------
%% VERSION CONTROL
%%------------------------------------------------------------------------
 
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*7+82,2001/03/30,13:16*44+'CEST'), "Added some
cuts & changed socket dispatch loop to be a fail loop.  (MCL)").

:- comment(version(1*7+71,2001/03/23,12:53*31+'CET'),
   "socket_connection/2 predicate definition is removed; the
   connection to Java is performed exclusively by start_socket_interface/1.
   (Jesus Correas Fernandez)").

:- comment(version(1*5+152,2000/05/26,13:36*48+'CEST'), "Some
constants in pred assertions changed to variables.  (MCL)").

:- comment(version(1*5+49,2000/02/08,16:35*38+'CET'), "Predicate
   documentation.  (Jesus Correas Fernandez)").

%%------------------------------------------------------------------------






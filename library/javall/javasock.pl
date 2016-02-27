:- module(javasock, [
	socket_connection/2,
	socket_disconnection/0,
	java_fast_read/2,
	java_fast_write/2,
	java_stream/3],
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


:- prop machine_name(?Name) # "@var{Name} is a valid host name.".
machine_name(_).

:- pred java_stream(DataStream, EventStream, Address)
	:: atm * int * machine_name # "Stores the identifiers of the streams
	used. A fact is asserted when the connection to the Java process is
	established. It Contains the data and event streams, and the network
	address where the Java process is running.".

:- dynamic java_stream/3.

%% -----------------------------------------------------------------------
:- pred socket_connection(+Node, +Stream) 
	:: atom * stream
        # "Given a stream connected to a node, it gets the socket port from
	  the given stream and creates the sockets to connect to the java
	  process.". 
%% -----------------------------------------------------------------------

socket_connection(Node, Stream):-
        current_input(CU),
        set_input(Stream),
        read(Stream, SocketId),
        set_input(CU),
        java_client(Node:SocketId).

%% -----------------------------------------------------------------------
:- pred socket_disconnection/0 
	# "It closes the sockets to disconnect from the java process.".
%% -----------------------------------------------------------------------

socket_disconnection :-
        retract_fact(java_stream(DataStream,EventStream,_)),
        close(DataStream),
        close(EventStream),
	!.

%% -----------------------------------------------------------------------
:- pred java_client(+Address)
	# "Opens a connection at an address, and asserts the @tt{java_stream}
	  corresponding fact.".
%% -----------------------------------------------------------------------

java_client(Address) :-
        open_client(Address,DataStream,EventStream),
        asserta_fact(java_stream(DataStream,EventStream,Address)).

%% -----------------------------------------------------------------------
:- pred open_client(+Address, -Stream, -Stream)
	:: term * stream * stream
        # "Given an address (@tt{Host:Port} format), creates and synchronizes
	  the sockets to the java process.".
%% -----------------------------------------------------------------------

open_client(Host:Port,DataStream,EventStream) :-
	open_data_client(Host, Port, DataStream),
	open_event_client(Host, Port, EventStream).

open_data_client(Host, Port, DataStream) :-
        connect_to_socket(Host, Port, DataStream),
	java_fast_write0(DataStream,data),
        java_fast_read0(DataStream, data).

open_event_client(Host, Port, EventStream) :-
        connect_to_socket(Host, Port, EventStream),
	java_fast_write0(EventStream,event),
        java_fast_read0(EventStream, event).

%% -----------------------------------------------------------------------
:- pred java_fast_write(+Type, +Term) :: atom * term # " It writes on the given
	stream type the term received as second argument. This is the basic
	predicate used to send data to the Java side. The first argument
	reflects the socket type: event or data. The second argument is the
	term to be sent to the Java side.".
%% -----------------------------------------------------------------------
java_fast_write(data,T) :-
        current_fact(java_stream(DataStream,_,_)),
        java_fast_write0(DataStream,T).

java_fast_write(event,T) :-
        current_fact(java_stream(_,EventStream,_)),
        java_fast_write0(EventStream,T).

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
	from the Java side. The first argument reflects the socket type:
	event or data. The second argument is unified with the data
	received from the socket.".
%% -----------------------------------------------------------------------

java_fast_read(data, T) :-
        current_fact(java_stream(DataStream,_,_)),
        java_fast_read0(DataStream,T).

java_fast_read(event, T) :-
        current_fact(java_stream(_,EventStream,_)),
        java_fast_read0(EventStream,T).

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


%%------------------------------------------------------------------------
%% VERSION CONTROL
%%------------------------------------------------------------------------
 
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+152,2000/05/26,13:36*48+'CEST'), "Some
constants in pred assertions changed to variables.  (MCL)").

:- comment(version(1*5+49,2000/02/08,16:35*38+'CET'), "Predicate
   documentation.  (Jesus Correas Fernandez)").

%%------------------------------------------------------------------------

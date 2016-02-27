:- module(sockets,
        [connect_to_socket/3,
         socket_recv/2,
         socket_type/1], 
         [assertions,isomodes,regtypes]).

:- reexport(library('sockets/sockets_c')).

:- comment(title, "The socket interface").

:- comment(author, "Manuel Carro").
:- comment(author, "Daniel Cabeza").

:- comment(module, "This module defines primitives to open sockets,
        send, and receive data from them.  This allows communicating
        with other processes, on the same machine or across the
        Internet. The reader should also consult standard bibliography
        on the topic for a proper use of these primitives.").

%% Socket types

:- comment(doinclude,connect_to_socket_type/4).
:- comment(doinclude,connect_to_socket/3).
:- comment(doinclude,bind_socket/3).
:- comment(doinclude,socket_accept/2).
:- comment(doinclude,select_socket/5).
:- comment(doinclude,socket_send/2).
:- comment(doinclude,socket_recv_code/3).
:- comment(doinclude,socket_recv/2).
:- comment(doinclude,socket_type/1).

%% :- type socket_type/1.

:- comment(socket_type/1,"Defines the atoms which can be used to
   specify the socket type recognized by
   @pred{connect_to_socket_type/4}. Defined as follows:
   @includedef{socket_type/1}").

:- regtype socket_type(T) # "@var{T} is a valid socket type.".

socket_type(stream).
socket_type(dgram).
socket_type(raw).
socket_type(seqpacket).
socket_type(rdm).


%% Automatically reexported

:- pred connect_to_socket_type(+Host, +Port, +Type, -Stream) ::
        atom * int * socket_type * stream
 # "Returns a @var{Stream} which connects to @var{Host}.  The @var{Type} of 
    connection can be defined.  A @var{Stream} is returned, which can be 
    used to @pred{write/2} to, to @pred{read/2}, to @pred{socket_send/2} to, 
    or to @pred{socket_recv/2} from the socket.".


:- pred connect_to_socket(+Host, +Port, -Stream) ::
        atm * int * stream
 # "Calls @pred{connect_to_socket_type/4} with SOCK_STREAM connection
    type.  This is the connection type you want in order to use the 
    @pred{write/2} and @pred{read/2} predicates (and other stream IO 
    related predicates).".

connect_to_socket(Host, Port, Stream):-
        connect_to_socket_type(Host, Port, stream, Stream).


%% Automatically reexported

:- pred bind_socket(?Port, +Length, -Socket) ::
        int * int * int
 # "Returs an AF_INET @var{Socket} bound to @var{Port} (which may be 
    assigned by the OS or defined by the caller), and listens to it 
    (hence no listen call in this set of primitives).
    @var{Length} specifies the maximum number of pending connections.".


%% Automatically reexported

:- pred socket_accept(+Sock, -Stream) ::
        int * stream
 # "Creates a new @var{Stream} connected to @var{Sock}.".


%% Automatically reexported

:- pred select_socket(+Socket, -NewStream, +TO_ms, +Streams, -ReadStreams) ::
        int * stream * int * list(stream) * list(stream).


%% Automatically reexported

:- pred socket_send(+Stream, +String) ::
        stream * string 
 # "Sends @var{String} to the socket associated to @var{Stream}. The socket 
    has to be in connected state. @var{String} is not supposed to be 
    NULL terminated, since it is a Prolog string.  If a NULL terminated 
    string is needed at the other side, it has to be explicitly created in 
    Prolog.".

%% Automatically reexported

:- pred socket_recv_code(+Stream, ?String, ?Length) ::
        stream * string * int 
 # "Receives a @var{String} from the socket associated to @var{Stream},
    and returns its @var{Length}.  If @var{Length} is -1, no more data is 
    available.".


:- pred socket_recv(+Stream, ?String) ::
        stream * string
 # "As @pred{socket_recv_code/3}, but the return code is ignored.".

socket_recv(Stream, String):- socket_recv_code(Stream, String, _).

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+127,2000/05/02,16:50*36+'CEST'), "The stream
associated to a socket was added locally.  Now a call to a new
function in the engine is made, so that this addition is made
atomically.  (MCL) (MCL)").

:- comment(version(1*3+5,1999/06/21,14:22*42+'MEST'), "Changed sockets
   library to do reexport (Daniel Cabeza Gras)").

:- comment(version(0*9+86,1999/05/07,23:25*34+'MEST'), "Improved
   comments somewhat.  (Manuel Hermenegildo)").

:- comment(version(0*7+19,1998/10/09,11:45*58+'MEST'), " Updated error
   messages. (Jose Manuel Gomez Perez & MCL)").

:- comment(version(0*7+2,1998/09/15,16:03*29+'MEST'), "Some new
documentation added.  (MCL)").

:- comment(version(0*6+17,1998/09/11,09:25*20+'MEST'), "Improved
   documentation of sockets library.  (Manuel Carro)").


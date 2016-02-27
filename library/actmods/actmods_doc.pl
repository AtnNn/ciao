:- use_package(assertions).

:- comment(nodoc,assertions).

:- comment(title,"Active modules (high-level distributed execution)").

:- comment(author,"Manuel Hermenegildo").
:- comment(author,"Daniel Cabeza").

:- comment(module,"

Active modules @cite{ciao-dis-impl-parimp-www} provide a high-level
model of @concept{inter-process communication} and
@concept{distributed execution} (note that using Ciao's communication
and concurrency primitives, such as sockets, concurrent predicates,
etc.).  An @index{active module} (or an @index{active object}) is an
ordinary module to which computational resources are attached, and
which resides at a given location on the network.  Compiling an active
module produces an executable which, when running, acts as a server
for a number of predicates, the predicates exported by the
module. Predicates exported by an active module can be accessed
by a program on the network by simply ``using'' the module, which then
imports such ``remote predicates.''  The process of loading an active
module does not involve transferring any code, but rather setting up
things so that calls in the module using the active module are
executed as remote procedure calls to the active module. This
occurs in the same way independently of whether the 
active module and the using module are in the same machine or in different machines across the network.

Except for having compiling it in a special way (see below), an active
module is identical from the programmer point of view to an ordinary
module. A program using an active module imports it and uses it in the
same way as any other module, except that it uses
``@decl{use_active_module}'' rather than ``@decl{use_module}'' (see
below). Also, an active module has an address (network address) which
must be known in order to use it.  In order to use an active module it
is necessary to know its address.


@footnote{It is also possible to provide active modules via a WWW
address.  However, we find it more straightforward to simply use
socket addresses. In any case, this is generally hidden inside the
access method and can be thus made transparent to the user.}

From the implementation point of view, active modules are essentially
daemons: executables which are started as independent processes at the
operating system level.  Communication with active modules is
implemented using sockets (thus, the address of an active module is an
IP socket address in a particular machine).  Requests to execute goals
in the module are sent through the socket by remote programs.  When
such a request arrives, the process running the active module takes it
and executes it, returning through the socket the computed
answers. These results are then taken and used by the remote
processes. Backtracking over such remote calls works as usual and
transparently.  The only limitation (this may change in the future,
but it is currently done for efficiency reasons) is that all
alternative answers are precomputed (and cached) upon the first call
to an active module and thus @em{an active module should not export a
predicate which has an infinite number of answers}.



  The first thing to do is to select a method whereby the client(s)
  (the module(s) that will use the active module) can find out in
  which machine/port (IP address/socket number) the server (i.e., the
  active module) will be listening once started.  The easiest way to
  do this is to use of the redezvous methods which are provided in the
  Ciao distribution in the @tt{library/actmods} directory, such as
  @tt{tmpbased...} or @tt{filebased...}. 

  The first is based on saving the IP address and socket number of the
  server in a file in a predefined directory (generally @tt{/tmp}, but
  this can be changed by changing @tt{tmpbased_common.pl}).

  The second one is similar but saves the info in the directory in
  which the server is started (as @em{<module_name>}@tt{.addr}). The
  clients must be started in the same directory. However, they can be
  started in different machines, provided this directory is shared
  (e.g., by NFS or Samba).

  These rendezvous methods are encoded in two modules: a first one,
  called @tt{...publish.pl}, is used by the server to publish its
  info. The second one, called @tt{...locate.pl}, is used by the
  client(s) to locate the server info. For efficiency, the client
  methods maintain a cache of addresses, so that the server
  information only needs to be read from the file system the first
  time the active module is accessed.

  Active modules are compiled using the @tt{-a} option of the Ciao
  compiler (this can also be done from the interactive top-level shell
  using @pred{make_actmod/2}). For example, issuing the following
  command:

  @begin{verbatim}
  ciaoc -a 'actmods/filebased_publish' simple_server
  @end{verbatim}

  compiles the simple server example that comes with the distribution
  (in the @tt{actmods/example} directory). The 
  @tt{simple_client_with_main} example (in the same directory) can be
  compiled as usual:

  @begin{verbatim}
  ciaoc simple_client_with_main
  @end{verbatim}

  Now, if the server is running (e.g., @tt{simple_server &} in Un*x or
  double-clicking on it in Win32) when the client is executed it will
  connect with the server to access the predicate(s) that it imports
  from it.

  A simpler even client @file{simple_client.pl} can be loaded into the
  top level and its predicates called as usual (and they will connect
  with the server if it is running).

  The architecture of the active modules model in Ciao allows defining
  other methods for this communication to happen. For example, one can
  implement a @concept{name server}. This can itself be an active
  module which resides at a fixed and known machine and port number
  (this is known as a @em{service} and is defined in
  @tt{/etc/services} in a Un*x machine). In order the best way is to
  mimick the code of the redezvous methods provided in the Ciao
  distribution.


Security: in the access method (?).

").

:- decl use_active_module(AModule,Predicates) : sourcename * list(predname)

        # "Specifies that this code imports from the @em{active
          module} defined in @var{AModule} the predicates in
          @var{Imports}.  The imported predicates must be exported by
          the active module. ".

% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+141,2000/05/11,13:33*29+'CEST'), "Added file to
   version control, started documentation (was about time).  (Manuel
   Hermenegildo)").
% ----------------------------------------------------------------------------


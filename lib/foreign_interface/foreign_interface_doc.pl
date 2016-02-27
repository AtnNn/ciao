:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title,"Foreign Language Interface Guidelines and Usage").

:- comment(author,"Jose Morales").
:- comment(author,"Manuel Carro").

:- comment(usage, "The foreign interface is used by including
@tt{foreign_interface} in the include list of a module, or by means of
an explicit @tt{:- use_package(foreign_interface)}.").

:- comment(summary, "The foreign interface module provides predicates
for building automatically the shared object necessary for accessing C
functions as Ciao Prolog predicates. Regular C functions do not have
their data in the internal format required by Ciao Prolog, and thus an
intermediate translation is necessary. Besides, run-time errors (such
as wrong instantiation states) must be handled and, finally, C code
must be compiled into a dynamically loadable object code form, which
is OS-dependent.  ").

:- comment(module, "Ciao Prolog provides a high-level, flexible way to
interface C and Prolog, based on the use of assertions to declare what
are the expected types and modes of the arguments of a Prolog
predicate, and which C files contain the corresponding code.  I.e., the user must provide: 

 @begin{itemize}
 @item A set of C files,
 @item A Ciao Prolog module defining whith predicates are
 implemented in the C files and the types and modes of their arguments,
 and an (optional) set of flags required for the compilation of the
 files.
 @end{itemize}


The Ciao Prolog compiler analyzes the Prolog code written by the user
and gathers this information to generate automatically C code
implementing the data translation between Prolog and C, and to compile
the C code into dynamically loadable C object files, which are loaded
automatically when needed.

To this end, each predicate implemented as a foreign C function must
appear in the Ciao Prolog module as:

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: type1 *
          ... * typeN + (foreign(foreign_function_name), returns(ArgR)).

     :- impl_defined([..., prolog_predicate/N, ...]).
   @end{verbatim}

     where @tt{m1}, ..., @tt{mN} and @tt{type1}, ..., @tt{typeN} are
respectively the modes and types of the arguments.
@code{foreign_function_name} is the name of the C function
implementing @pred{prolog_predicate/N}, and the result of this
function is unified with @tt{ArgR}.

     This notation can be simplified in several ways.  If the name of
the foreign function is the same as the name of the Ciao Prolog
predicate, @tt{foreign(foreign_function_name)} can be replaced by
@tt{foreign/0}. @tt{returns(ArgR)} specifies that the result of the
function corresponds to the @tt{ArgR} argument of the Ciao Prolog
predicate. If the foreign function does not return anything (or if its
value is ignored), then @tt{returns(ArgR)} must be removed. Note that
@tt{returns} cannot be used without @tt{foreign}.  A simplified form
is thus:

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: type1 *
          ... * typeN + foreign.
   @end{verbatim}

@section{Equivalence between Ciao Prolog and C types}

The translation between Ciao Prolog and C types is defined at the
moment only for some simple, but very useful, types. The translation
to be performed is solely defined by types of the arguments in the
Ciao Prolog file (i.e., no inspection of the corresponding C file is
done), obeying the table below.  The Ciao Prolog types understood by
the interface are contained in the @concept{package} @ref{Foreign
Language Interface Properties}, and are the following (Prolog types
are on the left, and the corresponding C types on the right):

   @begin{itemize}
   @item num <-> double
   @item int <-> int
   @item atm <-> char *
   @item string <-> char * (with trailing zero)
   @item byte_list <-> char * (a buffer of bytes, with associated length)
   @item int_list <-> int * (a buffer of integers, with associated length)
   @item address <-> void *
   @end{itemize}

   Strings, atoms, and lists of bytes are passed to (and from) C as
dynamically (malloc) created arrays of characters (bytes). Those
arrays are freed by Ciao Prolog upon return of the foreign function
unless @prop{do_not_free/2} is specified (see examples below).  This
caters for the case in which the C files save in a private state
(either by themselves, or by library being called by them) the values
passed on from Prolog.

Empty lists of bytes are converted into @tt{NULL}, and strings @tt{[]}
or atoms '' are converted into @em{\"\"} (a zero ended C string).
@em{NULL} is converted into an empty list or the @em{''} atom.

@tt{NULL} strings and empty buffers are converted into the empty list
or the null atom (@tt{''}). Empty Ciao Prolog strings (@tt{""} or
@tt{[]}) and null atoms are converted into zero-terminated strings of
zero length. Empty Ciao Prolog lists are converted into @tt{NULL}.

The type @regtype{byte_list/1} requires an additional property,
@prop{size_of/3}, to indicate which argument represents its size.

Most of the work is performed by the predicates in the 
@comment{@ref{Foreign Language Interface Builder}
         Turned this to @em since this file is not in the documentation and 
         thus the  cross-reference fails}
@em{Foreign Language Interface Builder}, which can be called
explicitly by the user.  Doing that is not usually needed, since the
Ciao Prolog Compiler takes care of building glue code files an of
compiling and linking whatever is necessary.


@section{Equivalence between Ciao Prolog and C modes}

        The in/1 or (prefix) +/1 mode states that the corresponding
argument is given a value in Prolog, and therefore it is an input
argument in the C part.  The go/1 or (prefix) -/1 mode states that the
Prolog side expects the C side to generate a value for that argument.
The return value of a function can always be used as an output
argument.  Other output arguments should appear as pointers to the
corresponding base type in the C files.  E.g., an argument which is an
integer generated by the C file, declared as

@begin{verbatim}
:- true pred get_int(go(ThisInt)) :: int + foreign
@end{verbatim}

or as

@begin{verbatim}
:- true pred get_int(+ThisInt) :: int + foreign
@end{verbatim}

should appear in the C code as

@begin{verbatim}
void get_int(int *thisint)
@{
        ....
@}
@end{verbatim}

The examples below illustrate this point, and the use of several
assertions to guide the compilation.


@section{Examples}

@subsection{Mathematical functions}

The mathematical library is accessed to provide the @em{sin},
@em{cos}, and @em{fabs} functions.  Note that that library is
specified simply as

@begin{verbatim}
:- use_foreign_library([m]).
@end{verbatim}

The foreign interface adds the @tt{-lm} at compile time.  Note also
how some additional options are added to optimize the compiled code
(only glue code, in this case) and mathematics (only in the case of
Linux in an Intel processor).

The functions imported from the C file, and exported as predicates by
the Prolog module are stated as defined elsewhere by the directive

@begin{verbatim}
:- impl_defined([sin/2,cos/2,fabs/2]).
@end{verbatim}

so that the Prolog compiler does not complain when examining the
Prolog file.

@em{math.pl}

@begin{verbatim} 
:- module(math,
	[sin/2,
	 cos/2,
	 fabs/2
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	]).

:- true pred sin(in(X),go(Y)) :: num * num + (foreign,returns(Y)).
:- true pred cos(in(X),go(Y)) :: num * num + (foreign,returns(Y)).
:- true pred fabs(in(X),go(Y)) :: num * num + (foreign,returns(Y)).

:- extra_compiler_opts(['-O2']).
:- extra_compiler_opts('LINUXi86',['-ffast-math']).
:- use_foreign_library([m]).

:- impl_defined([sin/2,cos/2,fabs/2]).
@end{verbatim}

@subsection{Addresses and C pointers}

The @tt{address} type designates any pointer, and provides a means to
deal with C pointers in Prolog without interpreting them whatsoever.
The C source file which implements the operations accessed from Prolog
is declared with the

@begin{verbatim}
:- use_foreign_source(objects_c).
@end{verbatim}

directive.

@em{objects.pl}

@begin{verbatim} 
:- module(objects,
	[object/2,
	 show_object/1
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	]).
	 
:- true pred object(in(N),go(Object)) :: int * address +
	(foreign,returns(Object)).

:- true pred show_object(in(Object)) :: address + foreign.

:- use_foreign_source(objects_c).
:- extra_compiler_opts('-O2').

:- impl_defined([object/2,show_object/1]).
@end{verbatim}

@em{objects_c.c}


@begin{verbatim} 
#include <stdio.h>

struct object @{
  char *name;
  char *colour;
@};

#define OBJECTS 3

struct object objects[OBJECTS] =
@{ @{\"ring\",\"golden\"@},
  @{\"table\",\"brown\"@},
  @{\"bottle\",\"green\"@} @};

struct object *object(int n) @{
  return &objects[n % OBJECTS];
@}

void show_object(struct object *o) @{
  printf(\"I show you a %s %s\\n\", o->colour, o->name);
@}
@end{verbatim}

@subsection{Lists of bytes and buffers}

A list of bytes (c.f., a list of ints) corresponds to a buffer in C.
The length of the buffer always goes associated to the list using the
property @tt{size_of/2}.  The returned buffer @bf{is freed by Ciao
Prolog} upon its recepction, unless the @tt{do_not_free/1} property is
specified (see later).  Conversely, a list of natural numbers not
exceeding 255 can be passed to C as a buffer.

@em{byte_lists.pl}

@begin{verbatim} 
:- module(byte_lists,
	[obtain_list/3,
	 show_list/2
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: int * int * byte_list
	+ (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: int * byte_list
	+ (foreign,size_of(List,Length)).

:- use_foreign_source(bytes_op).

:- impl_defined([obtain_list/3,show_list/2]).
@end{verbatim}

@em{bytes_op.c}

@begin{verbatim} 
#include <malloc.h>
#include <stdio.h>

void obtain_list(int n, int *l, char **s) @{
  int i;
  int c;
  if (n < 0) n = 0;
  *l = n;
  *s = (char *)malloc(*l);
  for (i = 0; i < *l; i++) @{
    (*s)[i] = i;
  @}
@}

void show_list(int l, char *s) @{
  if (s) @{
    int n;
    printf(\"From C: [\");
    for (n = 0; n < l; n++) @{
      printf(\" %d\", s[n]);
    @}
    printf(\"]\\n\");
  @} else @{
    printf(\"From C: []\\n\");
  @}
@}
@end{verbatim}

@subsection{Lists of integers}

@em{int_lists.pl}

@begin{verbatim} 
:- module(int_lists,
	[obtain_list/3,
	 show_list/2
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: int * int * int_list
	+ (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: int * int_list
	+ (foreign,size_of(List,Length)).

:- use_foreign_source(ints_op).

:- impl_defined([obtain_list/3,show_list/2]).
@end{verbatim}

@em{ints_op.c}

@begin{verbatim} 
#include <malloc.h>
#include <stdio.h>

void obtain_list(int n, int *l, int **s) @{
  int i;
  int c;
  if (n < 0) n = 0;
  *l = n;
  *s = (int *)malloc((*l) * sizeof(int));
  for (i = 0; i < *l; i++) @{
    (*s)[i] = i;
  @}
@}

void show_list(int l, int *s) @{
  if (s) @{
    int n;
    printf(\"From C:\");
    for (n = 0; n < l; n++) @{
      printf(\" %d\", s[n]);
    @}
    printf(\".\\n\");
  @} else @{
    printf(\"From C: []\\n\");
  @}
@}
@end{verbatim}

@subsection{Strings and atoms}

A C string can be seen as a buffer whose end is denoted by the
trailing zero, and therefore stating its length is not needed.  Two
translations are possible into Ciao Prolog: as a Prolog string (list
of bytes, with no trailing zero) and as an atom.  These are selected
automatically just by choosing the corresponding type (look at the
examples below).

Note how the @tt{do_not_free/1} property is specified in the
@pred{a_string/1} predicate: the string returned by C is static, and
therefore it should not be freed by Prolog.


@em{strings_and_atoms.pl}

@begin{verbatim} 
:- module(strings_and_atoms,
	[lookup_string/2,
	 lookup_atom/2,
	 a_string/1,
	 show_string/1,
	 show_atom/1
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	]).


:- true pred a_string(go(S)) ::
	string + (foreign(get_static_str),returns(S),do_not_free(S)).
	 
:- true pred lookup_string(in(N),go(S)) ::
	int * string + (foreign(get_str),returns(S)).
:- true pred lookup_atom(in(N),go(S)) ::
	int * atm + (foreign(get_str),returns(S)).

:- true pred show_string(in(S)) :: string + foreign(put_str).
:- true pred show_atom(in(S)) :: atm + foreign(put_str).

:- use_foreign_source(str_op).

:- impl_defined([lookup_string/2,lookup_atom/2,show_string/1,show_atom/1,
                 a_string/1]).
@end{verbatim}

@em{str_op.c}

@begin{verbatim} 
#include <malloc.h>
#include <stdio.h>

char *get_static_str() @{
  return \"this is a string Ciao Prolog should not free\";
@}

char *get_str(int n) @{
  char *s;
  int size;
  int i;
  int c;
  if (n < 0) n = -n;
  size = (n%4) + 5;
  s = (char *)malloc(size+1);
  for (i = 0, c = ((i + n) % ('z' - 'a' + 1)) + 'a'; i < size; i++,c++) @{
    if (c > 'z') c = 'a'; 
    s[i] = c;
  @}
  s[i] = 0;
  return s;
@}

void put_str(char *s) @{
  if (s) @{
    printf(\"From C: \\\"%s\\\"\\n\", s);
  @} else @{
    printf(\"From C: null\\n\");
  @}
@}
@end{verbatim}").



:- comment(bug,"The idea is nice, and should be extended to other languages").

:- comment(version_maintenance,dir('../../version')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*7+76,2001/03/26,18:08*57+'CEST'), "Improved
documentation a lot.  (MCL)").


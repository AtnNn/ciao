:- use_package([assertions]).
:- comment(nodoc,assertions).

:- comment(title,"Foreign language interface").

:- comment(author,"Jose Morales").
:- comment(author,"Manuel Carro").

:- comment(summary, "This module provides predicates for building
	automatically the shared object necessary for accessing C
	functions as Ciao Prolog predicates. Regular C functions do
	not have their data in the internal format required by Ciao
	Prolog, and thus an intermediate translation is
	necessary. Besides, run-time errors (such as wrong
	instantiation states) must be handled and, finally, C code
	must be compiled into a dynamically loadable object code form,
	which is OS-dependent. All the corresponding details are
	automatically handled by this module, which takes a Ciao
	Prolog module skeleton, defining:

	  @begin{itemize}
          @item A set of C files
          @item A definition of types and modes for the Ciao Prolog
                predicates
          @item An (optional) set of flags required by the compilation of the
                files
          @end{itemize}

        and produces a dynamically loadable object file.
").

:- comment(module, "This module exports two predicates,
   @pred{build_foreign_interface/1} and
   @pred{rebuild_foreign_interface/1}. These predicates know about the
   correspondence between C types and Ciao Prolog types through the 
   assertions documenting them.

   Each predicate implemented as a foreign C function must appear in the
   module as:

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: type1 *
          ... * typeN + (foreign(foreign_function_name), returns(ArgR)).

     :- impl_defined([..., prolog_predicate/N, ...]).
   @end{verbatim}

     where @tt{m1}, ..., @tt{mN} and @tt{type1}, ..., @tt{typeN} are 
     respectively the modes and types of the arguments.

     @pred{prolog_predicate/N} is implemented by @code{foreign_function_name},
     whose result is unified with @tt{ArgR}.

     This notation can be simplified if the name of the foreign function is
     the same as the name of the Ciao Prolog predicate, replacing
     @tt{foreign(foreign_function)} with just @tt{foreign}. @tt{returns(ArgR)}
     specifies that the result of the function corresponds to the @tt{ArgR}
     argument of the Ciao Prolog predicate. If the foreign function does not
     return anything, then @tt{returns(ArgR)} must be removed. Note that
     @tt{returns} cannot be used without @tt{foreign}.

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: type1 *
          ... * typeN + foreign.
   @end{verbatim}

@section{Equivalence between Ciao Prolog and C types}

   The translation between Ciao Prolog and C types is defined only for some
   simple types. The translation to be performed is solely defined by types
   of the arguments in the Ciao Prolog predicate, obeying the table below:

   @begin{itemize}
   @item num <-> double
   @item int <-> int
   @item atm <-> char *
   @item string <-> char * (with trailing zero)
   @item byte_list <-> char * (a buffer, with associated length)
   @item int_list <-> int * (a buffer of integers, with associated length)
   @item address <-> void *
   @end{itemize}

   Strings, atoms, and lists of bytes are passed to (and from) C as
   dynamically (malloc) created arrays of characters (bytes). Those arrays
   are freed by Ciao Prolog upon return of the foreign function unless
   @prop{do_not_free/2} is specified (see examples below).

   Empty lists of bytes are converted into @tt{NULL}, and strings @tt{[]} or
   atoms '' are converted into @em{\"\"} (a zero ended C string).
   @em{NULL} is converted into an empty list or the @em{''} atom.

   @tt{NULL} strings and empty buffers are converted into the empty list or
   the null atom (@tt{''}). Empty Ciao Prolog strings (@tt{""} or @tt{[]})
   and null atoms are converted into zero-terminated strings of zero
   length. Empty Ciao Prolog lists are converted into @tt{NULL}.

   The type @regtype{byte_list/1} requires an additional property,
   @prop{size_of/3}, to indicate which argument represents its size.

@section{Equivalence between Ciao Prolog and C modes}

   (This section has not been written yet)

@section{Examples}

@subsection{Mathematical functions}

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

void obtain_list(int n, int *l, int **s) {
  int i;
  int c;
  if (n < 0) n = 0;
  *l = n;
  *s = (int *)malloc((*l) * sizeof(int));
  for (i = 0; i < *l; i++) {
    (*s)[i] = i;
  }
}

void show_list(int l, int *s) {
  if (s) {
    int n;
    printf("From C:");
    for (n = 0; n < l; n++) {
      printf(" %d", s[n]);
    }
    printf(".\n");
  } else {
    printf("From C: []\n");
  }
}
@end{verbatim}
	
@subsection{Strings and atoms}

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




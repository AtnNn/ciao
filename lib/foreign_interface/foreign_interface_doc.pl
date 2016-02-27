:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title,"Foreign Language Interface").

:- comment(author,"Jose Morales").
:- comment(author,"Manuel Carro").
:- comment(copyright,"The Clip Group, 2001-2002").

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
is OS-dependent.").

:- comment(module, "Ciao Prolog includes a high-level, flexible way to
interface C and Prolog, based on the use of assertions to declare what
are the expected types and modes of the arguments of a Prolog
predicate, and which C files contain the corresponding code.  To this
end, the user provides:

 @begin{itemize}
 @item A set of C files, or a precompiled shared library,
 @item A Ciao Prolog module defining whith predicates are  implemented
  in the C files and the types and modes of their arguments,  and   
 @item an (optional) set of flags required for the compilation of the
 files.
 @end{itemize}

The Ciao Prolog compiler analyzes the Prolog code written by the user
and gathers this information in order to generate automatically C
\"glue\" code implementing the data translation between Prolog and C,
and to compile the C code into dynamically loadable C object files,
which are linked automatically when needed.


@section{Declaration of Types}

Each predicate implemented as a foreign C function must have
accompanying declarations in the Ciao Prolog associated file stating
the types and modes of the C function.  A sample declaration for
@tt{prolog_predicate} which is implemented as
@tt{foreign_function_name} is:


   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: 
                  type1 * ... * typeN + 
                  (foreign(foreign_function_name), returns(ArgR)).

     :- impl_defined([..., prolog_predicate/N, ...]).
   @end{verbatim}

where @tt{m1}, ..., @tt{mN} and @tt{type1}, ..., @tt{typeN} are
respectively the modes and types of the arguments.
@tt{foreign_function_name} is the name of the C function
implementing @pred{prolog_predicate/N}, and the result of this
function is unified with @tt{ArgR}, which must be one of @tt{Arg1}
... @tt{ArgN}.

This notation can be simplified in several ways.  If the name of the
foreign function is the same as the name of the Ciao Prolog predicate,
@tt{foreign(foreign_function_name)} can be replaced by
@tt{foreign/0}. @tt{returns(ArgR)} specifies that the result of the
function corresponds to the @tt{ArgR} argument of the Ciao Prolog
predicate. If the foreign function does not return anything (or if its
value is ignored), then @tt{returns(ArgR)} must be removed. Note that
@tt{returns} cannot be used without @tt{foreign}.  A simplified,
minimal form is thus:

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: 
                  type1 * ... * typeN + foreign.
   @end{verbatim}

@section{Equivalence between Ciao Prolog and C types}

The automatic translation between Ciao Prolog and C types is defined
(at the moment) only for some simple but useful types. The translation
to be performed is solely defined by the types of the arguments in the
Ciao Prolog file (i.e., no inspection of the corresponding C file is
done).  The names (and meaning) of the types known for performing that
translation are to be found in @ref{Foreign Language Interface
Properties}; they are also summarized below (Prolog types are on the
left, and the corresponding C types on the right):

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
dynamically (@tt{malloc}) created arrays of characters
(bytes). Those arrays are freed by Ciao Prolog upon return of the
foreign function unless the property @prop{do_not_free/2} is specified
(see examples below).  This caters for the case in which the C files
save in a private state (either by themselves, or by a library
function being called by them) the values passed on from Prolog.  The
type @regtype{byte_list/1} requires an additional property,
@prop{size_of/2}, to indicate which argument represents its size.


Empty lists of bytes and integers are converted into C @tt{NULL}
pointers, and vice versa.  Empty strings (@tt{[]}) and null atoms
(\'\') are converted into zero-length, zero-ended C strings
(@em{\"\"}).  C @tt{NULL} strings and empty buffers (i.e., buffers
with zero length) are transformed into the empty list or the null atom
(@tt{''}).

Most of the work is performed by the predicates in the @ref{Foreign
Language Interface Builder}, which can be called explicitly by the
user.  Doing that is not usually needed, since the Ciao Prolog
Compiler takes care of building glue code files an of compiling and
linking whatever is necessary.


@section{Equivalence between Ciao Prolog and C modes}

The (prefix) @tt{+/1} ISO mode (or, equivalently, the in/1 mode)
states that the corresponding Prolog argument is ground at the time of
the call, and therefore it is an input argument in the C part; this
groundness is automatically checked upon entry.  The (prefix) @tt{-/1}
ISO mode (or, equivalently, the go/1 mode) states that Prolog expects
the C side to generate a (ground) value for that argument.  Arguments
with output mode should appear in C functions as pointers to the
corresponding base type (as it is usual with C), i.e., an argument
which is an integer generated by the C file, declared as

@begin{verbatim}
:- true pred get_int(go(ThisInt)) :: int + foreign
@end{verbatim}

or as

@begin{verbatim}
:- true pred get_int(-ThisInt) :: int + foreign
@end{verbatim}

should appear in the C code as

@begin{verbatim}
void get_int(int *thisint)
@{
        ....
@}
@end{verbatim}

Note the type of the (single) argument of the function.  Besides, the
return value of a function can always be used as an output argument,
just by specifying to which Prolog arguments it corresponds, using the
@tt{foreing/1} property.  The examples below illustrate this point,
and the use of several assertions to guide the compilation.

@section{Custom access to Prolog from C}

Automatic type conversions does not cover all the possible cases.  
When the automatic type conversion is not enough (or if the user, for
any reason, does not want to go through the automatic conversion), it
is possible to instruct Ciao Prolog not to make implicit type
conversion.  The strategy in that case is to pass the relevant
argument(s) with a special type (a @tt{ciao_term}) which can represent
any term which can be built in Prolog.  Operations to construct,
traverse, and test this data abstraction from C are provided.  The
prototypes of these operations are placed on the
@tt{\"ciao_prolog.h\"} file, under the @tt{include} subdirectory of
the installation directory (the Ciao Prolog compiler knowns where it
has been installed, and gives the C compiler the appropriate flags).
This @em{non direct correspondence} mode is activated whenever a Ciao
Prolog type unknown to the foreign interface (i.e., none of these in
@ref{Foreign Language Interface Properties}) or the type @tt{any_term}
(which is explicitly recognised by the foreign language interface) is
found.  The latter is preferred, as it is much more informative, and
external tools, as the the @concept{CiaoPP} preprocessor, can take
advantage of them.

@subsection{Term construction}

All term construction primitives return an argument of type
@tt{ciao_term}, which is the result of constructing a term.  All Ciao
Prolog terms can be built using the interface operations
@tt{ciao_var()}, @tt{ciao_structure()}, @tt{ciao_integer()}, and
@tt{ciao_float()}.  There are, however, variants and specialized
versions of these operations which can be freely intermixed.  Using
one version or another is a matter of taste and convenience.  We list
below the prototypes of the primitives in order of complexity.

@begin{itemize} 
@item  @tt{ciao_term ciao_var();}

           Returns a fresh, unbound variable.

@item @tt{ciao_term ciao_integer(int i);}

	   Creates a term, representing an integer from the Prolog
point of vieew, from a C integer.

@item @tt{ciao_term ciao_float(double i);}

	   Creates a term, representing a floating point number, from
a floating point number.

@item @tt{ciao_term ciao_atom(char *name);}
	      
	   Creates an atom whose printable name is given as a C string.

@item @tt{ciao_term ciao_structure_a(char *name, int arity, ciao_term *args);}

Creates a structure with name 'name' (i.e., the functor name ), arity
'arity' and the components of the array 'args' as arguments:
@tt{args[0]} will be the first argument, @tt{args[1]} the second,
and so on.  The 'args' array itself is not needed after the term is
created, and can thus be a variable local to a procedure.  An atom can
be represented as a 0-arity structure (with @tt{ciao_structure(name,
0)}), and a list cell can be constructed using the '.'/2 structure
name.  The @tt{_a} suffix stands for @em{array}.


@item @tt{ciao_term ciao_structure(char *name, int arity, ...);}

Similar to ciao_structure_a, but the C arguments after the arity are
used to fill in the arguments of the structure.


@item @tt{ciao_term ciao_list(ciao_term head, ciao_term tail);}

Creates a list from a @tt{head} and a @tt{tail}. It is equivalent
to @tt{ciao_structure(\".\", 2, head, tail)}.


@item @tt{ciao_term ciao_empty_list();}

 Creates an empty list. It is equivalent to @tt{ciao_atom(\"[]\")}.

@item @tt{ciao_term ciao_listn_a(int len, ciao_term *args);}

Creates a list with 'len' elements from the array @tt{args}.  The
@em{nth} element of the list (starting at 1) is @tt{args[n-1]}
(starting at zero).

@item @tt{ciao_term ciao_listn(int length, ...);}

Like @tt{ciao_listn_a()}, but the list elements appear explicitly as
arguments in the call.

@item @tt{ciao_term ciao_dlist_a(int len, ciao_term *args, ciao_term base);}

Like @tt{ciao_listn_a}, but a difference list is
created. @tt{base} whill be used as the tail of the list, instead of
the empty list.

@item @tt{ciao_term ciao_dlist(int length, ...);}

Similar to @tt{ciao_dlist_a()} with a variable number of arguments.
The last one is the tail of the list.

@item @tt{ciao_term ciao_copy_term(ciao_term src_term);}

Returns a new copy of the @tt{term}, with fresh variables (as
@tt{copy_term/2} does).

@end{itemize} 


@subsection{Testing the Type of a Term}

A @tt{ciao_term} can contain @em{any} Prolog term, and its
implementation is opaque to the C code.  Therefore the only way to
know reliably what data is passed on is using explicit functions to
test term types.  Below, @tt{ciao_bool} is a type defined in
@tt{\"ciao_prolog.h\"} which can take the values 1 (for @bf{true}) and
0 (for @bf{false}).


@begin{itemize} 
@item @tt{ciao_bool ciao_is_variable(ciao_term term);}

Returns true if @tt{term} is currently an uninstantiated variable.

@item @tt{ciao_bool ciao_is_integer(ciao_term term);}

Returns true if @tt{term} is instantiated to an integer.

@item @tt{ciao_bool ciao_is_number(ciao_term term);}

Returns true if @tt{term} is an integer or a floating point number.

@item @tt{ciao_bool ciao_is_atom(ciao_term atom);}

Returns true if @tt{term} is an atom. 

@item @tt{ciao_bool ciao_is_list(ciao_term term);}

Returns true if @tt{term} is a list (actually, a @tt{cons} cell).

@item @tt{ciao_bool ciao_is_empty_list(ciao_term term);}

Returns true if @tt{term} is the atom which represents the empty
list (i.e., @tt{[]}).

@item @tt{ciao_bool ciao_is_structure(ciao_term term);}

Returns true if @tt{term} is a structure of any arity.  This
includes atoms (i.e., structures of arity zero) and lists, but
excludes variables and numbers.

@end{itemize} 


@subsection{Term navigation}

The functions below can be used to recover the value of a
@tt{ciao_term} into C variables, or to inspect Prolog structures.


@begin{itemize} 

@item @tt{int ciao_to_integer(ciao_term term); }

Converts @tt{term} to an integer. @tt{ciao_is_integer(term)} must
hold.

@item @tt{double ciao_to_float(ciao_term term);}

Converts @tt{term} to a float value. @tt{ciao_is_number(term)}
must hold.

@item @tt{char *ciao_atom_name(ciao_term atom);}

Returns the name of the atom.  The returned string @em{is the one
internally used by Ciao Prolog}, and should not be changed or altered
in any form. The advantage of using it is that it is fast.

@item @tt{char *ciao_atom_name_dup(ciao_term atom);}

Obtains a @bf{copy} of the name of the atom.  The string can be
modified, and the programmer has the responsibility of deallocating it
after being used.  Due to the copy, it is slower than calling
@tt{char *ciao_atom_name()}.

@item @tt{ciao_term ciao_list_head(ciao_term term);}

Extracts the head of the list @tt{term}. Requires @tt{term} to
be a list.

@item @tt{ciao_term ciao_list_tail(ciao_term term);}

Extracts the tail of the list @tt{term}. Requires @tt{term} to be
a list.

@item @tt{char *ciao_structure_name(ciao_term term);} 

Extracts the name of the structure @tt{term}.  Requires @tt{term}
to be a structure.

@item @tt{int ciao_structure_arity(ciao_term term);}

Extracts the arity of the structure @tt{term}.  

Requires @tt{term} to be a structure.

@item @tt{ciao_term ciao_structure_arg(ciao_term term, int n);}

Extracts the @em{nth} argument of the structure @tt{term}.  It behaves
like @tt{arg/3}, so the first argument has index 1. Requires @tt{term}
to be a structure.

@end{itemize} 


@subsection{Testing for Equality and Performing Unification}

Variables of type @tt{ciao_term} cannot be tested directly for
equality: they are (currently) implemented as a sort of pointers which
may be aliased (two different pointers may refer to the same object).
The interface provides helper functions for testing term equality and
to perform unification of terms.

@begin{itemize}
@item @tt{ciao_bool ciao_unify(ciao_term x, ciao_term y);}

Performs the unification of the terms @tt{x} and @tt{y}, and returns
true if the unification was successful.  This is equivalent to calling
the (infix) Prolog predicate @tt{=/2}.  The bindings are trailed and
undone on backtracking.

@item @tt{ciao_bool ciao_equal(ciao_term x, ciao_term y);}

Performs equality testing of terms, and returns true if the test was
successful.  This is equivalent to calling the (infix) Prolog
predicate @tt{==/2}.  Equality testing does not modify the terms
compared.

@end{itemize} 


@subsection{Raising Exceptions}

The following functions offers a way of throwing @concept{exceptions}
from C that can be caught in Prolog with @tt{catch/3}.  The term that
reaches Prolog is exactly the same which was thrown by C.  The
execution flow is broken at the point where
@tt{ciao_raise_exception()} is executed, and it returns to Prolog.

@begin{itemize} 

@item @tt{void ciao_raise_exception(ciao_term ball);}

 Raises an exception an throws the term @tt{ball}.

@end{itemize} 


@subsection{Calling Prolog from C}

It is also possible to make arbitraty calls to Prolog predicates from
C.  There are two basic ways of make a query, depending if only one
solution is needed (or if the predicate to be called is known to
generate only one solution), or if several solutions are required.

 When only one solution is needed @tt{ciao_commit_call} obtains it
(obviously, the first) and discards the resources used for finding it:

@begin{itemize}

@item @tt{ciao_bool ciao_commit_call(char *name, int arity, ...);}

Makes a call to a predicate and returns true or false depending on
whether the query has succedeed or not.  In case of success, the
(possibly) instantiated variables are reachable from C.

@item @tt{ciao_bool ciao_commit_call_term(ciao_term goal);}

 Like @tt{ciao_commit_call()} but uses the previously built term
@tt{goal} as goal.

@end{itemize}


If more than one solution is needed, it is necessary to use the
@tt{ciao_query} operations.  A consult begins with a
@tt{ciao_query_begin} which returns a @tt{ciao_query} object.
Whenever an additional solution is required, the @tt{ciao_query_next}
function can be called. The query ends by calling @tt{ciao_query_end}
and all pending search branches are pruned.

@begin{itemize} 

@item @tt{ciao_query *ciao_query_begin(char *name, int arity, ...);}

The predicate with the given name, arity and arguments (similar to the
@tt{ciao_structure()} operation) is transformed into a
@tt{ciao_query} object which can be used to make the actual query.

@item @tt{ciao_query *ciao_query_begin_term(ciao_term goal);}

 Like ciao_query_begin but using the term @tt{goal} instead.

@item @tt{ciao_bool ciao_query_ok(ciao_query *query);}

Determines whether the query may have pending solutions.  A false
return value means that there are no more solutions; a true return
value means that there are more possible solutions.


@item @tt{void ciao_query_next(ciao_query *query);}

Ask for a new solution.

@item @tt{void ciao_query_end(ciao_query *query);}

Ends the query and frees the used resources.

@end{itemize} 

@section{Examples}

@subsection{Mathematical functions}

In this example, the standard mathematical library is accessed to
provide the @em{sin}, @em{cos}, and @em{fabs} functions.  Note that the
library is specified simply as

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

@bf{File} @em{math.pl}:

@begin{verbatim} 
:- module(math, [sin/2, cos/2, fabs/2], 
                [assertions, basicmodes, regtypes, foreign_interface]).

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

@bf{File} @em{objects.pl}:

@begin{verbatim} 
:- module(objects, [object/2, show_object/1],
                   [assertions, basicmodes, regtypes, foreign_interface]).
	 
:- true pred object(in(N),go(Object)) :: int * address + 
                                         (foreign,returns(Object)).

:- true pred show_object(in(Object)) :: address + foreign.

:- use_foreign_source(objects_c).
:- extra_compiler_opts('-O2').

:- impl_defined([object/2,show_object/1]).
@end{verbatim}

@bf{File} @em{objects_c.c}:


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

A list of bytes (c.f., a list of ints) corresponds to a byte buffer in
C.  The length of the buffer is associated to that of the list using
the property @tt{size_of/2}.  The returned buffer @bf{is freed by Ciao
Prolog} upon its recepction, unless the @tt{do_not_free/1} property is
specified (see later).  Conversely, a list of natural numbers in the
range 0 to 255 can be passed to C as a buffer.

@bf{File} @em{byte_lists.pl}:

@begin{verbatim} 
:- module(byte_lists, [obtain_list/3, show_list/2],
                      [assertions, basicmodes, regtypes, foreign_interface]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: 
             int * int * byte_list +
             (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: 
             int * byte_list +
             (foreign,size_of(List,Length)).

:- use_foreign_source(bytes_op).

:- impl_defined([obtain_list/3,show_list/2]).
@end{verbatim}

@bf{File} @em{bytes_op.c}:

@begin{verbatim} 
#include <malloc.h>
#include <stdio.h>

void obtain_list(int n, int *l, char **s) @{
  int i, c;
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

@bf{File} @em{int_lists.pl}:

@begin{verbatim} 
:- module(int_lists, [obtain_list/3, show_list/2],
                     [assertions, basicmodes, regtypes, foreign_interface]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: 
             int * int * int_list +
             (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: 
             int * int_list +
             (foreign,size_of(List,Length)).

:- use_foreign_source(ints_op).

:- impl_defined([obtain_list/3,show_list/2]).
@end{verbatim}

@bf{File} @em{ints_op.c}:

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


@bf{File} @em{strings_and_atoms.pl}:

@begin{verbatim} 
:- module(strings_and_atoms,
          [lookup_string/2, lookup_atom/2, a_string/1, 
           show_string/1, show_atom/1],
          [assertions, basicmodes, regtypes, foreign_interface]).


:- true pred a_string(go(S)) ::	string + 
             (foreign(get_static_str),returns(S),do_not_free(S)).
	 
:- true pred lookup_string(in(N),go(S)) ::
             int * string +
             (foreign(get_str),returns(S)).
:- true pred lookup_atom(in(N),go(S)) ::
	     int * atm + 
             (foreign(get_str),returns(S)).

:- true pred show_string(in(S)) :: string + foreign(put_str).
:- true pred show_atom(in(S)) :: atm + foreign(put_str).

:- use_foreign_source(str_op).

:- impl_defined([lookup_string/2,lookup_atom/2,
                 show_string/1,show_atom/1, a_string/1]).
@end{verbatim}

@bf{File} @em{str_op.c}:

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
@end{verbatim}


@subsection{Arbitrary Terms}

This example shows how data Prolog can be passed untouched to C code,
and how it can be manipulated there.


@bf{File} @em{any_term.pl}:

@begin{verbatim} 
:- module(any_term, [custom_display_term/1, custom_create_term/2],
                    [assertions, basicmodes, regtypes, foreign_interface]).

:- true pred custom_display_term(in(X)) :: any_term + foreign.
:- true pred custom_create_term(in(L), go(X)) :: 
             int * any_term + (foreign,returns(X)).

:- use_foreign_source(any_term_c).
:- extra_compiler_opts('-O2').

:- impl_defined([custom_display_term/1,custom_create_term/2]).
@end{verbatim}

@bf{File} @em{any_term.c}:

@begin{verbatim} 
#include <stdio.h>
#include \"ciao_prolog.h\"

ciao_term custom_create_term(int n) @{
  ciao_term t;
  t = ciao_empty_list();
  while (n > 0) @{
    t = ciao_list(ciao_integer(n), t);
    n--;
  @}
  return t;
@}

void custom_display_term(ciao_term term) @{
  if (ciao_is_atom(term)) @{
    printf(\"<atom name=\\\"%s\\\"/>\", ciao_atom_name(term));
  @} else if (ciao_is_structure(term)) @{
    int i;
    int a;
    a = ciao_structure_arity(term);
    printf(\"<structure name=\\\"%s\\\" arity=\\\"%d\\\">\", 
           ciao_structure_name(term), a);
    for (i = 1; i <= a; i++) @{
      printf(\"<argument number=\\\"%d\\\">\", i);
      custom_display_term(ciao_structure_arg(term, i));
      printf(\"</argument>\");
    @}
    printf(\"</structure>\");
  @} else if (ciao_is_list(term)) @{
    printf(\"<list>\");
    printf(\"<head>\");
    custom_display_term(ciao_list_head(term));
    printf(\"</head>\");
    printf(\"<tail>\");
    custom_display_term(ciao_list_tail(term));
    printf(\"</tail>\");
    printf(\"</list>\");
  @} else if (ciao_is_empty_list(term)) @{
    printf(\"<empty_list/>\");
  @} else if (ciao_is_integer(term)) @{
    printf(\"<integer value=\\\"%d\\\"/>\", ciao_to_integer(term));
  @} else if (ciao_is_number(term)) @{
    printf(\"<float value=\\\"%f\\\"/>\", ciao_to_float(term));
  @} else @{
    printf(\"<unknown/>\");
  @}
@}
@end{verbatim}

@subsection{Exceptions}

The following example defines a predicate in C that converts a list of
codes into a number using @tt{strtol()}. If this conversion fails, then
a exception is raised.

	
@bf{File} @em{exceptions_example.pl}:
	
@begin{verbatim} 
:- module(exceptions_example,
          [codes_to_number_c/2, safe_codes_to_number/2],
          [assertions, basicmodes, regtypes, foreign_interface]).

:- use_module(library(format)).

% If the string is not a number, an exception is raised.
:- true pred codes_to_number_c(in(X), go(Y)) :: 
             string * int + (foreign, returns(Y)).

safe_codes_to_number(X, Y) :-
        catch(codes_to_number_c(X, Y), Error, handle_exception(Error)).

handle_exception(Error) :- format(\"Exception caught ~w~n\", [Error]).

:- use_foreign_source(exceptions_c).
:- extra_compiler_opts('-O2').

:- impl_defined([codes_to_number_c/2]).
@end{verbatim}

@bf{File} @em{exceptions_c.c}:

@begin{verbatim} 
#include <string.h>
#include \"ciao_prolog.h\"

int codes_to_number_c(char *s) @{
  char *endptr;
  int n;
  n = strtol(s, &endptr, 10);
  if (endptr == NULL || *endptr != '\\0') @{
    ciao_raise_exception(ciao_structure(\"codes_to_number_exception\", 1, 
                         ciao_atom(s)));
  @}
  return n;
@}
@end{verbatim}
").


 %% :- comment(doinclude,"ciao_term ciao_var();").
 %% :- comment("ciao_term ciao_var();", "Returns a fresh, unbound variable.").

 %% @begin{itemize} 
 %% @item  @tt{ciao_term ciao_var();}
 %% 
 %%            
 %% 
 %% @item @tt{ciao_term ciao_integer(int i);}
 %% 
 %% 	   Creates a term, representing an integer from the Prolog
 %% point of vieew, from a C integer.
 %% 
 %% @item @tt{ciao_term ciao_float(double i);}
 %% 
 %% 	   Creates a term, representing a floating point number, from
 %% a floating point number.
 %% 
 %% @item @tt{ciao_term ciao_atom(char *name);}
 %% 	      
 %% 	   Creates an atom whose printable name is given as a C string.
 %% 
 %% @item @tt{ciao_term ciao_structure_a(char *name, int arity, ciao_term *args);}


:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*7+206,2002/04/22,21:00*27+'CEST'), "Documentation
   thouroughly revised; many parts rewritten.  (MCL)").

:- comment(version(1*7+201,2002/04/18,19:40*51+'CEST'), "Added
   documentation for the improved foreign interface (support of custom
   terms, bug fixes) (Jose Morales)").

:- comment(version(1*7+76,2001/03/26,18:08*57+'CEST'), "Improved
   documentation a lot.  (MCL)").


:- module(indexer_doc,[ ],[ assertions, regtypes ]).

:- use_module(library(assertions(native_props)), [nonground/1]).

:- doc(title, "Multiple Argument Indexing").

:- doc(author, "Anil Nair (original work)").
:- doc(author, "Tom Howland"||
                   " (@uref{http://home.pacbell.net/tomjdnh/pd.html},"||
                   " derived the original work)").
:- doc(author, "Francisco Bueno (port to Ciao)").

:- doc(module,"This package is an extension of the idea of Prolog
   indexing, usually performed, in a limited way, on the first
   argument.  This package provides more powerful indexing schemes.
   It lets you pick different arguments to index on, and provides for
   different combinations of arguments to index on.  E.g., it will let
   you index on the first and third argument or the second and the
   third argument of a predicate.

   The indexing is based on computing a hash value for the terms to be
   indexed upon.
   Note, however, that the current implementation of the package is done
   at the source level, so it may sometimes not be as fast as expected. 
   Given this, this version of the package pays off only when the amount of
   clashing that your original predicate causes without the package
   superseeds the cost of the hashing function in the package. Such
   amount of course depends on the number and the form of the facts in
   your predicate.
").

/*
"library(indexer)" generates index tables of the form Nth_arg-1st_arg
to index on the Nth argument. This means that you could get redundant
solutions. This package adds an extra unique "clause number" argument
to each fact to get around this.

"library(indexer)" can be used only for dynamic facts. This directory
contains files for static rules (indexer.pl), dynamic facts
(dynamic_indexer.pl), and module-partitioned dynamic facts
(module_indexer.pl).
*/

:- doc(usage,"This facility is used as a package, thus either
   including @lib{indexer} in the package list of the module, or by
   using the @decl{use_package/1} declaration. The facility predicate
   @pred{hash_term/2}, documented here, is defined in library module
   @lib{indexer(hash)}.").

:- doc(doinclude,index/1).
:- decl index(IndexSpecs) => indexspecs
# "Declares an indexing scheme for a predicate. All specs of @var{IndexSpecs}
   must be terms for the same predicate. Each spec declares an indexing on
   a combination of the arguments. Indexing will be performed using any of
   the specs in @var{IndexSpecs} (being thus interpreted as an or).

   You should use a @tt{*} in an argument position if you wish to hash on 
   the entire term in that argument. If a @tt{+} is used only one level of 
   the term in the argument is used for hashing. An @tt{i} is used to
   indicate that argument is already an integer, and therefore its own value
   will be used for hashing. The argspec @tt{?} simply indicates not to use
   the argument for indexing.

   For example, the index specification:
@begin{verbatim}
:- index foo(+,?,*,i), foo(?,?,?,i).
@end{verbatim}
   declares indexing for @tt{foo/4} either on a combination of the first,
   third, and fourht arguments, or only on the last argument, which is an
   integer. In the first case, only the principal functor of the first 
   argument will be used for hashing; the third argument will be used in
   its entirety.

   The argspec @tt{n} is a pragmatic extension and can not be used in 
   conjunction with the other specifiers aside from @tt{?}. It stands for 
   ""nonvar"" and implies that the argument will not be used for hashing,
   since only ground terms can effectively be used in hashing. Thus, it
   can not be used in combination with other specifiers within a particular 
   index specification. It is often the fastest thing to use. 
".

:- regtype indexspecs(IndexSpecs)
   # "@var{IndexSpecs} is an index specification.".
:- doc(indexspecs/1,
  "An index specification is defined as follows:
   @includedef{indexspecs/1} @includedef{indexspec/1}").
:- doc(doinclude,indexspecs/1).

indexspecs(Spec) :- indexspec(Spec).
indexspecs((Spec,Specs)) :- indexspec(Spec), indexspecs(Specs).

indexspec(Spec) :- Spec=..[_F|Args], list(Args,argspec).

:- regtype argspec(Spec)
   # "@var{Spec} is an argument hash specification.".
:- doc(argspec/1,
  "An argument hash specification is defined as follows:
   @includedef{argspec/1}").
:- doc(doinclude,argspec/1).

argspec(+).
argspec(*).
argspec(i).
argspec(n).
argspec(?).

:- pred hash_term(T,N) : ground * var => int(N)
	# "@var{N} is a hashing index for @var{T}.".
:- pred hash_term(T,N) : nonground * var => var(N).
:- doc(hash_term(Term,HashValue),
  "Provides an efficient way to calculate an integer @var{HashValue} for a
   ground @var{Term}.").
:- doc(doinclude,hash_term/2).
% hash_term/2 documented, but not implemented here: --EMM
:- reexport(library(indexer(hash)), [hash_term/2]).

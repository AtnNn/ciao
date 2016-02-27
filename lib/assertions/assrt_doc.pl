
:- use_package([assertions,regtypes]).

:- comment(filetype,package).

:- use_module(library(assrt_props)).

:- comment(title,"Program assertions").

:- comment(subtitle,"@em{The CIAO assertion package}").
:- comment(subtitle,"@bf{The CIAO System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP 4/97.1").
:- comment(subtitle,"@em{Draft printed on:} @today{}").

:- comment(author, "The CLIP Group").
:- comment(author, "@tt{clip@@dia.fi.upm.es}").
:- comment(author, "@tt{http://www.clip.dia.fi.upm.es/}").
:- comment(author, "Facultad de Inform@'{a}tica").
:- comment(author, "Universidad Polit@'{e}cnica de Madrid").

:- comment(copyright,"
Copyright @copyright{} 1989-97 The CLIP Group / UPM

@include{Copyright.Manuals}
").

:- comment(bug,"The assertion design and the documentation are still
   in some flux.").

:- comment(bug,"This documentation is still in a preliminary stage.").

:- comment(summary,"This library provides modules which allow
   including @concept{program assertions} in user programs. Such
   assertions can be used to describe predicates, properties, modules,
   applications, etc. These descriptions can be formal specifications
   (such as preconditions and post-conditions) or machine-readable
   textual comments. The information contained in the assertions will
   be used as input by other tools for static or dynamic debugging and
   for automatic documentation generation.").

:- comment(usage,"The recommended procedure in order to make use of
   assertions in user programs is to include the @lib{assertions}
   syntax library, using one of the following declarations, as
   appropriate:

@begin{verbatim}
   :- module(...,...,[assertions]).
   :- include(library(assertions)).
   :- use_package([assertions]).
@end{verbatim}
   ").

:- comment(module,"The @lib{assertions} package adds a number of new 
   declaration definitions and new operator definitions which allow including
   @concept{program assertions} in user programs. Such assertions can
   be used to describe predicates, properties, modules, applications,
   etc. These descriptions can be formal specifications (such as
   preconditions and post-conditions) or machine-readable textual
   comments. 

   This module is part of the @lib{assertions} library. It defines the
   basic code-related assertions, i.e., those intended to be used
   mainly by compilation-related tools, such as the static analyzer or
   the run-time test generator.

   Giving @concept{specifications} for predicates and other program elements
   is the main functionality documented here. 
   The exact syntax of comments @cindex{comments, machine readable}
   is described in the autodocumenter
   (@apl{lpdoc} @cite{knuth-lit,lpdoc-tr}) manual,
   although some support for adding machine-readable comments in assertions
   is also mentioned here.

   There are two kinds of assertions: predicate assertions and program
   point assertions. 
   All predicate assertions are currently placed as directives in the source
   code, i.e., preceded by ``@tt{:-}''.
   Program point assertions are placed as goals in clause bodies.

   @section{More info} 

   The facilities provided by the library are documented in the
   description of its component modules. This documentation is
   intended to provide information only at a ``reference manual''
   level. For a more tutorial introduction to the subject and some
   more examples please see the document ``An Assertion Language for
   Debugging of Constraint Logic Programs (Technical Report
   CLIP2/97.1)''. The assertion language implemented in this library
   is modeled after this design document, although, due to
   implementation issues, it may differ in some details. The purpose
   of this manual is to document precisely what the implementation of
   the library supports at any given point in time.

   @section{Some attention points} 

   @begin{itemize}

   @item @bf{Formatting commands within text strings:} @cindex{formatting
   commands} many of the predicates defined in these modules include
   arguments intended for providing textual information. This includes
   titles, descriptions, comments, etc. The type of this argument is a
   character string. In order for the automatic generation of
   documentation to work correctly, this @concept{character string}
   should adhere to certain conventions. See the description of the
   @pred{docstring/1} type/grammar for details.

   @item @bf{Referring to variables:} In order for the automatic
   documentation system to work correctly, @concept{variable names}
   (for example, when referring to arguments in the head patterns of
   @em{pred} declarations) must be surrounded by an @tt{@@var}
   command. For example, @tt{@@var@{VariableName@}} should be used for
   referring to the variable ``VariableName'', which will appear then
   formatted as follows: @var{VariableName}. See the description of
   the @pred{docstring/1} type/grammar for details.

   @end{itemize}

").


% ----------------------------------------------------------------------------
% Assertion-related declarations
% ----------------------------------------------------------------------------

%pred(_).

:- decl pred(AssertionBody) : assrt_body 
   # "@cindex{pred assertions} This assertion provides information on
     a predicate.  The body of the assertion (@var{AssertionBody}) contains
     properties or comments in the formats defined by
     @pred{assrt_body/1}. 

     More than one of these assertions may appear per predicate, in
     which case each one represents a possible ``@concept{mode}'' of
     use (@concept{usage}) of the predicate. The exact scope of the
     usage is defined by the properties given for calls in the body of
     each assertion (which should thus distinguish the different usages
     intended).
".

:- push_prolog_flag(multi_arity_warnings,off).

%pred(_,_).

:- decl pred(AssertionStatus,AssertionBody) : assrt_status * assrt_body 
   # "@cindex{pred assertions} This assertion is similar to a
      @pred{pred/1} assertion but it is explicitely qualified.
      Non-qualified @pred{pred/1} assertions are assumed the qualifier
      @tt{check}.
".

:- pop_prolog_flag(multi_arity_warnings).

%calls(_).

:- decl calls(AssertionBody) : c_assrt_body
   # "@cindex{calls assertions} This assertion is similar to a
     @pred{pred/1} assertion but it only provides information about
     the calls to a predicate.
".

:- push_prolog_flag(multi_arity_warnings,off).

%calls(_,_).

:- decl calls(AssertionStatus,AssertionBody) : assrt_status * c_assrt_body 
   # "@cindex{calls assertions} This assertion is similar to a
      @pred{calls/1} assertion but it is explicitely qualified.
      Non-qualified @pred{calls/1} assertions are assumed the qualifier
      @tt{check}.
".

:- pop_prolog_flag(multi_arity_warnings).

%success(_).

:- decl success(AssertionBody) : s_assrt_body
   # "@cindex{success assertions} This assertion is similar to a
     @pred{pred/1} assertion but it only provides information about
     the answers to a predicate.
".

:- push_prolog_flag(multi_arity_warnings,off).

%success(_,_).

:- decl success(AssertionStatus,AssertionBody) : assrt_status * s_assrt_body 
   # "@cindex{success assertions} This assertion is similar to a
      @pred{success/1} assertion but it is explicitely qualified.
      Non-qualified @pred{success/1} assertions are assumed the qualifier
      @tt{check}.
".

:- pop_prolog_flag(multi_arity_warnings).

%comp(_).

:- decl comp(AssertionBody) : g_assrt_body
   # "@cindex{comp assertions} This assertion is similar to a
     @pred{pred/1} assertion but it only provides information about
     the global execution properties of a predicate.
".

:- push_prolog_flag(multi_arity_warnings,off).

%comp(_,_).

:- decl comp(AssertionStatus,AssertionBody) : assrt_status * g_assrt_body 
   # "@cindex{comp assertions} This assertion is similar to a
      @pred{comp/1} assertion but it is explicitely qualified.
      Non-qualified @pred{comp/1} assertions are assumed the qualifier
      @tt{check}.
".

:- pop_prolog_flag(multi_arity_warnings).

%prop(_).

:- decl prop(AssertionBody) : assrt_body
   # "@cindex{prop assertions} This assertion is similar to a pred
     assertion but it flags that the predicate being documented is
     also a ``@concept{property}.'' Properties are standard
     predicates, but which are @em{deterministic} and @em{guaranteed
     to terminate for any possible instantiation state of their
     argument(s)} (and can thus be safely used as run-time checks).

     The set of properties is thus a strict subset of the set of
     predicates. Note that properties can be used to describe
     characteristics of arguments in assertions and they can also be
     executed (called) as any other predicates.
".

:- push_prolog_flag(multi_arity_warnings,off).

%prop(_,_).

:- decl prop(AssertionStatus,AssertionBody) : assrt_status * assrt_body 
   # "@cindex{prop assertions} This assertion is similar to a
      @pred{prop/1} assertion but it is explicitely qualified.
      Non-qualified @pred{prop/1} assertions are assumed the qualifier
      @tt{check}.
".

:- pop_prolog_flag(multi_arity_warnings).

%regtype(_).

:- decl regtype(AssertionBody) : assrt_body
   # "@cindex{regtype assertions} This assertion is similar to a pred
     assertion but it flags that the predicate being documented is
     also a ``@concept{regular type}.'' This allows for example
     checking whether it is in the class of types supported by the
     type checking and inference modules. Currently, types are
     properties whose definitions are @em{regular programs}.

     @include{regular_type_syntax}

     The set of types is thus a well defined subset of the set of
     properties. Note that types can be used to describe
     characteristics of arguments in assertions and they can also be
     executed (called) as any other predicates.
".

:- decl type/1
   # "Same as @pred{regtype/1}. Deprecated. Included only for
      backwards compatibility.".

:- push_prolog_flag(multi_arity_warnings,off).

%regtype(_,_).

:- decl regtype(AssertionStatus,AssertionBody) : assrt_status * assrt_body 
   # "@cindex{regtype assertions} This assertion is similar to a
      @pred{regtype/1} assertion but it is explicitely qualified.
      Non-qualified @pred{regtype/1} assertions are assumed the qualifier
      @tt{check}.
".

:- pop_prolog_flag(multi_arity_warnings).

:- decl type/2
   # "Same as @pred{regtype/2}. Deprecated. Included only for
      backwards compatibility.".

%entry(_).

:- decl entry(AssertionBody) : c_assrt_body
   # "@cindex{entry assertions} This assertion provides information
     about the calls to a predicate.  It is identical syntactically to
     a @pred{calls/1} assertion. However, the properties stated are
     not taken as something to be checked but are instead @em{trusted}
     by the compiler. As a result, if these assertions are erroneous
     they can introduce bugs in programs. Thus, @pred{entry/1}
     assertions should be written with care.

     An important use of these assertions is in @concept{providing
     information to the compiler} which it may not be able to infer
     from the program. The main use is in providing information on the
     ways in which exported predicates of a module will be called from
     outside the module. This will greatly improve the precision of
     the analyzer, which otherwise has to assume that the arguments
     that exported predicates receive are any arbitrary term.
".

%modedef(_).

:- decl modedef(AssertionBody) : assrt_body 
# "This assertion is used to define modes. A mode defines in a compact
   way a set of call and success properties. Once defined, modes can
   be applied to predicate arguments in assertions. The meaning of
   this application is that the call and success properties defined by
   the mode hold for the argument to which the mode is applied. Thus,
   a mode is conceptually a ``property macro''.

   The syntax of mode definitions is similar to that of pred
   declarations.

   Example: 
 
   The following set of assertions:
 
@begin{verbatim}
:- modedef +A : nonvar(A) # ""@var{A} is bound upon predicate entry."".

:- pred p(+A,B) : integer(A) =>  ground(B).
@end{verbatim}

  is equivalent to:

@begin{verbatim}
:- pred p(A,B) : (nonvar(A),integer(A)) =>  ground(B)
   # ""@var{A} is bound upon predicate entry."".
@end{verbatim}

   ".

:- decl decl(AssertionBody) : assrt_body 
   # "@cindex{decl assertions} This assertion is similar to a
      @pred{pred/1} assertion but it is used for declarations instead 
      than for predicates.".

:- decl decl(AssertionStatus,AssertionBody) : assrt_status * assrt_body 
   # "@cindex{decl assertions} This assertion is similar to a
      @pred{decl/1} assertion but it is explicitely qualified.
      Non-qualified @pred{decl/1} assertions are assumed the qualifier
      @tt{check}.
".

:- decl comment(Pred,Comment) : head_pattern * docstring
   # "@cindex{comment assertions} This assertion gives a text 
      @var{Comment} for a given predicate @var{Pred}.".

% ----------------------------------------------------------------------------
% Assertion-related predicates
% ----------------------------------------------------------------------------
% Should be in rtchecks????

check(_).

:- pred check(PropertyConjunction) : property_conjunction
   # "@cindex{check assertions} This assertion provides information on
     a clause program point (position in the body of a clause). Calls
     to a @pred{check/1} assertion can appear in the body of a clause
     in any place where a literal can normally appear. The property
     defined by @var{PropertyConjunction} should hold in all the
     run-time stores corresponding to that program point.".

trust(_).

:- pred trust(PropertyConjunction) : property_conjunction
   # "@cindex{trust assertions} This assertion also provides information on
     a clause program point. It is identical syntactically to a @pred{check/1}
     assertion. However, the properties stated are not taken as
     something to be checked but are instead @em{trusted} by the
     compiler. While the compiler may in some cases detect an
     inconsistency between a @pred{trust/1} assertion and the program,
     in all other cases the information given in the assertion will be
     taken to be true.  As a result, if these assertions are erroneous
     they can introduce bugs in programs. Thus, @pred{trust/1}
     assertions should be written with care.

     An important use of these assertions is in @concept{providing
     information to the compiler} which it may not be able to infer
     from the program (either because the information is not present or
     because the analyzer being used is not precise enough). In
     particular, providing information on external predicates which
     may not be accessible at the time of compiling the module can
     greatly improve the precision of the analyzer. This can be easily
     done with trust assertion. ".

true(_).

:- pred true(PropertyConjunction) : property_conjunction
   # "@cindex{true assertions} This assertion is identical
     syntactically to a @pred{check/1} assertion. However, the
     properties stated have been proved to hold by the analyzer. Thus,
     these assertions often represent the @concept{analyzer output}.".

false(_).

:- pred false(PropertyConjunction) : property_conjunction
   # "@cindex{false assertions} This assertion is identical
     syntactically to a @pred{check/1} assertion. However, the
     properties stated have been proved not to hold by the
     analyzer. Thus, these assertions often represent the
     @concept{analyzer output}.".

%% --------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

%% --------------------------------------------------------------------------



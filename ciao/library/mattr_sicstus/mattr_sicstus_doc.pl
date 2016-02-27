:- module( _ , [ put_atts/2
	       , get_atts/2
	       , access_attr/1 
	       , verify_attributes/3
	       ] , [ assertions, regtypes ] ).

:- doc( title , "Sicstus Attributed Variables" ).

:- doc( author , "David Trallero Mena" ).  

:- doc( summary , "This module provides compatibility with local
Sicstus Prolog multi-attributed variables." ).

:- doc( module , "In Sicstus Prolog only two predicates are the
interface to manipulate an attributed variable: @pred{put_atts/2} and
@pred{get_atts/2}. The @pred{detach_attr/2} predicate is aimed via
@pred{put_atts/2} using - as an attribute access specification (see
@pred{access_attr/1} type).

This package requires, as in Sicstus Prolog, the declaration

:- attribute at1,at2/2.

that defines the local attributes of the module.  The attributes can
have predefined arity (at2/2) or changing arity (at1). Whenever
possible the arity is checked in compile time.



The hooks @tt{attribute_goal(Var, Goal)} and @tt{attribute_goal(Var,
Goal)} are not supported." ).



:- pred verify_attributes( Var , Value , Goals )
	: (var(Var), term(Value), list(Goals, callable))
	=> var(Var)

# "This hook is called when an attributed variable @var{Variable} is
about to be bound to @var{Value}. @var{Value} is a non-variable term,
or another attributed variable. @var{Goals} are expected to be a list
of goals to be called _after_ the unification of @var{Variable} with
@var{Value} success.

If this hook fails the unification would fail too. _After_ the hook
success, these actions are taken in order:

@begin{enumerate}

@item Variable is unified to Value. Note here that @var{Variable} is
expected to be free, but if @var{Variable} is bound to a term that
unifies with @var{Value} nothing bad will happen.

@item The list of goals Goals is called.
@end{enumerate}

The hook @pred{verify_attributes/3} is supposed to see compatibility
between the attributes of @var{Var} and @var{Value} if it is a term,
or use @pred{get_atts/2} to merge the attributes of @var{Var} with
the ones of @var{Value}. Otherwise it should just fail.".


verify_attributes( _Var , _Value , _Goals ).



:- pred put_atts( Var , AccessAttr )
	: (var(Var), access_attr(AccessAttr))

# "Sets the attributes of @var{Var} according to @var{AccessAttr}.
The effects of @pred{put_atts/2} are undone on backtracking. The
prefixes in the @var{AccessAttr} have the following meaning:

@begin{itemize}

@item +(Attribute): The corresponding actual attribute is set to
           Attribute.  If the actual attribute was already present, it
           is simply replaced.

@item -(Attribute): The corresponding actual attribute is removed.  If
           the actual attribute was already absent, nothing happens.

@end{itemize}".

put_atts( _Var , _AccessAttr ).


:- pred get_atts( Var , AccessAttr )
	: (var(Var), access_attr( AccessAttr ))

# "Returns the attribute of variable @var{Var} according to
  @var{AccessAttr}. If @var{AccessAttr} is a variable a list with all
  attributes of @var{Var} is returned. The prefixes in the
  @var{AccessAttr} have the following meaning:

@begin{itemize}
  @item +(Attribute) The corresponding actual attribute must be
          present and is unified with ATTRIBUTE.

  @item -(Attribute) The corresponding actual attribute must be
          absent.  The arguments of Attribute are ignored, only the
          name and arity are relevant.
@end{itemize}".


get_atts( _ , _ ).



:- regtype access_attr( Access ) # "Attribute access
type. @var{Access} is a term representing the attribute (it can
contain variable) which could have the prefixes - (minus) or +
(plus). These prefixes have different meaning depending on whether
they are used in @pred{put_atts/2} or @pred{get_atts/2}.".

access_attr( +X ) :- term(X).
access_attr( -X ) :- term(X).
access_attr(  X ) :- term(X).
